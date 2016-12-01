{-# OPTIONS -fcontext-stack=100 #-}
{-# LANGUAGE TypeOperators, KindSignatures, ScopedTypeVariables, EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}

module FullSession.NwSession where

import qualified Control.Exception as E
import System.IO.Error (mkIOError, userErrorType)
import Prelude hiding (catch)
import System.IO
import Data.IORef
import qualified Network.Socket as N
import Network.BSD
import Control.Monad (liftM)
import Control.Concurrent
import System.IO.Unsafe (unsafePerformIO)
import System.Exit

import FullSession.Base
import FullSession.Types
import FullSession.TypeAlgebra
import FullSession.Ended
import FullSession.SMonad


errorExit :: String -> a
errorExit str = E.throw (mkIOError userErrorType str Nothing Nothing)


finish :: (Ended ss n, IsEnded ss T) => String -> Session t ss tt ()
finish str = Session (\_ -> errorExit str)

connectTo :: String -> Int -> IO (Handle, IORef String)
connectTo host port_ = do
      let port = toEnum port_
      sock <- N.socket N.AF_INET N.Stream 0
      addrs <- liftM hostAddresses $ getHostByName host
      if null addrs then errorExit $ "no such host : " ++ host else return ()
      N.connect sock $ N.SockAddrInet port (head addrs)
      handle <- N.socketToHandle sock ReadWriteMode
      str <- hGetContents handle
      ref <- newIORef str
      return (handle, ref)

listenAt :: Int -> IO (Handle, IORef String)
listenAt port_ = do
      let port = toEnum port_
      lsock <- N.socket N.AF_INET N.Stream 0
      N.bindSocket lsock $ N.SockAddrInet port N.iNADDR_ANY
      N.listen lsock 1 -- 1 本しか接続を受け付けない． 複数の場合は N.sOMAXCONN を使う？
      (sock,N.SockAddrInet _ _) <- N.accept lsock -- 複数の接続を受ける場合acceptを複数回
      N.sClose lsock -- bindされたソケットを閉じる
      handle <- N.socketToHandle sock ReadWriteMode
      str <- hGetContents handle
      ref <- newIORef str
      return (handle, ref)
  

newtype NwService u = NwService (String,Int)

mkNwService :: NwSession u => String -> Int -> u -> NwService u
mkNwService str port _ = NwService (str,port)

connectNw :: (SList ss l, NwSession u) => NwService u -> Session t ss (ss:>u) (Channel t l)
connectNw (NwService (host,port)) = Session $ \ss -> do
      (handle, ref) <- connectTo host port
      let u = genSession ref handle
      return (ss:>u, C (len_ ss))


newtype NwService2 u u' = NwService2 (String,Int)

mkNwService2 :: (NwSendOnly u, NwReceiveOnly u') => String -> Int -> u -> u' -> NwService2 u u'
mkNwService2 str port _ _ = NwService2 (str,port)

connectNw2 :: (SList ss l, NwSendOnly u, NwReceiveOnly u') => NwService2 u u' -> Session t ss (ss:>u:>u') (Channel t l, Channel t (S l))
connectNw2 (NwService2 (host,port)) = Session $ \ss -> do
      (handle, ref) <- connectTo host port
      let u  = genSession ref handle
          u' = genSession ref handle
      return (ss:>u:>u', (C (len_ ss), C (S (len_ ss))))

acceptOneNw2 :: (SList ss l, NwSendOnly u, NwReceiveOnly u') => NwService2 u u' -> Session t ss (ss:>u:>u') (Channel t l, Channel t (S l))
acceptOneNw2 (NwService2 (_,port)) = Session $ \ss -> do
      (handle, ref) <- listenAt port
      let u  = genSession ref handle
          u' = genSession ref handle
      return (ss:>u:>u', (C (len_ ss), C (S (len_ ss))))

dualNw :: NwDual u u' => NwService u -> NwService u'
dualNw (NwService (host,port)) = NwService (host,port)

dualNw2 :: (NwDual u1 u1', NwDual u2 u2') => NwService2 u1 u2 -> NwService2 u1' u2'
dualNw2 (NwService2 (host,port)) = NwService2 (host,port)

class Message mes where
  parseMessage :: String -> Maybe (mes,String)
  showMessage :: mes -> String

class NwSession u => NwSender u
instance (NwSession u, Message v) => NwSender (Send v u)
instance (NwSender u1, NwSender u2) => NwSender (SelectN u1 u2)

class NwSession u => NwReceiver u where
  tryParse :: u -> String -> Bool
instance (NwSession u, Message v) => NwReceiver (Recv v u) where
  tryParse _ str = maybe False (const True) (parseMessage str::Maybe (v,String))
instance (NwReceiver u1, NwReceiver u2) => NwReceiver (OfferN u1 u2) where
  tryParse (OfferN _ u1 u2) str = tryParse u1 str || tryParse u2 str

class NwSession u where
  genSession :: IORef String -> Handle -> u
instance (Message v, NwSession u) => NwSession (Send v u) where
  genSession str h = Send (\v -> do hPutStr h (showMessage v); hFlush h) (genSession str h)
instance (Message v, NwSession u) => NwSession (Recv v u) where
  genSession ref h = 
    Recv (do str <- readIORef ref; 
             case parseMessage str of
               Just (v, rest) -> do writeIORef ref rest; return v; 
               Nothing -> errorExit ("no parse : "++str)
         ) (genSession ref h)
instance NwSession Close where
  genSession _ h = Close (putStrLn "closing connection.." >> hFlush h >> hClose h)
instance (NwSender u1, NwSender u2) => NwSession (SelectN u1 u2) where
  genSession str h = SelectN (genSession str h) (genSession str h)
instance (NwReceiver u1, NwReceiver u2) => NwSession (OfferN u1 u2) where
  genSession ref h = offer (genSession ref h) (genSession ref h)
    where 
      offer l r =
        OfferN 
          (do str <- readIORef ref; 
              if tryParse l str 
                 then return (Just True) 
                 else if tryParse r str 
                        then return (Just False) 
                        else return Nothing)
          l r
instance (NwSession u, Nat m) => NwSession (Rec m u) where -- FIXME recursion variable numbering
  genSession str h = Rec nat (genSession str h)
instance Nat n => NwSession (Var n) where
  genSession str h = Var nat
  
-- |output a label `1'
sel1N :: (Pickup ss n (SelectN s x), Update ss n s tt) => Channel t n -> Session t ss tt () 
sel1N (C n) = Session (\ss ->
  case pickup ss n of SelectN u1 _ -> return (update ss n u1, ()))

sel2N :: (Pickup ss n (SelectN x s), Update ss n s tt) => Channel t n -> Session t ss tt () 
sel2N (C n) = Session (\ss ->
  case pickup ss n of SelectN _ u2 -> return (update ss n u2, ()))

ifSelectN :: (Pickup ss n (SelectN x y), Update ss n x sx, Update ss n y sy, Diff xx yy zz, IsEnded ss F)
         => Channel t n 
         -> Bool
         -> Session t sx xx a 
         -> Session t sy yy a
         -> Session t ss zz a
ifSelectN (C n) b (Session s) (Session t) = Session $ \ss -> case pickup ss n of
  (SelectN x y) -> (\diff -> 
    if b then s (update ss n x) >>= \(xx,a) -> return (diff (Left xx), a)
         else t (update ss n y) >>= \(yy,a) -> return (diff (Right yy), a)) diff


offerN :: (NwReceiver x, NwReceiver y, 
          Pickup ss n (OfferN x y), Update ss n x sx, Update ss n y sy, Diff xx yy zz, IsEnded ss F)
      => Channel t n 
      -> Session t sx xx a 
      -> Session t sy yy a
      -> Session t ss zz a
offerN (C n) (Session s) (Session t) = Session $ \ss -> case pickup ss n of
  (OfferN test x y) -> test >>= \m -> (\diff -> 
    case m of
      Just True  -> s (update ss n x) >>= \(xx,a) -> return (diff (Left xx), a)
      Just False -> t (update ss n y) >>= \(yy,a) -> return (diff (Right yy), a)
      Nothing    -> errorExit "no parse"
    ) diff


class (NwSession s, NwSession t) => NwDual s t | s -> t, t -> s -- FIXME - recursion variable numbering
instance NwDual Close Close
instance (Message t, Message t', NwDual u u', t ~ t') => NwDual (Send t u) (Recv t' u')
instance (Message t, Message t', NwDual u' u, t ~ t') => NwDual (Recv t' u') (Send t u)
instance (NwSender u1, NwReceiver u1', NwSender u2, NwReceiver u2', NwDual u1 u1', NwDual u2 u2') => NwDual (SelectN u1 u2) (OfferN u1' u2')
instance (NwReceiver u1, NwSender u1', NwReceiver u2, NwSender u2', NwDual u1 u1', NwDual u2 u2') => NwDual (OfferN u1 u2) (SelectN u1' u2')
instance (NwDual r r', Nat m, m ~ m') => NwDual (Rec m r) (Rec m' r')
instance (Nat v, v ~ v') => NwDual (Var v) (Var v')

class NwSession u => NwSendOnly u -- FIXME recursion variable numbering
instance (NwSendOnly u, Message v) => NwSendOnly (Send v u)
instance (NwSendOnly u1, NwSendOnly u2, NwSender u1, NwSender u2) => NwSendOnly (SelectN u1 u2)
instance NwSendOnly Close
instance (NwSendOnly u, Nat m) => NwSendOnly (Rec m u)
instance Nat n => NwSendOnly (Var n)

class NwSession u => NwReceiveOnly u -- FIMXE recursion variable numbering
instance (NwReceiveOnly u, Message v) => NwReceiveOnly (Recv v u)
instance (NwReceiveOnly u1, NwReceiveOnly u2, NwReceiver u1, NwReceiver u2) => NwReceiveOnly (OfferN u1 u2)
instance NwReceiveOnly Close
instance (NwReceiveOnly u, Nat m) => NwReceiveOnly (Rec m u)
instance Nat n => NwReceiveOnly (Var n)

finallys :: Session t ss tt () -> IO () -> Session t ss tt ()
finallys (Session f) m = Session (\tt -> E.finally (f tt) m)
