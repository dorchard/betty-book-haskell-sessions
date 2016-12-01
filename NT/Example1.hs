{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances, InstanceSigs, UndecidableInstances,
             ScopedTypeVariables, Rank2Types, FlexibleContexts,
             StandaloneDeriving #-}

import System.IO
import Network
import Data.Char

class Command command where
  parseCommand :: ReadS command
  unparseCommand :: command -> ShowS

data Session st a = Session { run :: st -> String -> Handle -> IO a }

class SEND st message nextst | st message -> nextst where
    send :: message -> Session nextst () -> Session st ()

class RECEIVE st cont | st -> cont where
   receive' :: cont -> st -> String -> Handle -> (Maybe (IO ()))

   receive :: (RECEIVE st cont) => cont -> Session st ()
   receive g =
      Session (\ st inp h -> case receive' g st inp h of
                                  Just action -> action
                                  Nothing -> fail "unparsable")

io :: IO a -> Session st a
io action = Session (\_ _ _ -> action)

-- DAO: The definition of 'close' has one two many wild-card patterns on the paper (p.5)
close :: Session NULL () -> Session a ()
close cont = Session (\_ _ -> run cont NULL [])

data NULL = NULL                 -- the closed session
data EPS = EPS                   -- the empty session
data SEND_MSG m r = SEND_MSG m r -- send message m, then session r
data RECV_MSG m r = RECV_MSG m r -- receive message m, then session r
data ALT l r = ALT l r           -- alternative session: either l or r
data REC f = REC (f (REC f))

instance Command m => SEND (SEND_MSG m b) m b where
    send :: m -> Session b () -> Session (SEND_MSG m b) ()
    send mess cont =
        -- DAO: We must add a separator character, otherwise successive messages
        -- of the same typo might not get parsed correctly, e.g. sending two
        -- integers gets coalesced to one previously
        Session (\ st inp h -> do hPutStr h (unparseCommand mess [chr 0])
                                  run cont (send_next st) inp h)

instance Command m => RECEIVE (RECV_MSG m x) (m -> Session x ()) where
    receive' :: (m -> Session x ()) -> (RECV_MSG m x) -> String -> Handle -> Maybe (IO ())
    receive' g st inp h =
        case parseCommand inp of
            -- DAO: strip out the additional 0 separator
             ((e, zero:inp'):_) -> Just (run (g e) (recv_next st) inp' h)
             [] -> Nothing

    -- receive :: (m -> Session x ()) -> Session (RECV_MSG m x) ()

-- DAO: runSession in the original paper is undefined and appears to be
-- a re-ordered version of run above (which is called unSession in the original paper)


send_next (SEND_MSG m s) = s
recv_next (RECV_MSG m s) = s

instance (RECEIVE spec1 m1, RECEIVE spec2 m2)
      => RECEIVE (ALT spec1 spec2) (ALT m1 m2) where
    receive' :: (ALT m1 m2) -> (ALT spec1 spec2) -> String -> Handle -> Maybe (IO ())
    receive' (ALT g1 g2) (ALT spec1 spec2) inp h =
      case receive' g1 spec1 inp h of
       Just action -> Just action
       Nothing -> receive' g2 spec2 inp h


left  :: Session l x -> Session (ALT l r) x
right :: Session r x -> Session (ALT l r) x

left  (Session g) = Session (\(ALT l r) inp -> g l inp)
right (Session g) = Session (\(ALT l r) inp -> g r inp)

class RECBODY t c | t -> c where recbody :: t -> c

instance (RECEIVE t c, RECBODY (f (REC f)) t) => RECEIVE (REC f) c where
  receive' :: c -> (REC f) -> String -> Handle -> Maybe (IO ())
  receive' g = \(REC fRECf) inp h -> Just $
                          run (receive g) (recbody fRECf) inp h

instance (SEND t x y, RECBODY (f (REC f)) t) => SEND (REC f) x y where
  send :: x -> Session y () -> Session (REC f) ()
  send mess cont = Session (\ (REC fRECf) inp ->
                              run (send mess cont) (recbody fRECf) inp)

{- EXAMPLE 1 without recursion -}

instance Command Int where
   parseCommand = readsPrec 0
   unparseCommand = showsPrec 0

instance Command Bool where
   parseCommand = readsPrec 0
   unparseCommand = showsPrec 0

intWitness  = 0
boolWitness = False

simpleSpec (send :: (forall x y . x -> y -> s x y))
           (recv :: (forall x y . x -> y -> r x y)) =
  recv intWitness (recv intWitness (send boolWitness EPS))

port = 4251

simpleServer = do
    socket <- listenOn (PortNumber port)
    (h, _ ,_) <- accept socket
    str <- hGetContents h
    let recvNum1  = receive (\x -> recvNum2 x)
        recvNum2 (x :: Int) = receive (\y -> sendEq x y)
        sendEq x y = send (x == y) (finish x y)
        finish x y = close (io (putStrLn $ "Server: " ++ show x ++ " and " ++ show y))
    run recvNum1 (simpleSpec SEND_MSG RECV_MSG) str h
    sClose socket

simpleClient = do
   h <- connectTo "127.0.0.1" (PortNumber port)
   str <- hGetContents h
   let sendNum1 = send (42 :: Int) sendNum2
       sendNum2 = send (53 :: Int) recvEq
       recvEq   = receive (\b -> finish b)
       finish b = close (io (putStrLn $ "Client: " ++ show b))
   run sendNum1 (simpleSpec RECV_MSG SEND_MSG) str h

data Label = Eq | Nil

deriving instance Show Label
deriving instance Read Label

instance Command Label where
   parseCommand = readsPrec 0
   unparseCommand = showsPrec 0

{- EXAMPLE 1 with recursion -}

exampleSpec (send :: (forall x y . x -> y -> s x y))
            (recv :: (forall x y . x -> y -> r x y)) = a0
  where a0 = REC (MkExm (ALT
              (recv Eq (recv intWitness (recv intWitness (send boolWitness a0))))
              (recv Nil EPS)))

data Exm send recv a =
 MkExm (ALT (recv Label (recv Int (recv Int (send Bool a))))
            (recv Label EPS))

instance RECBODY (Exm send recv a)
                 (ALT (recv Label (recv Int (recv Int (send Bool a))))
                      (recv Label EPS)) where
    recbody (MkExm x) = x



exampleServer = do
    socket <- listenOn (PortNumber port)
    (h, _ ,_) <- accept socket

    let session    = receive (ALT (\Eq -> recvNum1) (\Nil -> finish))
        recvNum1   = receive (\x -> recvNum2 x)
        recvNum2 x = receive (\y -> sendEq x y)
        sendEq x y = send (x == y) session
        finish     = close (io $ putStrLn "Fin.")
        
    str <- hGetContents h
    run session (exampleSpec SEND_MSG RECV_MSG) str h
    sClose socket
