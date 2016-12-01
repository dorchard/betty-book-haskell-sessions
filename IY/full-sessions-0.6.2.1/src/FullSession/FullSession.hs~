{-# LANGUAGE TypeOperators, KindSignatures, ScopedTypeVariables, EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}


module FullSession.FullSession where

import Control.Concurrent

import FullSession.Base
import FullSession.TypeEq
import FullSession.Types
import FullSession.TypeAlgebra
import FullSession.Ended
import FullSession.SMonad
import FullSession.Recursion

close :: (Pickup ss n Close, Update ss n End ss', IsEnded ss F)
       => Channel t n -> Session t ss ss' ()
close (C n) = Session (\ss -> case pickup ss n of Close c -> c >> (return (update ss n End, ())))

send :: (Pickup ss n (Send v a), Update ss n a ss', IsEnded ss F) 
      => Channel t n -> v -> Session t ss ss' ()
send (C n) v = Session (\ss -> case pickup ss n of (Send outp u) -> outp v >> (return (update ss n u, ())))

recv :: (Pickup ss n (Recv v u), Update ss n u ss', IsEnded ss F) 
      => Channel t n -> Session t ss ss' v
recv (C n) = Session (\ss -> case pickup ss n of (Recv inp u) -> inp >>= \x -> (return (update ss n u, x)))

sendS :: (Pickup ss n1 (Throw c1 u1), 
          Update ss n1 u1 ss',
          Pickup ss' n2 c1,
          Update ss' n2 End ss'',
          IsEnded ss F)
       => Channel t n1 -> Channel t n2 -> Session t ss ss'' ()
sendS (C n1) (C n2) = Session (\ss -> 
    case pickup ss n1 of 
      (Throw outp u1) -> 
        let ss' = update ss n1 (u1) in
          outp (pickup ss' n2) >> return (update ss' n2 (End), ()))

recvS :: (Pickup ss n1 (Catch c1 u1), 
          Update ss n1 u1 ss',
          SList ss' l,
          IsEnded ss F)
       => Channel t n1 -> Session t ss (ss':>c1) (Channel t l)
recvS (C n1) = Session (\ss -> 
    case pickup ss n1 of
      (Catch inp u1) -> 
        let ss' = update ss n1 (u1) in
          inp >>= \c -> return (ss' :> c, C (len_ ss')))

-- |output a label `1'
sel1 :: (Pickup ss n (Select s x), Update ss n s tt, IsEnded ss F) => Channel t n -> Session t ss tt () 
sel1 (C n) = Session (\ss -> case pickup ss n of Select c s _ -> c True >> return (update ss n s, ()))

-- |output a label `2'
sel2 :: (Pickup ss n (Select x s), Update ss n s tt, IsEnded ss F) => Channel t n -> Session t ss tt () 
sel2 (C n) = Session (\ss -> case pickup ss n of Select c _ s -> c False >> return (update ss n s, ()))

ifSelect :: (Pickup ss n (Select x y), Update ss n x sx, Update ss n y sy, Diff xx yy zz, IsEnded ss F)
         => Channel t n 
         -> Bool
         -> Session t sx xx a 
         -> Session t sy yy a
         -> Session t ss zz a
ifSelect (C n) b (Session s) (Session t) = Session $ \ss -> case pickup ss n of
  (Select outp x y) -> outp b >> (\diff -> 
    if b then s (update ss n x) >>= \(xx,a) -> return (diff (Left xx), a)
         else t (update ss n y) >>= \(yy,a) -> return (diff (Right yy), a)) diff

offer :: (Pickup ss n (Offer x y), Update ss n x sx, Update ss n y sy, Diff xx yy zz, IsEnded ss F)
      => Channel t n 
      -> Session t sx xx a 
      -> Session t sy yy a
      -> Session t ss zz a
offer (C n) (Session s) (Session t) = Session $ \ss -> case pickup ss n of
  (Offer inp x y) -> inp >>= \b -> (\diff -> 
    if b then s (update ss n x) >>= \(xx,a) -> return (diff (Left xx), a)
         else t (update ss n y) >>= \(yy,a) -> return (diff (Right yy), a)) diff

-- |pointwise extension of `Comp' -- FIXME: method
class Par ss tt' tt | ss tt' -> tt, ss tt -> tt', tt tt' -> ss where
   split :: tt -> IO (ss, tt')
instance Par Nil Nil Nil where
   split _ = return (Nil, Nil)
instance (Comp s t' t, IsEnded ss b, Par' b ss tt' tt) => Par (ss:>s) (tt':>t') (tt:>t) where
   split (uu':>u) = do (ss',tt') <- split' uu'; (s,t) <- decomp u; return (ss':>s,tt':>t) 

-- |the specialized case for `ended' ss -- FIXME
class Par' t ss tt' tt | t ss tt' -> tt, t ss tt-> tt', t tt tt' -> ss where
  split' :: IsEnded ss t => tt -> IO (ss, tt')
instance (SList tt n, Ended n ss, IsEnded ss T, tt' ~ tt) => Par' T ss tt' tt where
  split' x = return (ended (len_ x),x)
instance (IsEnded ss F, Par ss tt' tt) => Par' F ss tt' tt where
  split' u = do (ss,tt) <- split u; return (ss, tt)

-- |start a new thread
forkIOs, forkOSs :: (SList ss l, SList tt' l, SList tt l, Ended l' ss', IsEnded ss b, Par' b ss tt' tt) 
                 => Session t ss ss' () -> Session t tt tt' ThreadId
forkIOs (Session f) = Session (\tt -> do (ss, tt') <- split' tt; tid <- forkIO (f ss >> return ()); return (tt', tid))
forkOSs (Session f) = Session (\tt -> do (ss, tt') <- split' tt; tid <- forkOS (f ss >> return ()); return (tt', tid))

io :: IO a -> Session t ss ss a
io m = Session (\ss -> m >>= \x -> return (ss, x))

io_ :: IO a -> Session t ss ss ()
io_ m = Session (\ss -> m >> return (ss, ()))

new :: SList ss l => Session t ss (ss:>Bot) (Channel t l)
new = Session (\ss -> return (ss:>Bot, C (len_ ss)))

unwind :: (RecFold m u r r, RecUnfold m r r u,
           Pickup ss n (Rec m r),
           Update ss n u tt,
           IsEnded ss F)
        => Channel t n -> Session t ss tt ()
unwind (C n) = Session $ \ss -> return (update ss n (unfold (pickup ss n)), ())

unwind0 :: (RecFold Z u r r, RecUnfold Z r r u,
           Pickup ss n (Rec Z r),
           Update ss n u tt,
           IsEnded ss F)
        => Channel t n -> Session t ss tt ()
unwind0 (C n) = Session $ \ss -> return (update ss n (unfold0 (pickup ss n)), ())

unwind1 :: (RecFold (S Z) u r r, RecUnfold (S Z) r r u,
           Pickup ss n (Rec (S Z) r),
           Update ss n u tt,
           IsEnded ss F)
        => Channel t n -> Session t ss tt ()
unwind1 (C n) = Session $ \ss -> return (update ss n (unfold1 (pickup ss n)), ())

unwind2 :: (RecFold (S (S Z)) u r r, RecUnfold (S (S Z)) r r u,
           Pickup ss n (Rec (S (S Z)) r),
           Update ss n u tt,
           IsEnded ss F)
        => Channel t n -> Session t ss tt ()
unwind2 (C n) = Session $ \ss -> return (update ss n (unfold2 (pickup ss n)), ())

recur1 :: (EndedWithout n s ss, AppendEnd ss ss', SList ss' l, Ended l tt) => 
         (Channel t n -> Session t ss tt ()) -> 
         Channel t n -> Session t ss' tt ()
recur1 f c = case f c of Session m -> Session (\ss' -> m (deleteEnd ss') >> return (ended (len_ ss'), ()))

recur2 :: (EndedWithout2 n m s s' ss, AppendEnd ss ss', SList ss' l, Ended l tt) => 
         (Channel t n -> Channel t m -> Session t ss tt ()) -> 
         Channel t n -> Channel t m -> Session t ss' tt ()
recur2 f c d = case f c d of Session m -> Session (\ss' -> m (deleteEnd ss') >> return (ended (len_ ss'), ()))


newtype Service u = Service (u -> IO (), IO u)

newService :: Dual Z u u' => IO (Service u)
newService = do mv <- newEmptyMVar; return (Service (putMVar mv, takeMVar mv))

connect :: (Dual Z u u', SList ss l) => Service u -> Session t ss (ss:>u') (Channel t l)
connect (Service (put,_)) = Session (\ss -> do (u,u') <- dual Z; put u; return (ss:>u', C (len_ ss)))

connectRunS :: (Dual Z u u', Ended xs n) => Service u -> (forall t. Channel t Z -> Session t (Nil:>u') xs a) -> IO a
connectRunS (Service (put,_)) f = do (u,u') <- dual Z; put u; (_,a) <- session (f (C Z)) (Nil:>u'); return a

accept :: (Dual Z u u', SList ss l) => Service u -> Session t ss (ss:>u) (Channel t l)
accept (Service (_,get)) = Session (\ss -> do u <- get; return (ss:>u, C (len_ ss)))

acceptRunS :: (Dual Z u u', Ended xs n) => Service u -> (forall t. Channel t Z -> Session t (Nil:>u) xs a) -> IO a
acceptRunS (Service (_,get)) f = do u <- get; (_,a) <- session (f (C Z)) (Nil:>u); return a




