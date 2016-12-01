{-# LANGUAGE TypeOperators, KindSignatures, ScopedTypeVariables, EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}

module FullSession.SMonad where

import FullSession.Base
import FullSession.Types
import FullSession.Ended

-- | The @Session@ monad. @ss@ and @tt@ denotes the /usage/ of channels. 
-- 
--   * @ss@ denotes /pre-type/, which denotes the type-level list of session types /required/ to run the session.
-- 
--   * @tt@ denotes /post-type/, which denotes the type-level lists of session types /produced/ by the session.
-- 
--   @t@ denotes a /type-tag/, which prevents abuse of use of channels. For detail, see 'runS'.
-- 
newtype Session t ss tt a = Session { session :: ss -> IO (tt,a) }

-- | Bind (a.k.a @>>=@) operation for 'Session' monad.
(>>>=) :: Session t ss tt a -> (a -> Session t tt uu b) -> Session t ss uu b
(Session m) >>>= f = Session (\ss -> m ss >>= \(tt,x) -> session (f x) tt)

(>>>) :: Session t ss tt a -> Session t tt uu b -> Session t ss uu b
(Session m) >>> (Session n) = Session (\ss -> m ss >>= \(tt,_) -> n tt)

-- | Unit (a.k.a @return@) operation for 'Session' monad.
ireturn :: a -> Session t ss ss a
ireturn a = Session (\ss -> return (ss, a))

-- | 'runS' runs the 'Session'. The pretype (see 'Session') must be 'Nil'. 
--   The posttype must be 'Ended', i.e. all channels must be 'End'.
--
--   Forall'd type variable @t@ prevents abuse of use of channels inside different run.
--   For example, @new >>>= \c -> 'io_' (runS ( ... send c ...) )@ is rejected by the Haskell typechecker with error @Inferred type is less polymorphic than expected@.
runS :: Ended n ss => forall a n. (forall t. Session t Nil ss a) -> IO a
runS s = case s of Session m -> m Nil >>= \(_,a) -> return a


channeltype1 :: Pickup ss' Z s' => (Channel t Z -> Session t (Nil:>s) ss' a) -> (s, s')
channeltype1 = error "ERROR - channeltype1 is for type checking purpose only!"
channeltype2 :: (Pickup ss' Z s', Pickup ss' (S Z) t') => (Channel t Z -> Channel t (S Z) -> Session t (Nil:>s:>t) ss' a) -> ((s, s'), (t, t'))
channeltype2 = error "ERROR - channeltype2 is for type checking purpose only!"

typecheck1
  :: (SList ss l) =>
     (Channel t l -> Session t (ss:>s1) ss' a)
     -> Session t (ss:>s1) ss' a
typecheck1 = error "ERROR - typecheck2 is for type checking purpose only!"

typecheck2
  :: (SList ss l) =>
     (Channel t l -> Channel t (S l) -> Session t (ss:>s1:>s2) ss' a)
     -> Session t (ss:>s1:>s2) ss' a
typecheck2 = error "ERROR - typecheck2 is for type checking purpose only!"

inspect1 :: SList tt l => (Channel t l -> Session t ss ss () -> Session t (tt:>t1) tt' a) -> (ss, Session t (tt:>t1) tt' a)
inspect1 = undefined
