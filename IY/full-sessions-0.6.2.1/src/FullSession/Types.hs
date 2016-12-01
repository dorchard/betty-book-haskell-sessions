{-# LANGUAGE TypeOperators, KindSignatures, ScopedTypeVariables, EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}

module FullSession.Types where

import FullSession.Base





-- | @`Send` v u@ denotes a protocol to emit a value of type @v@ followed by a behavior of type @u@. 
-- Use of @`send`@ on a channel changes its session type from @`Send` v u@ into @u@. 
data Send v u = Send (v -> IO ()) u
-- | @`Recv` v u@ denotes a protocol of receiving a value of type @v@ followed by a behavior of type @u@.
-- Use of @`recv`@ on a channel changes its session type from @`Recv` v u@ into @u@. 
data Recv v u = Recv (IO v) u
-- | @`Throw` u1 u2@ denotes a behavior to output of a channel with session type @u1@ followed by a behavior of type @u2@. 
-- Use of @`sendS`@ on a channel changes its session type from @`Throw` u1 u2@ into @u2@. 
data Throw u' u = Throw (u' -> IO ()) u
-- | @`Catch` u1 u2@ is the input of a channel with session type @u1@ followed by a behavior of type @u2@. 
-- Use of @`recvS`@ on a channel changes its session type from @`Catch` u1 u2@ into @u2@. 
data Catch u' u = Catch (IO u') u
-- | @`Select` u1 u2@ denotes to be either behavior of type @u1@ or type @u2@ after emitting a corresponding label @1@ or @2@.
-- Use of @`sel1`@ or @`sel2`@ on a channel changes its session type from @`Select` u1 u2@ into @u1@ or @u2@, respectively. 
data Select u1 u2 = Select (Bool -> IO ()) u1 u2
-- | @`Offer` u1 u2@ denotes a behavior like either @u1@ or @u2@ according to the incoming label.
data Offer u1 u2 = Offer (IO Bool) u1 u2
-- | `Bot` is the type for a channel whose both endpoints are already engaged by two processes, so that no further processes can own that channel. 
--   For example,  in @forkIO (send k e) >>> recv k@, @k@ has type `Bot`. 
data Bot = Bot
-- | `End` denotes a terminated session. Further communication along a channel with type `End` cannot take place.
data End = End
-- | @`Rec` m r@ denotes recursive session, where @m@ represents the binder of recursion variable. 
-- a type-level natural numer (like @`S` `Z`@). nesting level of @`Rec`@, and
-- @r@ is the body of the recursion which may contain @`Var` m@. 
data Rec m r = Rec m r deriving Show
-- | Recursion variable.
data Var n = Var n deriving Show
-- | @`Close`@ denotes a session that can do nothing but closing it.
data Close = Close (IO ())
data SelectN u1 u2 = SelectN u1 u2
data OfferN u1 u2 = OfferN (IO (Maybe Bool)) u1 u2


-- | The channel type. The type-level number @n@ points to the session-type in type environments. For example, in the type
--  @Session t (Nil:>Send Int End) (Nil:>End) ()@, 
-- the usage of the channel @c :: Channel t Z@ is @Send Int End@ in pretype and @End@ in posttype.
newtype Channel t n = C n


class Diff xx yy zz | xx yy -> zz where
  diff :: Either xx yy -> zz
instance (SList xx lx, SList yy ly, Diff' (SubT lx ly) xx yy zz, Sub lx ly) => Diff xx yy zz where
  diff e  = (\sub_ diff' -> 
    case e of 
      Left xx -> fst (diff' (sub_ (Left (len_ xx)))) xx
      Right yy -> snd (diff' (sub_ (Right (len_ yy)))) yy
    ) sub_ diff'

class Diff' n xx yy zz | n xx yy -> zz where
  diff' :: n -> (xx -> zz, yy -> zz)
instance (xx ~ zz, yy ~ zz) => Diff' Z xx yy zz where
  diff' _ = (id, id)
instance (Diff' n xx' yy zz', xx ~ (xx' :> End), zz ~ (zz' :> End)) => Diff' (S n) xx yy zz where
  diff' (S n) = (\f -> (\(xx' :> End) -> fst f xx' :> End, \yy -> snd f yy :> End)) (diff' n)
instance (Diff' n xx yy' zz', yy ~ (yy' :> End), zz ~ (zz' :> End)) => Diff' (P n) xx yy zz where
  diff' (P n) = (\f -> (\xx -> fst f xx :> End, \(yy' :> End) -> snd f yy' :> End)) (diff' n)


