{-# LANGUAGE TypeOperators, KindSignatures, ScopedTypeVariables, EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE IncoherentInstances #-}

module FullSession.Ended where

import FullSession.Base
import FullSession.Types

-- | @'Ended' n ss@ denotes that the session-type environment @ss@ (the length of it is @n@) is Ended. The all elements in an Ended type environments are @'End'@. 
class (SList ss n, IsEnded ss T) => Ended n ss | n -> ss where
  ended :: n -> ss
instance Ended Z Nil where
  ended _ = Nil
instance Ended n ss => Ended (S n) (ss :> End) where
  ended ~(S n) = ended n :> End

-- | @'IsEnded' ss b@ denotes that b ~ T if @ss@ is Ended, otherwise @b ~ F@.  In other words, @b ~ T@ if the all elements of ss are End
class IsEnded ss b | ss -> b
instance IsEnded Nil T
instance (IsEnded ss b) => IsEnded (ss:>End) b

class IsEndedST s b | s -> b
instance IsEndedST End T
instance IsEndedST (Send x y) F
instance IsEndedST (Recv x y) F
instance IsEndedST (Throw x y) F
instance IsEndedST (Catch x y) F
instance IsEndedST (Select x y) F
instance IsEndedST (Offer x y) F
instance IsEndedST (SelectN x y) F
instance IsEndedST (OfferN x y) F
instance IsEndedST (Bot) F
instance IsEndedST (Close) F
instance IsEndedST (Rec n r) F
instance IsEndedST (Var v) F


class EndedWithout n s ss --  | n s -> ss
instance (SList ss l, EndedWithout' (SubT l (S n)) s l ss) => EndedWithout n s ss

class EndedWithout' n s l ss | n s l -> ss
instance (ss ~ (ss':>s), l ~ S l', Ended l'  ss', IsEnded ss' T) => EndedWithout' Z s l ss
instance (ss ~ (ss':>End), l ~ S l', EndedWithout' n s l' ss') => EndedWithout' (S n) s l ss

class EndedWithout2 n m s t ss  -- | n s m t -> ss
instance (SList ss l, EndedWithout2' (SubT l (S n)) (SubT l (S m)) s t l ss) => EndedWithout2 n m s t ss
class EndedWithout2' n m s t l ss | n m s t l -> ss
instance (ss ~ (ss':>s), l ~ S l', EndedWithout' m t l' ss') => EndedWithout2' Z (S m) s t l ss
instance (ss ~ (ss':>t), l ~ S l', EndedWithout' n s l' ss') => EndedWithout2' (S n) Z s t l ss
instance (ss ~ (ss':>End), l ~ S l', EndedWithout2' n m s t l' ss') => EndedWithout2' (S n) (S m) s t l ss


class AppendEnd ss ss' where
  appendEnd :: ss -> ss'
  deleteEnd :: ss' -> ss
instance (SList ss l, SList ss' l', Sub l' l, AppendEnd' (SubT l' l) ss ss') => AppendEnd ss ss' where
  appendEnd ss  = let d = sub (len_ ss') (len_ ss); ss' = appendEnd' d ss in ss'
  deleteEnd ss' = let d = sub (len_ ss') (len_ ss); ss = deleteEnd' d ss' in ss
class AppendEnd' n ss ss' | n ss -> ss', n ss' -> ss where
  appendEnd' :: n -> ss -> ss'
  deleteEnd' :: n -> ss' -> ss
instance ss ~ ss' => AppendEnd' Z ss ss' where
  appendEnd' _ ss = ss
  deleteEnd' _ ss = ss
instance (AppendEnd' n ss ss'', ss' ~ (ss'':>End)) => AppendEnd' (S n) ss ss' where
  appendEnd' (S n) ss = appendEnd' n ss :> End
  deleteEnd' (S n) (ss:>_) = deleteEnd' n ss

