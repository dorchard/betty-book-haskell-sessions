{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, TypeOperators #-}
{-# LANGUAGE OverlappingInstances, IncoherentInstances #-}

module FullSession.Incoherent where

import FullSession.Base
import FullSession.TypeEq
import FullSession.Types
import FullSession.TypeAlgebra
import FullSession.Ended
import FullSession.Recursion
import FullSession.SMonad
import FullSession.FullSession
import FullSession.NwSession


instance IsEnded (ss:>Send x y) F
instance IsEnded (ss:>Recv x y) F
instance IsEnded (ss:>Throw x y) F
instance IsEnded (ss:>Catch x y) F
instance IsEnded (ss:>Select x y) F
instance IsEnded (ss:>Offer x y) F
instance IsEnded (ss:>SelectN x y) F
instance IsEnded (ss:>OfferN x y) F
instance IsEnded (ss:>Bot) F
instance IsEnded (ss:>Close) F
instance IsEnded (ss:>Rec n r) F
instance IsEnded (ss:>Var v) F
instance (IsEnded ss b1, IsEndedST s b2, And b1 b2 b) => IsEnded (ss:>s) b

instance And T T T
instance And b F F
instance And F b F

