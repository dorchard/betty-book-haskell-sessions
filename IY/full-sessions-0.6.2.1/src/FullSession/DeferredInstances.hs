{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module FullSession.DeferredInstances where

import FullSession.Base

instance (SList ss l, Sub l (S n), PickupR ss (SubT l (S n)) s) 
    => Pickup ss n s where
  pickup ss n = pickupR ss (sub (len_ ss) (S n))
instance (SList ss l, Sub l (S n), UpdateR ss (SubT l (S n)) t ss') 
 => Update ss n t ss' where
  update ss n t = updateR ss (sub (len_ ss) (S n)) t
