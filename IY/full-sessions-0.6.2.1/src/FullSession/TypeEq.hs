{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverlappingInstances #-}

module FullSession.TypeEq where

import FullSession.Base

class TypeEq' () x y b => TypeEq x y b | x y -> b where
  type'eq :: x -> y -> b
  type'eq _ _ = undefined::b
class TypeEq' q x y b | q x y -> b
class TypeEq'' q x y b | q x y -> b
instance TypeEq' () x y b => TypeEq x y b
instance b ~ T => TypeEq' () x x b -- redundunt
instance TypeEq'' q x y b => TypeEq' q x y b
instance EqNat x y b => TypeEq'' () x y b
