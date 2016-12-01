{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies, AllowAmbiguousTypes #-}

module FullSession.Recursion where

import FullSession.TypeEq
import FullSession.Base
import FullSession.Types

-- folding
class RecFold m u s r | m u -> r  where 
  fold' :: m -> u -> s -> r
instance RecFold m (Var n) s (Var n) where 
  fold' _ v _ = v 
instance (TypeEq m n b, RecFoldCont b m n a s r) => RecFold m (Rec n a) s r where -- fold if m = n
  fold' m (Rec n a) s = fold'cont (type'eq m n) m n a s

class RecFoldCont b m n a s r | b m n a -> r where
   fold'cont :: b -> m -> n -> a -> s -> r
instance a ~ s => RecFoldCont T m n a s (Var m) where -- fold
   fold'cont _ m _ _ _ = Var m
instance (RecFold2 n s s', RecUnfold n s' xx s, RecFold m a s' r)
      => RecFoldCont F m n a s (Rec n r) where -- do not fold
   fold'cont b m n a s = Rec n (fold' m a (fold2' n s))

-- folding
class RecFold2 m u r | m u -> r  where 
  fold2' :: m -> u -> r
instance RecFold2 m (Var n) (Var n) where 
  fold2' _ v = v 
instance (TypeEq m n b, RecFoldCont2 b m n a r) => RecFold2 m (Rec n a) r where -- fold if m = n
  fold2' m (Rec n a) = fold2'cont (type'eq m n) m n a

class RecFoldCont2 b m n a r | b m n a -> r where
   fold2'cont :: b -> m -> n -> a -> r
instance RecFoldCont2 T m n a (Var m) where -- fold
   fold2'cont _ m _ _ = Var m
instance (RecFold2 m a r)
      => RecFoldCont2 F m n a (Rec n r) where -- do not fold
   fold2'cont b m n a = Rec n (fold2' m a)

-- unfolding
class RecUnfold m r s u | m r s -> u where
  unfold' :: m -> r -> s -> u
instance (RecFold2 n s s', RecUnfold m a s' a') => RecUnfold m (Rec n a) s (Rec n a') where
  unfold' m (Rec n a) s = Rec n (unfold' m a (fold2' n s))
instance (TypeEq m n b, RecUnfoldCont b m n s a) => RecUnfold m (Var n) s a where -- unfold if m = n
  unfold' m (Var n) s = unfoldCont (type'eq m n) m n s

class RecUnfoldCont b m n s a | b m n s -> a where
  unfoldCont :: b -> m -> n -> s -> a
instance RecUnfoldCont T m n s (Rec m s) where -- unfold
  unfoldCont _ m _ s = Rec m s
instance RecUnfoldCont F m n s (Var n) where -- do not unfold
  unfoldCont _ _ n _ = Var n

unfold :: (RecFold m u r r, RecUnfold m r r u) => Rec m r -> u -- RecUnfold ensures that our fold/unfold is isomorphic
unfold (Rec m r) = unfold' m r r 

unfold0 :: (RecFold Z u r r, RecUnfold Z r r u) => Rec Z r -> u
unfold0 (Rec m r) = unfold' m r r 

unfold1 :: (RecFold (S Z) u r r, RecUnfold (S Z) r r u) => Rec (S Z) r -> u
unfold1 (Rec m r) = unfold' m r r 

unfold2 :: (RecFold (S (S Z)) u r r, RecUnfold (S (S Z)) r r u) => Rec (S (S Z)) r -> u
unfold2 (Rec m r) = unfold' m r r 


instance (RecUnfold m u s u', v ~ v') => RecUnfold m (Send v u) s (Send v' u') where
  unfold' m (Send c u) s = Send c (unfold' m u s)
instance (RecUnfold m u s u', v ~ v') => RecUnfold m (Recv v u) s (Recv v' u') where
  unfold' m (Recv c u) s = Recv c (unfold' m u s)
instance (RecUnfold m u s u', v ~ v') => RecUnfold m (Throw v u) s (Throw v' u') where
  unfold' m (Throw c u) s = Throw c (unfold' m u s)
instance (RecUnfold m u s u', v ~ v') => RecUnfold m (Catch v u) s (Catch v' u') where
  unfold' m (Catch c u) s = Catch c (unfold' m u s)
instance (RecUnfold m u1 s u1', RecUnfold m u2 s u2') => RecUnfold m (Select u1 u2) s (Select u1' u2') where
  unfold' m (Select c u1 u2) s = Select c (unfold' m u1 s) (unfold' m u2 s)
instance (RecUnfold m u1 s u1', RecUnfold m u2 s u2') => RecUnfold m (Offer u1 u2) s (Offer u1' u2') where
  unfold' m (Offer c u1 u2) s = Offer c (unfold' m u1 s) (unfold' m u2 s)
instance (RecUnfold m u1 s u1', RecUnfold m u2 s u2') => RecUnfold m (SelectN u1 u2) s (SelectN u1' u2') where
  unfold' m (SelectN u1 u2) s = SelectN (unfold' m u1 s) (unfold' m u2 s)
instance (RecUnfold m u1 s u1', RecUnfold m u2 s u2') => RecUnfold m (OfferN u1 u2) s (OfferN u1' u2') where
  unfold' m (OfferN c u1 u2) s = OfferN c (unfold' m u1 s) (unfold' m u2 s)
instance RecUnfold m End s End where
  unfold' m End s = End
instance RecUnfold m Close s Close where
  unfold' m (Close c) s = Close c

instance (RecFold m u s u', v ~ v') => RecFold m (Send v u) s (Send v' u') where
  fold' m (Send c u) s = Send c (fold' m u s)
instance (RecFold m u s u', v ~ v') => RecFold m (Recv v u) s (Recv v' u') where
  fold' m (Recv c u) s = Recv c (fold' m u s)
instance (RecFold m u s u', v ~ v') => RecFold m (Throw v u) s (Throw v' u') where
  fold' m (Throw c u) s = Throw c (fold' m u s)
instance (RecFold m u s u', v ~ v') => RecFold m (Catch v u) s (Catch v' u') where
  fold' m (Catch c u) s = Catch c (fold' m u s)
instance (RecFold m u1 s u1', RecFold m u2 s u2') => RecFold m (Select u1 u2) s (Select u1' u2') where
  fold' m (Select c u1 u2) s = Select c (fold' m u1 s) (fold' m u2 s)
instance (RecFold m u1 s u1', RecFold m u2 s u2') => RecFold m (Offer u1 u2) s (Offer u1' u2') where
  fold' m (Offer c u1 u2) s = Offer c (fold' m u1 s) (fold' m u2 s)
instance (RecFold m u1 s u1', RecFold m u2 s u2') => RecFold m (SelectN u1 u2) s (SelectN u1' u2') where
  fold' m (SelectN u1 u2) s = SelectN (fold' m u1 s) (fold' m u2 s)
instance (RecFold m u1 s u1', RecFold m u2 s u2') => RecFold m (OfferN u1 u2) s (OfferN u1' u2') where
  fold' m (OfferN c u1 u2) s = OfferN c (fold' m u1 s) (fold' m u2 s)
instance RecFold m End s End where
  fold' m End _ = End
instance RecFold m Close s Close where
  fold' m (Close c) _ = Close c

instance (RecFold2 m u u', v ~ v') => RecFold2 m (Send v u) (Send v' u') where
  fold2' m (Send c u) = Send c (fold2' m u)
instance (RecFold2 m u u', v ~ v') => RecFold2 m (Recv v u) (Recv v' u') where
  fold2' m (Recv c u) = Recv c (fold2' m u)
instance (RecFold2 m u u', v ~ v') => RecFold2 m (Throw v u) (Throw v' u') where
  fold2' m (Throw c u) = Throw c (fold2' m u)
instance (RecFold2 m u u', v ~ v') => RecFold2 m (Catch v u) (Catch v' u') where
  fold2' m (Catch c u) = Catch c (fold2' m u)
instance (RecFold2 m u1 u1', RecFold2 m u2 u2') => RecFold2 m (Select u1 u2) (Select u1' u2') where
  fold2' m (Select c u1 u2) = Select c (fold2' m u1) (fold2' m u2)
instance (RecFold2 m u1 u1', RecFold2 m u2 u2') => RecFold2 m (Offer u1 u2) (Offer u1' u2') where
  fold2' m (Offer c u1 u2) = Offer c (fold2' m u1) (fold2' m u2)
instance (RecFold2 m u1 u1', RecFold2 m u2 u2') => RecFold2 m (SelectN u1 u2) (SelectN u1' u2') where
  fold2' m (SelectN u1 u2) = SelectN (fold2' m u1) (fold2' m u2)
instance (RecFold2 m u1 u1', RecFold2 m u2 u2') => RecFold2 m (OfferN u1 u2) (OfferN u1' u2') where
  fold2' m (OfferN c u1 u2) = OfferN c (fold2' m u1) (fold2' m u2)
instance RecFold2 m End End where
  fold2' m End = End
instance RecFold2 m Close Close where
  fold2' m (Close c) = Close c


