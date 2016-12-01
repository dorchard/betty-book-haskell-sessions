{-# LANGUAGE TypeOperators, KindSignatures, ScopedTypeVariables, EmptyDataDecls #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module FullSession.Base where

-- ** Type level numbers

-- | Type level zero.
data Z = Z deriving Show
-- | Type level successor. @'S' n@ denotes @(n+1)@.
data S n = S n deriving Show
-- | Type level predecessor (only for internal use). @'P' n@ denotes @(n-1)@.
data P n = P n

-- | The class which covers type-level natural numbers.
class Nat n where
  nat :: n
instance Nat Z where
  nat = Z
instance Nat n => Nat (S n) where
  nat = S nat

-- | Type level @True@.
data T = T
-- | Type level @False@.
data F = F

-- | type-level `&&'
class And b1 b2 b | b1 b2 -> b


-- | Equality on type-level natural numbers.  @b ~ 'T'@ if @x == y@. Otherwise @b ~ F@.
class EqNat x y b | x y -> b
instance EqNat Z Z T
instance EqNat (S n) Z F
instance EqNat Z (S n) F
instance EqNat n n' b => EqNat (S n) (S n') b

-- | Computes subtraction of @n@ by @n'@ (FIXME:英語OK?)
type Sub n n' = Reify (SubT n n')

sub :: forall n n' . Sub n n' => n -> n' -> SubT n n'
sub _ _ = reify (Proxy :: Proxy (SubT n n'))

sub_ :: forall n n' . Sub n n' => Either n n' -> SubT n n'
sub_ _ = reify (Proxy :: Proxy (SubT n n'))

data Proxy n = Proxy
class Reify n where
    reify :: Proxy n -> n
instance Reify Z where
    reify Proxy = Z
instance Reify n => Reify (S n) where
    reify Proxy = S $ reify (Proxy :: Proxy n)


{-
instance Sub n n where { sub _ _ = Z; sub_ _ = Z }
instance Sub (S n) n where { sub _ _ = S Z; sub_ _ = S Z }
instance Sub (S (S n)) n where { sub _ _ = S (S Z); sub_ _ = S (S Z) }
instance Sub (S (S (S n))) n where { sub _ _ = S (S (S Z)); sub_ _ = S (S (S Z)) }
instance Sub (S (S (S (S n)))) n where { sub _ _ = S (S (S (S Z))); sub_ _ = S (S (S (S Z))) }
instance Sub (S (S (S (S (S n))))) n where { sub _ _ = S (S (S (S (S Z)))); sub_ _ = S (S (S (S (S Z)))) }
instance Sub (S (S (S (S (S (S n)))))) n where { sub _ _ = S (S (S (S (S (S Z))))); sub_ _ = S (S (S (S (S (S Z))))) }
instance Sub (S (S (S (S (S (S (S n))))))) n where { sub _ _ = S (S (S (S (S (S (S Z)))))); sub_ _ = S (S (S (S (S (S (S Z)))))) }
instance Sub (S (S (S (S (S (S (S (S n)))))))) n where { sub _ _ = S (S (S (S (S (S (S (S Z))))))); sub_ _ = S (S (S (S (S (S (S (S Z))))))) }
instance Sub (S (S (S (S (S (S (S (S (S n))))))))) n where { sub _ _ = S (S (S (S (S (S (S (S (S Z)))))))); sub_ _ = S (S (S (S (S (S (S (S (S Z)))))))) }
instance Sub n (S n) where { sub _ _ = P Z; sub_ _ = P Z }
instance Sub n (S (S n)) where { sub _ _ = P (P Z); sub_ _ = P (P Z) }
instance Sub n (S (S (S n))) where { sub _ _ = P (P (P Z)); sub_ _ = P (P (P Z)) }
instance Sub n (S (S (S (S n)))) where { sub _ _ = P (P (P (P Z))); sub_ _ = P (P (P (P Z))) }
instance Sub n (S (S (S (S (S n))))) where { sub _ _ = P (P (P (P (P Z)))); sub_ _ = P (P (P (P (P Z)))) }
instance Sub n (S (S (S (S (S (S n)))))) where { sub _ _ = P (P (P (P (P (P Z))))); sub_ _ = P (P (P (P (P (P Z))))) }
instance Sub n (S (S (S (S (S (S (S n))))))) where { sub _ _ = P (P (P (P (P (P (P Z)))))); sub_ _ = P (P (P (P (P (P (P Z)))))) }
instance Sub n (S (S (S (S (S (S (S (S n)))))))) where { sub _ _ = P (P (P (P (P (P (P (P Z))))))); sub_ _ = P (P (P (P (P (P (P (P Z))))))) }
instance Sub n (S (S (S (S (S (S (S (S (S n))))))))) where { sub _ _ = P (P (P (P (P (P (P (P (P Z)))))))); sub_ _ = P (P (P (P (P (P (P (P (P Z)))))))) }
-}

-- | Computes subtraction of @n@ by @n'@
type family SubT n n' :: * where
            SubT n         n = Z
            SubT n         Z = n
            SubT Z     (S m) = P (SubT Z m)
            SubT (S n) (S m) = SubT n m

{-
            SubT n (S (S (S (S (S (S (S (S (S n))))))))) = P (P (P (P (P (P (P (P (P Z))))))))
            SubT n (S (S (S (S (S (S (S (S n)))))))) = P (P (P (P (P (P (P (P Z)))))))
            SubT n (S (S (S (S (S (S (S n))))))) = P (P (P (P (P (P (P Z))))))
            SubT n (S (S (S (S (S (S n)))))) = P (P (P (P (P (P Z)))))
            SubT n (S (S (S (S (S n))))) = P (P (P (P (P Z))))
            SubT n (S (S (S (S n)))) = P (P (P (P Z)))
            SubT n (S (S (S n))) = P (P (P Z))
            SubT n (S (S n)) = P (P Z)
            SubT n (S n) = P Z

            SubT n n = Z
            SubT (S n) n = S Z
            SubT (S (S n)) n = S (S Z)
            SubT (S (S (S n))) n = S (S (S Z))
            SubT (S (S (S (S n)))) n = S (S (S (S Z)))
            SubT (S (S (S (S (S n))))) n = S (S (S (S (S Z))))
            SubT (S (S (S (S (S (S n)))))) n = S (S (S (S (S (S Z)))))
            SubT (S (S (S (S (S (S (S n))))))) n = S (S (S (S (S (S (S Z))))))
            SubT (S (S (S (S (S (S (S (S n)))))))) n = S (S (S (S (S (S (S (S Z)))))))
            SubT (S (S (S (S (S (S (S (S (S n))))))))) n = S (S (S (S (S (S (S (S (S Z))))))))
-}








-- | Type-level snoc (reversed version of cons @(:)@). @ss :> s@ denotes a list @ss@ with @s@ on its end.  (FIXME:English)
data ss :> s = ss :> s
-- | Type-level empty list (@[]@). 
data Nil = Nil


-- ** Operations on type level lists


-- | The class which covers session-type environments. The second parameter of the class denotes the length of the list.
class SList ss l | ss -> l where
  len_ :: ss -> l
instance SList ss l => SList (ss :> s) (S l) where
  len_ ~(ss :> s) = S (len_ ss)
instance SList Nil Z where
  len_ _ = Z

-- | @'Pickup' ss n s@ denotes that the @n@-th element of the list @ss@ is @s@. 
--   This type class plays an important role in session-type inference.
-- 
--   Formally, @'Pickup' ss n s@ if @s = pickup ss n@ where @pickup@ is:
-- 
--   @
--   pickup ss n = pickupR ss (len ss - (n+1))
--     where pickupR (ss:>s) Z     = s
--           pickupR (ss:>s) (S n) = pickupR ss n
--           len Nil     = 0
--           len (ss:>s) = (len ss) + 1
--   @
-- 
--   For example, @Pickup (End :> Bot :> Send Int End) Z t)@ is an instance of @Pickup@, and @t@ is unified with @Bot@.
-- 
--   Note that the list counts from left to right. 
--   For example, The @0@-th element of the list @((Nil :> End) :> Bot) :> Send Int End@ is @End@.
-- 
--   Usually the list is accessed from the right end. 
--   The context 
-- 
--   @
--     'SList' ss (S n), 'Pickup' (ss:>Bot:>Recv Char End) n s
--   @
--
--   is expanded into
--
--   @
--     'SList' ss (S n), 'PickupR' (ss:>Bot:>Recv Char End) ('SubT' (S n) (S n)) s, 'Sub' (S n) (S n)
--   @
--   
--   since @'SubT' ('S' n) ('S' n) ~ Z@,  it will be reduced to 
--  
--   @
--     'PickupR' (ss:>Bot:>Recv Char End) Z s
--   @ 
--   
--   and then @s@ is unified with @Recv Char End@.
class -- (SList ss l, Sub l (S n), PickupR ss (SubT l (S n)) s) =>
       Pickup ss n s | ss n -> s where
  pickup :: ss -> n -> s

-- | The reversed version of 'Pickup' which accesses lists in reversed order (counts from right to left).
--   I.e., @'PickupR' (End :> Bot :> Send Int End) Z (Send Int End)@ is an instance of 'PickupR'.
class PickupR ss n s | ss n -> s where
  pickupR :: ss -> n -> s
instance ss ~ (ss':>t) => PickupR ss Z t where
  pickupR (_ :> t) _ = t
instance (PickupR ss' n t, ss ~ (ss':>s')) => PickupR ss (S n) t where
  pickupR (ss' :> _) (S n) = pickupR ss' n

-- | @'Update' ss n t ss'@ denotes that @ss'@ is same as @ss@ except that its @n@-th element is @t@.
--   Formally, @'Update' ss n t ss'@ if @ss' = update ss n t@ where @update@ is:
--
-- @
--   update ss n t = updateR ss (len ss - (n+1)) t
--     where updateR (ss:>_) Z     t = ss :> t
--           updateR (ss:>s) (S n) t = updateR ss n t :> s
--           len Nil     = 0
--           len (ss:>s) = (len ss) + 1
-- @
-- 
--   In other words, @'Update' (End :> Bot :> Send Int End) Z End (End :> Bot :> End))@ is an instance of @Update@.
-- 
--   Note that the list counts from left to right, as in the case of @Pickup@. 
--            
class -- (SList ss l, Sub l (S n), UpdateR ss (SubT l (S n)) t ss') =>
    Update ss n t ss' | ss n t -> ss' where
  update :: ss -> n -> t -> ss'

-- | The reversed version of 'Update'.
class UpdateR ss n t ss' | ss n t -> ss' where
  updateR ::  ss -> n -> t -> ss'
instance UpdateR (ss:>s) Z t (ss:>t) where
  updateR (ss:>_) _ t = ss :> t
instance UpdateR ss n t ss' => UpdateR (ss:>s) (S n) t (ss':>s) where
  updateR (ss:>s) (S n) t = updateR ss n t :> s



