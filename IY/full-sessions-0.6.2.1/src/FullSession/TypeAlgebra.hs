{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module FullSession.TypeAlgebra where

import Control.Concurrent

import FullSession.Base
import FullSession.Types

-- |duality
class Dual n s t | s -> t, t -> s where
  dual :: n -> IO (s, t)
instance Dual n End End where
  dual _ = return (End, End)
instance Dual n Close Close where
  dual _ = return (Close (return ()), Close (return ()))
instance (Dual n u u', t ~ t') => Dual n (Send t u) (Recv t' u') where
  dual n = do m <- newEmptyMVar; (u, u') <- dual n; return (Send (putMVar m) u, Recv (takeMVar m) u')
instance (Dual n u' u, t ~ t') => Dual n (Recv t' u') (Send t u) where
  dual n = do m <- newEmptyMVar; (u, u') <- dual n; return (Recv (takeMVar m) u, Send (putMVar m) u')
instance (Dual n u1 u1', Dual n u2 u2') => Dual n (Select u1 u2) (Offer u1' u2') where
  dual n = do m <- newEmptyMVar; 
              (u1, u1') <- dual n; (u2, u2') <- dual n; 
              return (Select (putMVar m) u1 u2, Offer (takeMVar m) u1' u2')
instance (Dual n u1 u1', Dual n u2 u2') => Dual n (Offer u1 u2) (Select u1' u2') where
  dual n = do m <- newEmptyMVar; 
              (u1, u1') <- dual n; (u2, u2') <- dual n; 
              return (Offer (takeMVar m) u1 u2, Select (putMVar m) u1' u2')
instance (Dual n u u', v ~ v') => Dual n (Throw v u) (Catch v' u') where
  dual n = do m <- newEmptyMVar; (u, u') <- dual n; return (Throw (putMVar m) u, Catch (takeMVar m) u')
instance (Dual n u u', v ~ v') => Dual n (Catch v u) (Throw v' u') where
  dual n = do m <- newEmptyMVar; (u, u') <- dual n; return (Catch (takeMVar m) u, Throw (putMVar m) u')
instance (Dual (S n) r r', n ~ m, n ~ m') => Dual n (Rec m r) (Rec m' r') where
  dual n = do (r, r') <- dual (S n); return (Rec n r, Rec n r')
instance (Nat v, v ~ v') => Dual n (Var v) (Var v') where
  dual _ = return (Var nat, Var nat)


-- |sesion type algebra
class Comp s t u | s u -> t, t u -> s, s t -> u where
  decomp :: u -> IO (s, t)
instance (Dual Z u u', t ~ t') => Comp (Send t u) (Recv t' u') Bot where
  decomp _ = do (s,t) <- dual Z; return (s, t)
instance (Dual Z u u' , t ~ t') => Comp (Recv t u) (Send t' u') Bot where
  decomp _ = do (s,t) <- dual Z; return (s, t)
instance (Dual Z u1 u1', Dual Z u2 u2') => Comp (Select u1 u2) (Offer u1' u2') Bot where
  decomp _ = do (s,t) <- dual Z; return (s, t)
instance (Dual Z u1 u1', Dual Z u2 u2') => Comp (Offer u1 u2) (Select u1' u2') Bot where
  decomp _ = do (s,t) <- dual Z; return (s, t)
-- instance (Dual Z u1 u1', Dual Z u2 u2') => Comp (SelectN u1 u2) (OfferN u1' u2') Bot where
--   decomp _ = do (s,t) <- dual Z; return (s, t)
-- instance (Dual Z u1 u1', Dual Z u2 u2') => Comp (OfferN u1 u2) (SelectN u1' u2') Bot where
--   decomp _ = do (s,t) <- dual Z; return (s, t)
instance (Dual Z u u' , t ~ t') => Comp (Throw t u) (Catch t' u') Bot where
  decomp _ = do (s,t) <- dual Z; return (s, t)
instance (Dual Z u u' , t ~ t') => Comp (Catch t u) (Throw t' u') Bot where
  decomp _ = do (s,t) <- dual Z; return (s, t)
instance (Dual (S Z) r r', m ~ Z, m' ~ Z) => Comp (Rec m r) (Rec m' r') Bot where
  decomp _ = do (s,t) <- dual (S Z); return (Rec Z s, Rec Z t)

---- ここから
instance (v ~ v'', u ~ u'') => Comp (Send v u) End (Send v'' u'') where
  decomp c = return (c, End)
instance (v ~ v'', u ~ u'') => Comp (Recv v u) End (Recv v'' u'') where
  decomp c = return (c, End)
instance (u1 ~ u1'', u ~ u'') => Comp (Throw u1 u) End (Throw u1'' u'') where
  decomp c = return (c, End)
instance (u1 ~ u1'', u ~ u'') => Comp (Catch u1 u) End (Catch u1'' u'') where
  decomp c = return (c, End)
instance (u1 ~ u1'', u2 ~ u2'') => Comp (Select u1 u2) End (Select u1'' u2'') where
  decomp c = return (c, End)
instance (u1 ~ u1'', u2 ~ u2'') => Comp (Offer u1 u2) End (Offer u1'' u2'') where
  decomp c = return (c, End)
instance (u1 ~ u1'', u2 ~ u2'') => Comp (SelectN u1 u2) End (SelectN u1'' u2'') where
  decomp c = return (c, End)
instance (u1 ~ u1'', u2 ~ u2'') => Comp (OfferN u1 u2) End (OfferN u1'' u2'') where
  decomp c = return (c, End)
instance (u ~ u'', m ~ Z, m' ~ Z) => Comp (Rec m u) End (Rec m' u'') where
  decomp c = return (c, End)


instance (v ~ v'', u ~ u'') => Comp End (Send v u) (Send v'' u'') where
  decomp c = return (End, c)
instance (v ~ v'', u ~ u'') => Comp End (Recv v u) (Recv v'' u'') where
  decomp c = return (End, c)
instance (u1 ~ u1'', u ~ u'') => Comp End (Throw u1 u) (Throw u1'' u'') where
  decomp c = return (End, c)
instance (u1 ~ u1'', u ~ u'') => Comp End (Catch u1 u) (Catch u1'' u'') where
  decomp c = return (End, c)
instance (u1 ~ u1'', u2 ~ u2'') => Comp End (Select u1 u2) (Select u1'' u2'') where
  decomp c = return (End, c)
instance (u1 ~ u1'', u2 ~ u2'') => Comp End (Offer u1 u2) (Offer u1'' u2'') where
  decomp c = return (End, c)
instance (u1 ~ u1'', u2 ~ u2'') => Comp End (SelectN u1 u2) (SelectN u1'' u2'') where
  decomp c = return (End, c)
instance (u1 ~ u1'', u2 ~ u2'') => Comp End (OfferN u1 u2) (OfferN u1'' u2'') where
  decomp c = return (End, c)
instance (u ~ u'', m ~ Z, m' ~ Z) => Comp End (Rec m u) (Rec m' u'') where
  decomp c = return (End, c)

instance Comp End Close Close where
  decomp c = return (End, c)
instance Comp Close End Close where
  decomp c = return (c,End)

instance Comp End Bot Bot where
  decomp c = return (End, c)
instance Comp Bot End Bot where
  decomp c = return (c, End)

instance Comp End End End where
  decomp c = return (c, c)
---- ここまで
