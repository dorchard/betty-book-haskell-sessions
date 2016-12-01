{-# LANGUAGE RebindableSyntax, ScopedTypeVariables, Rank2Types,
             TypeOperators, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

import Control.Concurrent.SimpleSession.Positional
import Control.Monad.Indexed
import Prelude hiding (Monad(..))
import Unsafe.Coerce

ifThenElse True x y = x
ifThenElse False x y = y

(>>=)  = flip ibind

return :: IxPointed m => a -> m i i a
return = ireturn
x >> y = x >>= (\_ -> y)
fail   = undefined

-- Example 1

serverA c = do
    x <- recv c
    y <- recv c
    send c (x == y)

server0 c = do { enter c; loop }
 where loop = offer c
         (do x <- recv c
             y <- recv c
             send c (x == y)
             zero c
             loop)
         (close c)

client0 c = do
    enter c
    sel1 c
    send c 42
    send c 53
    x <- recv c
    io $ putStrLn $ "Got: " ++ show x
    zero c
    sel2 c
    close c

example1 = runSession $ do
    rv <- io newRendezvous
    forkSession (request rv client0)
    accept rv server0


{- Example 2 with shared channels -}

server1R r c = do
    enter c
    loop
  where
    loop = offer c
           (do (x :: Int) <- recv c
               y <- recv c
               accept r (\d -> do
                            send d (x == y)
                            close d)
               zero c
               loop)
           (close c)

client1R r c = do
   enter c
   sel1 c
   send c 42
   send c 53
   request r (\d' -> do
         x <- recv d'
         io $ putStrLn $ "Got: " ++ show x
         close d')
   zero c
   sel2 c
   close c

example1R = runSession $ do
   rv <- io newRendezvous
   rv' <- io newRendezvous
   forkSession (request rv (client1R rv'))
   accept rv (server1R rv')

{- Example 2, failed attempt trying to encode delegation using
 an 'new' combinator and Imai et al. existential proposal -}

-- Causes a deadlock!
new :: Dual r r' =>
       (forall t t' . Channel t -> Channel t' -> Session (Cap t () r, (Cap t' () r', x)) y a)
    -> Session x y a
new m = do
    r <- io $ newRendezvous
    request r (\c -> accept r (\c' -> m c' c))

swap3 :: Session (r, (s, (t, x))) (t, (r, (s, x))) ()
swap3 = (dig swap) >> swap

server1 c = do
    enter c
    loop
  where
    loop = offer c
           (do (x :: Int) <- recv c
               y <- recv c
               d <- recv c
               recv_cap c
               swap
               send d (x == y)
               close d
               zero c
               loop)
           (close c)


data EChannel k e d c = ME (forall t' . k (Channel t') (k (Cap t' e d) c))

instance Dual c c' => Dual (EChannel (:?:) e s c) (EChannel (:!:) e s c')
instance Dual c c' => Dual (EChannel (:!:) e s c) (EChannel (:?:) e s c')

wrap :: (Session (Cap t e (Channel t' :!: (Cap t' e' r) :!: c),
                                           (Cap t' e' r, x)) y a)

      -> Session (Cap t e (EChannel (:!:) e' d c), (Cap t' e' d, x)) y a
wrap k = unsafeCoerce k

wrapD :: (Session (Cap t e (Channel t' :?: (Cap t' e' r) :?: c), x) y a)
       -> (Session (Cap t e (EChannel (:?:) e' r c), x) y a)
wrapD k = unsafeCoerce k

client1 c = do
   enter c
   sel1 c
   send c 42
   send c 53
   io $ putStrLn "C: sending done"
   new (\d d' -> do
      swap3         -- (d, (d', c)) (Channel t' :!: Cap t' e' d :!: c, (d, d'))
      io $ putStrLn "did swap"
      wrap    (do io $ putStrLn $ "C: sending chan"
                  send c d   -- (Cap t' e' d :!: c, (d, d')) (c, Cap d' (Bool :?: Eps))
                  io $ putStrLn $ "sent chan"
                  send_cap c
                  swap          -- (c, Cap d' (Bool :?: Eps)) -> (Cap d' (Bool :?: Eps), c)
                  x <- recv d'  -- (Cap d' (Bool :?: Eps), c)
                  io $ putStrLn $ "Got: " ++ show x
                  close d'))     -- (Cap d' Eps, c) c
   zero c
   sel2 c
   close c

server1' c = do
    enter c
    loop
  where
    loop = offer c
           (do (x :: Int) <- recv c
               y <- recv c
               io $ putStrLn "got two integers"
               wrapD (do
                d <- recv c
                io $ putStrLn "got a chan"
                recv_cap c
                swap
                send d (x == y)
                close d)
               zero c
               loop)
           (close c)

example1' = runSession $ do
   rv <- io newRendezvous
   forkSession (request rv client1)
   forkSession (accept rv server1')
