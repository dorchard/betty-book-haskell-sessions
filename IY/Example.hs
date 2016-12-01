{-# LANGUAGE FlexibleContexts, RebindableSyntax, NoMonomorphismRestriction,
             AllowAmbiguousTypes #-}

import Control.Concurrent.FullSession
import Prelude hiding (return,(>>=),fail,(>>))

return = ireturn
(>>=) = (>>>=)
(>>) = (>>>)
fail = fail

----------------------------------
-- Approach of Keigo Imai, Shoji Yuen, Kiyoshi Agusa:
  --  "Session Type Inference in Haskell" PLACES'10

-- based on "Haskell session types (almost) no class"
--                    Pucella, Tov, Haskell 2008
----------------------------------------------------

{- Example 1, without recursion -}

server0 c = do
     offer c (do
         x <- recv c
         io $ (putStrLn $ x)
         send c "World"
         close c)
       (close c)
     return ()

client0 c = do
    sel1 c
    send c "Hello"
    y <- recv c
    io $ putStrLn $ y
    close c

go0 = runS $ do
    c <- new
    forkIOs (server0 c)
    client0 c

{- Example 1, with recursion -}

serverEq c = do
    unwind0 c
    offer c
        (do
          x <- recv c
          y <- recv c
          send c (x == y)
          recur1 serverEq c)
        (close c)


clientEq c = do
    unwind0 c
    sel1 c
    send c (42 ::Int)
    send c 53
    x <- recv c
    io $ putStrLn $ "Got: " ++ show x
    sel2 c
    close c

{-exampleEq = runS $ do
    c <- new
    forkIOs (serverEq c)
    (clientEq c)

-}


{- Example 2 -}

serverEq2 c = do
    unwind0 c
    offer c
        (do
          x <- recv c
          y <- recv c
          d <- recvS c
          send d (x == y)
          close d
          recur1 serverEq2 c)
        (close c)

clientEq2 c = do
    unwind0 c
    sel1 c
    send c (42 ::Int)
    send c 53
    d <- new
    sendS c d
    x <- recv d
    close d
    io $ putStrLn $ "Got: " ++ show ""
    sel2 c
    close c

{-
example2 = runS $ do
   c <- new
   forkIOs (serverEq2 c)
   clientEq2 c
-}