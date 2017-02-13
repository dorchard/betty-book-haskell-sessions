{-# LANGUAGE ConstraintKinds, FlexibleContexts,
             TypeFamilies, UndecidableInstances, EmptyDataDecls, ScopedTypeVariables #-}

module SessionA where

import Data.Typeable
import Control.Concurrent.Chan
import Control.Concurrent

{-

Session types in Haskell - attempt A

Using polymorphism, type classes, and a little type families,
we can encode a kind of weak session type theory where a
session type:

(p t . S) is really ((p t)* . S)*

-}

data Send a t = Send a (Chan t)
data Recv a t = Recv a (Chan t)
data End

send :: Chan (Send a t) -> a -> IO (Chan t)
send c x = do
    c' <- newChan
    writeChan c (Send x c')
    return c'

recv :: Chan (Recv a t) -> IO (a, Chan t)
recv c = do
    (Recv a c') <- readChan c
    return (a, c')

close :: Chan End -> IO ()
close c = return ()

fork :: Link s => (Chan s -> IO ()) -> IO (Chan (Dual s))
fork f = do
    c <- newChan
    c' <- newChan
    forkIO $ link (c, c')
    forkIO (f c)
    return c'


type family
    Dual s where
    Dual (Send a t) = Recv a (Dual t)
    Dual (Recv a t) = Send a (Dual t)
    Dual End        = End


class Link s where
    link :: (Chan s, Chan (Dual s)) -> IO ()

instance Link t => Link (Send a t) where
    link (c0, c1) = do
        (Send x c0') <- readChan c0
        c1' <- newChan
        forkIO $ link (c0', c1')
        writeChan c1 (Recv x c1')

instance (Link t) => Link (Recv a t) where
    link (c0, c1) = do
        (Send x c1') <- readChan c1
        c0' <- newChan
        forkIO $ link (c0', c1')
        writeChan c0 (Recv x c0')

instance Link End where
    link (c0, c1) = return ()

---- Example 0 --------------------------------
-- Untyped interaction

client c = do
    writeChan c (Just "Hello")
    writeChan c (Just "World")
    writeChan c Nothing

print_server c = do
    x <- readChan c
    case x of
        Nothing -> return ()
        Just str -> do putStrLn str
                       print_server c

example = do
    c <- newChan
    forkIO (client c)
    print_server c

-- Untyped interaction 2 - unsafe communication

client' c = do
    writeChan c (Just "Hello")
    writeChan c (Just "World")
    writeChan c Nothing
    x <- readChan c
    putStrLn $ "Client got " ++ show x
    return ()

example' = do
    c <- newChan
    forkIO (client' c)
    print_server c

-- Untyped interaction 3 - unsafe communication

client'' c = do
    writeChan c (Just "Hello")
    writeChan c (Just "World")
    writeChan c Nothing
    writeChan c (Just "Hello again")

example'' = do
    c <- newChan
    forkIO (client'' c)
    print_server c

---- Example eq server --------------------------------
-- Untyped interaction

clientEq c d = do
    writeChan c (Just 42)
    writeChan c (Just 53)
    r <- readChan d
    putStrLn $ "Result: " ++ show r
    writeChan c Nothing

serverEq c d = do
    x <- readChan c
    case x of
        Nothing -> return ()
        Just x' -> do
            (Just y') <- readChan c
            writeChan d (x' == y')
            serverEq c d

exampleEq = do
    c <- newChan
    d <- newChan
    forkIO (clientEq'' c d)
    serverEq c d

-- Communication unsafe client
clientEq' c d = do
    writeChan c (Just 42)
    writeChan c (Just 53)
    r <- readChan d
    putStrLn $ "Result: " ++ show r
    writeChan c Nothing
    writeChan c Nothing

-- Communication unsafe client
clientEq'' c d = do
    writeChan c (Just 42)
    readChan c
    r <- readChan d
    putStrLn $ "Result: " ++ show r
    writeChan c Nothing



---- Example 1 --------------------------------

client1 s0 = do
    s <- send s0 True
    s <- send s (42 :: Int)
    close s

server1 s = do
    (b, s) <- recv s
    (x, s) <- recv s
    close s
    putStrLn $ if b then show x else "None"

example1 = do
    c' <- fork client1
    server1 c'
    return ()

---- Example 2  equality server ----------------

serverEqT c = do
    (x, c) <- recv c
    (y, c) <- recv c
    (d, c) <- recv c
    d <- send d (x == y)
    close c
    close d

clientEqT c = do
    c <- send c 42
    c <- send c 53
    d <- fork (\d' -> do
        c <- send c d'
        close c)
    (r, d) <- recv d
    putStrLn ("Result: " ++ show r)
    close d

exampleEqT = do
    c' <- fork clientEqT
    serverEqT c'

