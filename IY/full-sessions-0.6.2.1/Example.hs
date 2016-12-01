{-# LANGUAGE FlexibleContexts, RebindableSyntax, NoMonomorphismRestriction #-}

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

server c = do x <- recv c
              io $ (putStrLn $ x)
              return ()

client c = send c "Hello"

go = do c <- new
        forkIOs (server c)
        client c
