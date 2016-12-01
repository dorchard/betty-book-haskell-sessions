{-# OPTIONS_GHC -F -pgmF ixdopp #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

module Chatter (chatter, client, server) where

import Control.Concurrent.FullSession
import Prelude hiding (catch)

import System.IO
import Data.Maybe

-- chat protocol
type Chat = Send String (Rec Z (Send NowTyping (SelectN (Send Msg (Var Z)) (Send Canceled (Var Z)))))

-- type of message
data Canceled = Canceled deriving (Read, Show)
data NowTyping = NowTyping deriving (Read, Show)
newtype Msg = Msg String deriving (Read, Show)

instance (Show a, Read a) => Message a where
  showMessage = (++"\n") . show 
  parseMessage = listToMaybe . reads

-- chat port
mkChatService host port = mkNwService2 host port (undefined::Chat) (undefined::NwDual Chat u => u)

chatter name up down = ixdo
    io_ $ prepareConsole
    
    send up name
    peername <- recv down
    
    forkIOs (receiver peername down)
    sender up

  where

    receiver peername down = ixdo
      unwind0 down
      
      NowTyping <- recv down
      io_ (putStrLn ("\n" ++ peername ++" is typing ..."))
      
      offerN down (ixdo
          Msg str <- recv down
          io_ (putStrLn ("\n" ++ peername ++" says: "++str))
        ) (ixdo
          Canceled <- recv down
          io_ (putStrLn ("\n" ++ peername ++ " canceled typing."))
        )
      recur1 (receiver peername) down -- loop
      
    sender ch = ixdo
        
        unwind0 up
        
        c <- io getRegularChar
        send ch NowTyping
        
        res <- io $ loop [c]
        case res of
          Nothing -> ixdo 
            sel2N ch; send ch Canceled
          Just str -> ixdo 
            sel1N ch; send ch (Msg str); 
            io_ $ putStrLn ("sent: "++str)

        recur1 sender ch

      where  
        loop str = do
          c <- getChar
          case c of
            '\DEL' -> do
                putChar '\b'>> hFlush stdout
                if length str <= 1 
                  then return Nothing
                  else loop (init str)
            '\n' -> do
                putStrLn ""
                if str=="" then loop "" else return (Just str)
            c -> do
                putChar c
                hFlush stdout
                loop (str++[c])
            
    getRegularChar = do
      c <- getChar
      if c=='\n' || c=='\DEL' then getRegularChar else putChar c >> hFlush stdout >> return c
      
    prepareConsole = do
      hSetBuffering stdin NoBuffering
      hSetEcho stdin False
      putStrLn "connection established."

client name host port = ixdo
    io $ putStrLn $ "connecting to: "++host++":"++show port
    (up,down) <- connectNw2 (mkChatService host port)
    chatter name up down

server name port = ixdo
    io $ putStrLn $ "waiting a connection at port " ++ show port
    (up,down) <- acceptOneNw2 (mkChatService "*****" port)
    chatter name up down
    
