{-# OPTIONS_GHC -F -pgmF ixdopp #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

module GuiChatter (chatter, client, server) where

import Control.Concurrent.FullSession hiding (close)

import System.IO
import System.Environment
import Data.Maybe

import Graphics.UI.WXCore hiding (Var)
import Graphics.UI.WX hiding (Var)

import Control.Concurrent
import Control.Exception (unblock)
import qualified System.Timeout as TO
import EnableGUI

-- chat protocol
type Chat = Send String (Rec Z (Send TNowTyping (SelectN (Send TMsg (Var Z)) (Send TCanceled (Var Z)))))

-- type of message
data TCanceled = TCanceled deriving (Read, Show)
data TNowTyping = TNowTyping deriving (Read, Show)
newtype TMsg = TMsg String deriving (Read, Show)

instance (Show a, Read a) => Message a where
  showMessage = (++"\n") . show 
  parseMessage = listToMaybe . reads

takeMVarTimeout mv to = TO.timeout to (takeMVar mv)

-- chat port
mkChatService host port = mkNwService2 host port (undefined::Chat) (undefined::NwDual Chat u => u)

-- the type for communication between gui and networking part
data TypingState = NowTyping | Msg String | Canceled  

chatter sendmv recvmv errch timeout name up down = ixdo
    io_ $ prepareConsole    
    forkIOs (finallys (receiver down) (writeChan errch ()))
    finallys (sender up) (writeChan errch ()) 

  where

    receiver down = ixdo
      unwind0 down
      TNowTyping <- recv down
      io_ $ putMVar recvmv NowTyping
      
      offerN down (ixdo
          TMsg str <- recv down
          io_ $ putMVar recvmv (Msg str)
        ) (ixdo
          TCanceled <- recv down
          io_ $ putMVar recvmv Canceled
        )
      recur1 receiver down -- loop

    sender ch = ixdo
        unwind0 ch
        ts <- io $ takeMVar sendmv
        send ch TNowTyping
        case ts of
          NowTyping -> 
            let loop = ixdo
                 ts' <- io $ takeMVarTimeout sendmv timeout
                 case ts' of
                   Nothing -> ixdo sel2N ch; send ch TCanceled
                   Just NowTyping -> loop
                   Just Canceled -> ixdo sel2N ch; send ch TCanceled
                   Just (Msg str) -> ixdo sel1N ch; send ch (TMsg str)
            in loop
          Msg str -> ixdo sel1N ch; send ch (TMsg str)
        
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

guichatter name up down = ixdo
    upmv <- io newEmptyMVar
    downmv <- io newEmptyMVar
    errch <- io newChan
    namech <- new 
    forkOSs $ flip finallys (writeChan errch ()) $ ixdo
      send up name
      peername <- recv down
      send namech peername
      chatter upmv downmv errch  1000000 name up down
    peername <- recv namech
    io_ $ gui peername name upmv downmv errch

myEvent = newEvent "myEvent" 

receiveEventID = wxID_HIGHEST+1
errEventID = wxID_HIGHEST+2



-- gui :: MVar TypingState -> MVar String -> Chan () -> IO ()
gui peername myname upmv downmv errch
  = do f      <- frame [text := myname]
       p      <- panel f []                       -- panel for tab-management etc.
       input  <- comboBox p [processEnter := True, text := "cmd"]
       output <- textCtrlRich p [bgcolor := black, textColor := white, font := fontFixed{ _fontSize = 12 }]
       focusOn input
       textCtrlSetEditable output False
       set f [layout := container p $
                        margin 10 $ column 5 [fill (widget output)
                                             ,row 5 [hfill (widget input)]]
             ,clientSize  := sz 600 400
             ]
       let message txt = appendText output txt
       set input [on command := talk input output, on keyboard := keypress input]

       let readLoop = 
             do readMVar downmv; ev <- commandEventCreate wxEVT_COMMAND_MENU_SELECTED receiveEventID; 
                evtHandlerAddPendingEvent f ev; readLoop
       tid <- forkOS $ unblock readLoop
       let healthCheckLoop = 
             do readChan errch; ev <- commandEventCreate wxEVT_COMMAND_MENU_SELECTED errEventID; 
                evtHandlerAddPendingEvent f ev; healthCheckLoop
       tid2 <- forkOS $ unblock healthCheckLoop
       evtHandlerOnMenuCommand f receiveEventID (do receive output)
       evtHandlerOnMenuCommand f errEventID (do killAll f tid tid2)
       return ()
  where
    talk input output
      = do
           txt <- get input text
           appendText output ("\n"++myname++": "++txt++"\n")
           set input [text := ""]
           putMVar upmv (Msg txt)
           propagateEvent
    keypress me key
      = do
           propagateEvent
           str <- get me text
           if str /= "" 
             then putMVar upmv NowTyping
             else return ()
    receive output
      = do
           ts <- tryTakeMVar downmv
           case ts of
             Just (Msg txt) -> appendText output ("\n"++peername++": "++txt++"\n")
             Just NowTyping -> appendText output (peername ++ " is typing now...")
             Just Canceled -> appendText output "canceled.\n"
             Nothing -> return ()
    killAll f tid tid2
      = do
           close f
           killThread tid
           killThread tid2


client name host port = start $ do
    putStrLn $ "connecting to: "++host++":"++show port
    runS $ ixdo
      (up,down) <- connectNw2 (mkChatService host port)
      guichatter name up down

server name port = start $ do
    putStrLn $ "waiting a connection at port " ++ show port
    runS $ ixdo
      (up,down) <- acceptOneNw2 (mkChatService "*****" port)
      guichatter name up down

-- eventNewEventType
-- commandEventCreate
