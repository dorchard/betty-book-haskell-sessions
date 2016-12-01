{-# OPTIONS_GHC -F -pgmF ixdopp #-}

-- a very simple SMTP client.

import Control.Concurrent.FullSession

{- 
$ ghci examples/smtp-client.hs

-- case 1.
*Main> main
host name of SMTP server:
xxx.yyy.zzz.www
port number:
25
from:
sender@example.com
to:
recipient@example.com
msg:
Hello!
complete.
*Main>

-- case 2.
*Main> main
host name of SMTP server:
xxx.yyy.zzz.www
port number:
25
from:
sender@example.com
to:
nobodyxxxx
msg:
Hello!
["550 <nobodyxxxx>: Recipient address rejected: User unknown in local recipient table\r"]
*Main>
-}


-- Types for SMTP commands. 
newtype EHLO = EHLO String; newtype MAIL = MAIL String; newtype RCPT = RCPT String
data    DATA = DATA;        data    QUIT = QUIT;        
newtype MailBody = MailBody [String]

-- Types for SMTP server replies (200 OK, 500 error and 354 start mail input)
newtype R2yz = R2yz [String]; newtype R5yz = R5yz [String]; newtype R354 = R354 String

-- auxiliary functions
send_receive_200 c mes = ixdo send c mes; (R2yz _) <- recv c; ireturn ()
send_receive_354 c mes = ixdo send c mes; (R354 _) <- recv c; ireturn ()

sendMail c d = -- let from=undefined;to=undefined;mail=undefined in 
 ixdo         -- the body of our SMTP client
  (R2yz _) <- recv c                   -- receive 220
  send_receive_200 c (EHLO "mydomain") -- send EHLO, then receive 250
  unwind0 c; sel1N c                   -- (annotation) branch to send 'MAIL FROM'
  from <- recv d                          -- (1) input the sender's address on d
  send_receive_200 c (MAIL from)       -- send 'MAIL FROM', then receive 250
  unwind1 c; sel1N c                   -- (annotation) branch to send 'RCPT TO'
  to <- recv d                            -- (2) input the recipient's address on d
  send c (RCPT to)                     -- send 'RCPT TO'
  -- 
  offerN c (ixdo                       -- branch the session according to the reply
    (R2yz _) <- recv c                 -- if 250 OK is offered
    sel1 d; mail <- recv d                -- (3) input the content of the mail on d 
    unwind1 c; sel2N c                 -- (annotation) branch to send 'DATA'
    send_receive_354 c DATA            -- send 'DATA' and receive 354
    send_receive_200 c (MailBody mail) -- send the content of the mail
    unwind0 c; sel2N c                 -- (annotation) branch to send 'QUIT'
    send c QUIT; close c               -- send 'QUIT' and close the connection
   ) (ixdo 
    (R5yz errmsg) <- recv c;           -- if 500 ERROR is offered
    sel2 d; send d errmsg;                -- (4) 
    send c QUIT; close c)              -- send 'QUIT' and close the connection
  close d


start host port from to msg = ixdo
  d <- new
  c <- connectNw (mkNwService host port undefined)
  forkIOs (sendMail c d)
  send d from
  send d to
  offer d (send d [msg] >>> io_ (putStrLn "complete.")) (recv d >>>= \errmsg -> io_ (print errmsg))
  close d

main = do
  putStrLn "host name of SMTP server:"
  host <- getLine
  putStrLn "port number:"
  portstr <- getLine
  putStrLn "from:"
  from <- getLine
  putStrLn "to:"
  to <- getLine
  putStrLn "msg:"
  msg <- getLine
  runS $ start host (read portstr) from to msg



instance Message EHLO where
  showMessage (EHLO domain) = "EHLO " ++ domain ++ "\r\n"
  parseMessage = undefined

instance Message MAIL where
  showMessage (MAIL revpath) = "MAIL FROM:<"++revpath++">\r\n"
  parseMessage = undefined

instance Message R2yz where
  showMessage = undefined
  parseMessage r = case readRes r of
    (ls@(('2':_):_),r') -> Just (R2yz ls,r')
    _ -> Nothing
 
instance Message R5yz where
  showMessage = undefined
  parseMessage r = case readRes r of
    (ls@(('5':_):_),r') -> Just (R5yz ls,r')
    _ -> Nothing

instance Message R354 where
  showMessage = undefined
  parseMessage r = case readRes r of
    ([l@('3':'5':'4':_)],r') -> Just (R354 l,r')
    _ -> Nothing

instance Message RCPT where
  showMessage (RCPT addr) = "RCPT TO:<"++addr++">\r\n"
  parseMessage = undefined

instance Message DATA where
  showMessage DATA = "DATA\r\n"
  parseMessage = undefined

instance Message MailBody where
  showMessage (MailBody ls) = mailBody ls
  parseMessage = undefined

instance Message QUIT where
  showMessage QUIT = "QUIT\r\n"
  parseMessage = undefined

cut :: Char -> String -> (String, String)
cut c s = let (l,r) = span (/=c) s in (l,safetail r)
  where
    safetail (_:s) = s
    safetail []    = []

line :: String -> (String, String)
line = cut '\n'

code_line s = splitAt 3 s

readRes :: String -> ([String],String)
readRes s =
      let (l, r)       = line s
          (code,l')    = code_line l      
      in 
        case head l' of 
           '-' -> let (ls,r') = readRes r in (l:ls,r')
           _   -> ([l],r)
{- 
http://tools.ietf.org/html/rfc2821#section-4.5.2
4.5.2 Transparency


   Without some provision for data transparency, the character sequence
   "<CRLF>.<CRLF>" ends the mail text and cannot be sent by the user.
   In general, users are not aware of such "forbidden" sequences.  To
   allow all user composed text to be transmitted transparently, the
   following procedures are used:

   -  Before sending a line of mail text, the SMTP client checks the
      first character of the line.  If it is a period, one additional
      period is inserted at the beginning of the line.

   -  When a line of mail text is received by the SMTP server, it checks
      the line.  If the line is composed of a single period, it is
      treated as the end of mail indicator.  If the first character is a
      period and there are other characters on the line, the first
      character is deleted.
-}
-- each line should be 7 bit strings
mailBody :: [String] -> String
mailBody ls = mb ls []
  where
    mb (l@('.':_):ls) s = '.':l ++ "\r\n"++mb ls s
    mb (l:ls) s   = l ++ "\r\n" ++ mb ls s
    mb [] s = ".\r\n" ++ s

