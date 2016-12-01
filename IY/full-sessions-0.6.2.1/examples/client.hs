import Control.Concurrent.FullSession
import Chatter
import System.Environment (getArgs)

main = do
  [host, port] <- getArgs
  runS $ client "Ms.Client" host (read port)
