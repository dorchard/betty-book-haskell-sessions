import Control.Concurrent.FullSession
import Chatter
import System.Environment (getArgs)

main = do
  [port] <- getArgs
  runS $ server "Mr.Server" (read port)