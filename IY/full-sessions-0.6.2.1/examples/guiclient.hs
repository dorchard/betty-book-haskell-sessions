
import GuiChatter
import EnableGUI
import System.Environment

main = do
  [host,port] <- getArgs
  enableGUI
  client "Ms.Client" host (read port)
  

