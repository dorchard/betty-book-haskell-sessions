
import GuiChatter
import EnableGUI
import System.Environment

main = do
  [port] <- getArgs
  enableGUI
  server "Mr.Server" (read port)
