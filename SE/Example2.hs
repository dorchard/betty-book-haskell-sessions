(s, serverSpec) = makeSessionType $ do
  a <- newLabel
  let eq = do {recv intW; recv intW; recvSession (send boolW); jump a}
  a .= offer (eq ~|~ end ~|~ BLNil)
  return a

server = do
  cid <- fork serverChan dual (cons (serverSpec, notDual) nil) client
  c <- createSession serverSpec dual cid
  withChannel c (soffer ((do
       x <- srecv
       y <- srecv
       recvChannel c (\d ->
         withChannel d (do { ssend (x == y); sjump })))
   ~||~ (return ()) ~||~ OfferImplsNil))