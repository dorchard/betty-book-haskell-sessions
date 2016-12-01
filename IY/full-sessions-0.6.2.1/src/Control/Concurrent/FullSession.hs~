-- | Pi-calculus style communication and concurrency primitives which come with session types.
module Control.Concurrent.FullSession (
-- * Type level construcs
-- ** Type level numbers and booleans
    Z, S, P, Nat, T, F, 
-- * Session types (protocol types)
    Send, Recv, Throw, Catch, Select, Offer, End, Bot, Rec, Var, Close, SelectN, OfferN,
-- * Session type environments
    (:>), Nil,
-- * The Session monad
    Session, (>>>=), (>>>), ireturn, runS, 
-- * Communication and concurrency primitives
-- ** Channel types 
    Channel, Service, 
-- ** General communication
    close, send, recv, sendS, recvS, sel1, sel2, ifSelect, offer, new, newService, connect, connectRunS, accept, acceptRunS,
-- ** Network communication
-- *** Primitives
    connectNw, connectNw2, acceptOneNw2, sel1N, sel2N, ifSelectN, offerN, 
    dualNw, dualNw2, mkNwService, mkNwService2, 
-- *** Type class for messages
    Message (parseMessage, showMessage), 
-- ** Thread creation
    forkIOs, forkOSs,
-- ** Interfacing with the IO monad
    io, io_, 
-- ** Exception handling
    finallys,
-- ** Recursive protocol support
    unwind0, unwind1, unwind2, recur1, recur2, 
-- * Utility functions for type inferene
    channeltype1, channeltype2, typecheck1, typecheck2,
-- * Type classes for type-level operations
-- ** Type level arithmetics and boolean operators
    EqNat, Sub, SubT, And,
-- ** Operations on type level lists
    SList, Pickup, PickupR, Update, UpdateR,
-- ** Type classes for ended type environments (1)
    Ended, IsEnded, IsEndedST,
-- ** Duality of session types
    Dual, 
-- ** Parallel composition of session types
    Comp, Par, Par', 
-- ** Type classes for ended type environments (2)
    EndedWithout, EndedWithout', EndedWithout2, EndedWithout2', AppendEnd, AppendEnd', Diff, Diff', 
-- ** Restrictions on session types for network communication
    NwService, NwService2, NwSender, NwReceiver, NwSession, NwDual, NwSendOnly, NwReceiveOnly, 
-- ** Recursive protocol
    RecFold, RecFoldCont, RecFold2, RecFoldCont2, RecUnfold, RecUnfoldCont,
-- ** Type equality
    TypeEq, TypeEq', TypeEq'',
  ) where

import FullSession.Base
import FullSession.TypeEq
import FullSession.Types
import FullSession.TypeAlgebra
import FullSession.Ended
import FullSession.Recursion
import FullSession.SMonad
import FullSession.FullSession
import FullSession.NwSession
import FullSession.Incoherent
import FullSession.DeferredInstances
