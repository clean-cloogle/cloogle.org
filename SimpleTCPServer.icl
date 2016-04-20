implementation module SimpleTCPServer

import TCPIP
import StdEnv
import StdMaybe

instance zero (Logger a b s) where zero = \_ _ w -> (undef, w)

serve :: (a *World -> *(b,*World)) (Maybe (Logger a b s)) Port *World -> *World | fromString a & toString b
serve f log port w
# (ok, mbListener, w) = openTCP_Listener port w
| not ok = abort ("Couldn't open port " +++ toString port)
# listener = fromJust mbListener
# log = if (isNothing log) zero (fromJust log)
# (listener, w) = handle f log listener w
= closeRChannel listener w
where
    handle :: (a *World -> *(b,*World)) (Logger a b s) TCP_Listener *World
              -> (TCP_Listener, *World) | fromString a & toString b
    handle f log li w
    # ((ip, dupChan),li,w) = receive li w
    # (s, w) = log (Connected ip) undef w
    # (msg, rChan, w) = receive dupChan.rChannel w
      dupChan = {dupChan & rChannel=rChan}
    # msg = fromString (toString msg)
    # (s, w) = log (Received msg) s w
    # (resp, w) = f msg w
    # (sChan, w) = send (toByteSeq (toString resp)) dupChan.sChannel w
      dupChan = {dupChan & sChannel=sChan}
    # (s, w) = log (Sent resp) s w
    # w = closeRChannel dupChan.rChannel w
    # w = closeChannel dupChan.sChannel w
    # (s, w) = log Disconnected s w
    = handle f log li w

