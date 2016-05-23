implementation module SimpleTCPServer

import TCPIP
import StdEnv
import StdMaybe
import System._Posix

TIMEOUT :== Just 5000

instance zero (Logger a b s) where zero = \_ _ w -> (undef, w)

serve :: (a *World -> *(b,*World)) (Maybe (Logger a b s)) Port *World -> *World | fromString a & toString b
serve f log port w
# (ok, mbListener, w) = openTCP_Listener port w
| not ok = abort ("Couldn't open port " +++ toString port)
# listener = fromJust mbListener
# log = if (isNothing log) zero (fromJust log)
# (_,w) = signal 17 1 w // SIGCHLD, SIG_IGN: no notification if child ps dies
# (listener, w) = loop f log listener w
= closeRChannel listener w
where
	loop :: (a *World -> *(b,*World)) (Logger a b s) TCP_Listener *World
	     -> (TCP_Listener, *World) | fromString a & toString b
	loop f log li w
	#! ((ip,dupChan),li,w) = receive li w
	#! (st,w)              = log (Connected ip) undef w
	#  (pid,w)             = fork w
	| pid < 0
		= abort "fork failed\n"
	| pid > 0
		// Parent: handle new requests
		= loop f log li w
		// Child: handle current request
		= handle f log st dupChan w

	handle :: (a *World-> (b,*World)) (Logger a b s) !s !TCP_DuplexChannel !*World
	       -> (TCP_Listener, *World) | fromString a & toString b
	handle f log st dupChannel=:{rChannel,sChannel} w
	# (tRep,msg,rChannel,w) = receive_MT TIMEOUT rChannel w
	| tRep <> TR_Success
		# (st,w)            = log Disconnected st w
		# w                 = closeChannel sChannel (closeRChannel rChannel w)
		= exit 0 w
	# msg                = fromString (toString (fromJust msg))
	# (st, w)            = log (Received msg) st w
	# (resp, w)          = f msg w
	# (sChannel, w)      = send (toByteSeq (toString resp)) sChannel w
	# (st, w)            = log (Sent resp) st w
	= handle f log st {dupChannel & rChannel=rChannel, sChannel=sChannel} w

signal :: !Int !Int !*World -> *(!Int, !*World)
signal signum handler w = code {
	ccall signal "II:I:A"
}
