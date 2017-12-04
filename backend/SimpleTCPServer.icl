implementation module SimpleTCPServer

import StdEnv
import Data.Maybe
import System._Posix
import TCPIP

instance zero (Logger a b s t) where zero = \_ _ w -> (undef, w)

serve :: !(Server a b s t) !*World -> *World | fromString a & toString b
serve server w
# (ok, mbListener, w) = openTCP_Listener server.port w
| not ok = abort ("Couldn't open port " +++ toString server.port +++ "\n")
# listener = fromJust mbListener
# (_,w) = signal 17 1 w // SIGCHLD, SIG_IGN: no notification if child ps dies
# (listener, w) = loop listener w
= closeRChannel listener w
where
	logger = fromMaybe zero server.logger

	loop :: TCP_Listener *World -> (TCP_Listener, *World)
	loop li w
	#! (tRep,conn,li,w) = receive_MT server.connect_timeout li w
	| tRep <> TR_Success
		= (li,w)
	#! (ip,dupChan)     = fromJust conn
	#! (pid,w)          = fork w
	| pid < 0
		= abort "fork failed\n"
	| pid > 0 // Parent: handle new requests
		= loop li w
	| pid == 0 // Child: handle current request
		#! (st,w)       = logger (Connected ip) Nothing w
		= handle st dupChan w

	//handle :: !(Maybe s) !TCP_DuplexChannel !*World -> *(!TCP_Listener, !*World)
	handle st dupChannel=:{rChannel,sChannel} w
	#! (tRep,msg,rChannel,w) = receive_MT server.keepalive_timeout rChannel w
	| tRep <> TR_Success
		#! (st,w)            = logger Disconnected st w
		#! w                 = closeChannel sChannel (closeRChannel rChannel w)
		= exit 0 w
	#! msg                   = fromString (toString (fromJust msg))
	#! (st, w)               = logger (Received msg) st w
	#! (resp, hidden, w)     = server.handler msg w
	#! (sChannel, w)         = send (toByteSeq (toString resp)) sChannel w
	#! (st, w)               = logger (Sent resp hidden) st w
	= handle st {dupChannel & rChannel=rChannel, sChannel=sChannel} w

signal :: !Int !Int !*World -> *(!Int, !*World)
signal signum handler w = code {
	ccall signal "II:I:A"
}
