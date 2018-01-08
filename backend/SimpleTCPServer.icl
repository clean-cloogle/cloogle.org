implementation module SimpleTCPServer

import StdEnv
import Data.Maybe
import System._Posix
import TCPIP

instance zero (Logger a b s t) where zero = \_ _ w -> (undef, w)

serve :: !(Server req res .st logst sentinfo) .st !*World -> *World | fromString req & toString res
serve server st w
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
		#! (logst,w)    = logger (Connected ip) Nothing w
		= handle logst st dupChan w

	//handle :: !(Maybe s) !TCP_DuplexChannel !*World -> *(!TCP_Listener, !*World)
	handle logst st dupChannel=:{rChannel,sChannel} w
	#! (tRep,msg,rChannel,w) = receive_MT server.keepalive_timeout rChannel w
	| tRep <> TR_Success
		#! (logst,w)         = logger Disconnected logst w
		#! w                 = closeChannel sChannel (closeRChannel rChannel w)
		= exit 0 w
	#! msg                   = fromString (toString (fromJust msg))
	#! (logst, w)            = logger (Received msg) logst w
	#! (resp, hidden, st, w) = server.handler msg st w
	#! (sChannel, w)         = send (toByteSeq (toString resp)) sChannel w
	#! (logst, w)            = logger (Sent resp hidden) logst w
	= handle logst st {dupChannel & rChannel=rChannel, sChannel=sChannel} w

signal :: !Int !Int !*World -> *(!Int, !*World)
signal signum handler w = code {
	ccall signal "II:I:A"
}
