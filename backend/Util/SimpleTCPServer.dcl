definition module Util.SimpleTCPServer

from StdOverloaded import class zero, class fromString, class toString
from StdMaybe import :: Maybe
from TCPIP import ::IPAddress, ::Port

:: LogMessage req res sentinfo
	= Connected IPAddress
	| Received req
	| Sent res sentinfo
	| Disconnected

:: Logger req res logst sentinfo
	:== (LogMessage req res sentinfo) (Maybe logst) *World -> *(Maybe logst, *World)

:: Server req res st logst sentinfo =
	{ handler           :: !req -> .(st -> *(*World -> *(!res, !sentinfo, !st, !*World)))
	, logger            :: !Maybe (Logger req res logst sentinfo)
	, port              :: !Int
	, connect_timeout   :: !Maybe Int
	, keepalive_timeout :: !Maybe Int
	}

serve :: !(Server req res .st logst sentinfo) .st !*World -> *World | fromString req & toString res
