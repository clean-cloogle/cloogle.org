definition module SimpleTCPServer

from StdOverloaded import class zero, class fromString, class toString
from StdMaybe import :: Maybe
from TCPIP import ::IPAddress, ::Port

:: LogMessage a b t
	= Connected IPAddress
	| Received a
	| Sent b t
	| Disconnected

:: Logger a b s t :== (LogMessage a b t) (Maybe s) *World -> *(Maybe s, *World)

:: Server a b s t
	= { handler           :: !a *World -> *(!b, !t, !*World)
	  , logger            :: !Maybe (Logger a b s t)
	  , port              :: !Int
	  , keepalive_timeout :: !Maybe Int
	  }

serve :: !(Server a b s t) !*World -> *World | fromString a & toString b
