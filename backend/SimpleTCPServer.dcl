definition module SimpleTCPServer

from StdOverloaded import class zero, class fromString, class toString
from StdMaybe import :: Maybe
from TCPIP import ::IPAddress, ::Port

:: LogMessage a b t = Connected IPAddress
                    | Received a
                    | Sent b t
                    | Disconnected

:: Logger a b s t :== (LogMessage a b t) s *World -> *(s, *World)

serve :: (a *World -> *(b,t,*World)) (Maybe (Logger a b s t)) Port *World
	-> *World | fromString a & toString b
