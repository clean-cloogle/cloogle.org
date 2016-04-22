definition module SimpleTCPServer

from StdOverloaded import class zero, class fromString, class toString
from StdMaybe import :: Maybe
from TCPIP import ::IPAddress, ::Port

:: LogMessage a b = Connected IPAddress
                  | Received a
                  | Sent b
                  | Disconnected

:: Logger a b s :== (LogMessage a b) s *World -> *(s, *World)

serve :: (a *World -> *(b,*World)) (Maybe (Logger a b s)) Port *World
         -> *World | fromString a & toString b

