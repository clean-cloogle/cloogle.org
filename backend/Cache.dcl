definition module Cache

from StdOverloaded import class toString
from Data.Maybe import :: Maybe
from Text.JSON import generic JSONEncode, generic JSONDecode, :: JSONNode

readCache :: !a !*World -> (Maybe b, !*World) | toString a & JSONDecode{|*|} b
writeCache :: !a !b !*World -> (!b, !*World) | toString a & JSONEncode{|*|} b
