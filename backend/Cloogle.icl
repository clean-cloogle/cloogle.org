implementation module Cloogle

from Data.Func import $
import Text
import Text.JSON

derive JSONEncode Request, Response, Result, ShortClassResult, BasicResult,
	FunctionResultExtras, TypeResultExtras, ClassResultExtras, MacroResultExtras
derive JSONDecode Request, Response, Result, ShortClassResult, BasicResult,
	FunctionResultExtras, TypeResultExtras, ClassResultExtras, MacroResultExtras

instance zero Request
where
	zero = { unify            = Nothing
	       , name             = Nothing
	       , className        = Nothing
	       , typeName         = Nothing
	       , modules          = Nothing
	       , libraries        = Nothing
		   , include_builtins = Nothing
	       , page             = Nothing
	       }

instance zero Response
where
	zero = { return         = 0
	       , msg            = "Success"
	       , data           = []
	       , more_available = Nothing
	       , suggestions    = Nothing
	       }

instance toString Request where toString r = toString $ toJSON r
instance toString Response where toString r = toString (toJSON r) + "\n"

instance fromString (Maybe Request) where fromString s = fromJSON $ fromString s

instance < BasicResult where (<) r1 r2 = r1.distance < r2.distance
instance < Result
where
	(<) r1 r2 = basic r1 < basic r2
	where
		basic :: Result -> BasicResult
		basic (FunctionResult (br,_)) = br
		basic (TypeResult     (br,_)) = br
		basic (ClassResult    (br,_)) = br
		basic (MacroResult    (br,_)) = br

err :: Int String -> Response
err c m = { return         = c
          , data           = []
          , msg            = m
          , more_available = Nothing
          , suggestions    = Nothing
          }
