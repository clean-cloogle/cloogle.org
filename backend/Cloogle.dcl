definition module Cloogle

from StdOverloaded import class zero, class fromString, class toString, class <

from Data.Maybe import :: Maybe
from Text.JSON import generic JSONEncode, generic JSONDecode, :: JSONNode

:: Request
	= { unify            :: Maybe String
	  , name             :: Maybe String
	  , className        :: Maybe String
	  , typeName         :: Maybe String
	  , modules          :: Maybe [String]
	  , libraries        :: Maybe [String]
	  , include_builtins :: Maybe Bool
	  , include_core     :: Maybe Bool
	  , page             :: Maybe Int
	  }

:: Response
	= { return         :: Int
	  , data           :: [Result]
	  , msg            :: String
	  , more_available :: Maybe Int
	  , suggestions    :: Maybe [(Request, Int)]
	  }

:: Result
	= FunctionResult FunctionResult
	| TypeResult TypeResult
	| ClassResult ClassResult
	| MacroResult MacroResult
	| ModuleResult ModuleResult

:: BasicResult
	= { library  :: String
	  , filename :: String
	  , modul    :: String
	  , dcl_line :: Maybe Int
	  , icl_line :: Maybe Int
	  , distance :: Int
	  , builtin  :: Maybe Bool
	  }

:: FunctionResult :== (BasicResult, FunctionResultExtras)
:: FunctionResultExtras
	= { func                :: String
	  , unifier             :: Maybe StrUnifier
	  , cls                 :: Maybe ShortClassResult
	  , constructor_of      :: Maybe String
	  , recordfield_of      :: Maybe String
	  , generic_derivations :: Maybe [(String, [LocationResult])]
	  }

:: TypeResult :== (BasicResult, TypeResultExtras)
:: TypeResultExtras
	= { type             :: String
	  , type_instances   :: [(String, [String], [LocationResult])]
	  , type_derivations :: [(String, [LocationResult])]
	  }

:: ClassResult :== (BasicResult, ClassResultExtras)
:: ClassResultExtras
	= { class_name      :: String
	  , class_heading   :: String
	  , class_funs      :: [String]
	  , class_instances :: [([String], [LocationResult])]
	  }

:: MacroResult :== (BasicResult, MacroResultExtras)
:: MacroResultExtras
	= { macro_name           :: String
	  , macro_representation :: String
	  }

:: ModuleResult :== (BasicResult, ModuleResultExtras)
:: ModuleResultExtras
	= { module_is_core :: Bool
	  }

:: LocationResult :== (String, String, Maybe Int, Maybe Int)

:: StrUnifier
	= { left_to_right :: [(String,String)]
	  , right_to_left :: [(String,String)]
	  , used_synonyms :: [(String,String)]
	  }

:: ShortClassResult = { cls_name :: String, cls_vars :: [String] }

derive JSONEncode Request, Response, Result, ShortClassResult, BasicResult,
	FunctionResultExtras, TypeResultExtras, ClassResultExtras,
	MacroResultExtras, ModuleResultExtras, StrUnifier
derive JSONDecode Request, Response, Result, ShortClassResult, BasicResult,
	FunctionResultExtras, TypeResultExtras, ClassResultExtras,
	MacroResultExtras, ModuleResultExtras, StrUnifier

instance zero Request
instance zero Response

instance toString Request
instance toString Response

instance fromString (Maybe Request)

instance < BasicResult
instance < Result

CLOOGLE_E_NORESULTS    :== 127
CLOOGLE_E_INVALIDINPUT :== 128
CLOOGLE_E_INVALIDNAME  :== 129
CLOOGLE_E_INVALIDTYPE  :== 130

err :: Int String -> Response
