definition module TypeDB

// Standard libraries
from StdOverloaded import class <, class zero
from StdClass import class Ord

from Data.Map import ::Map
from Data.Maybe import ::Maybe

from GenEq import generic gEq

// CleanTypeUnifier
from Type import ::Type, ::TypeVar, ::TypeVarAssignment, class print(..)

:: TypeDB
instance zero TypeDB
derive gEq TypeDB

:: FunctionLocation = FL Library Module FunctionName
instance < FunctionLocation
instance print FunctionLocation

:: Library :== String
:: Module :== String
:: FunctionName :== String

:: Class :== String

getType :: FunctionLocation TypeDB -> Maybe Type
putType :: FunctionLocation Type TypeDB -> TypeDB
putTypes :: [(FunctionLocation, Type)] TypeDB -> TypeDB

searchExact :: Type TypeDB -> [(FunctionLocation, Type)]
searchUnifiable :: Type TypeDB -> [(FunctionLocation, Type, [TypeVarAssignment], [TypeVarAssignment])]

newDb :: TypeDB
openDb :: *File -> *(Maybe TypeDB, *File)
saveDb :: TypeDB *File -> *File

