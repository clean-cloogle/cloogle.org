definition module TypeDB

// Standard libraries
from StdOverloaded import class <, class zero
from StdClass import class Ord

from Data.Map import ::Map
from Data.Maybe import ::Maybe

from GenEq import generic gEq

// CleanTypeUnifier
from Type import ::Type, ::TypeVar, ::TVAssignment, class print(..)

:: TypeDB
instance zero TypeDB
derive gEq TypeDB

:: FunctionLocation = FL Library Module FunctionName
instance < FunctionLocation
instance print FunctionLocation

:: ClassLocation = CL Library Module Class

:: Library :== String
:: Module :== String
:: FunctionName :== String
:: Class :== String

getType :: FunctionLocation TypeDB -> Maybe Type
putType :: FunctionLocation Type TypeDB -> TypeDB
putTypes :: [(FunctionLocation, Type)] TypeDB -> TypeDB
findType :: FunctionName TypeDB -> [(FunctionLocation, Type)]
findType` :: (FunctionLocation Type -> Bool) TypeDB -> [(FunctionLocation, Type)]
findType`` :: [FunctionLocation Type -> Bool] TypeDB -> [(FunctionLocation, Type)]

getInstances :: Class TypeDB -> [Type]
putInstance :: Class Type TypeDB -> TypeDB
putInstances :: Class [Type] TypeDB -> TypeDB
putInstancess :: [(Class, [Type])] TypeDB -> TypeDB

getClass :: ClassLocation TypeDB -> Maybe ([TypeVar],[(FunctionName,Type)])
putClass :: ClassLocation [TypeVar] [(FunctionName, Type)] TypeDB -> TypeDB
putClasses :: [(ClassLocation, [TypeVar], [(FunctionName, Type)])] TypeDB -> TypeDB
findClass :: Class TypeDB -> [(ClassLocation, [TypeVar], [(FunctionName, Type)])]
findClass` :: (ClassLocation [TypeVar] [(FunctionName,Type)] -> Bool) TypeDB
		-> [(ClassLocation, [TypeVar], [(FunctionName, Type)])]

findClassMembers` :: (ClassLocation [TypeVar] FunctionName Type -> Bool) TypeDB
		-> [(ClassLocation, [TypeVar], FunctionName, Type)]
findClassMembers`` :: [ClassLocation [TypeVar] FunctionName Type -> Bool]
		TypeDB -> [(ClassLocation, [TypeVar], FunctionName, Type)]


searchExact :: Type TypeDB -> [(FunctionLocation, Type)]
searchUnifiable :: Type TypeDB
		-> [(FunctionLocation, Type, [TVAssignment], [TVAssignment])]

newDb :: TypeDB
openDb :: *File -> *(Maybe TypeDB, *File)
saveDb :: TypeDB *File -> *File

