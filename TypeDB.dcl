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

:: TypeExtras = { te_priority :: Maybe TE_Priority }
:: TE_Priority = LeftAssoc Int | RightAssoc Int | NoAssoc Int
instance print TE_Priority

:: ExtendedType = ET Type TypeExtras

:: ClassLocation = CL Library Module Class

:: Library :== String
:: Module :== String
:: FunctionName :== String
:: Class :== String

getType :: FunctionLocation TypeDB -> Maybe ExtendedType
putType :: FunctionLocation ExtendedType TypeDB -> TypeDB
putTypes :: [(FunctionLocation, ExtendedType)] TypeDB -> TypeDB
findType :: FunctionName TypeDB -> [(FunctionLocation, ExtendedType)]
findType` :: (FunctionLocation ExtendedType -> Bool) TypeDB
		-> [(FunctionLocation, ExtendedType)]
findType`` :: [(FunctionLocation ExtendedType -> Bool)] TypeDB
		-> [(FunctionLocation, ExtendedType)]

getInstances :: Class TypeDB -> [Type]
putInstance :: Class Type TypeDB -> TypeDB
putInstances :: Class [Type] TypeDB -> TypeDB
putInstancess :: [(Class, [Type])] TypeDB -> TypeDB

getClass :: ClassLocation TypeDB -> Maybe ([TypeVar],[(FunctionName,ExtendedType)])
putClass :: ClassLocation [TypeVar] [(FunctionName, ExtendedType)] TypeDB -> TypeDB
putClasses :: [(ClassLocation, [TypeVar], [(FunctionName, ExtendedType)])] TypeDB -> TypeDB
findClass :: Class TypeDB -> [(ClassLocation, [TypeVar], [(FunctionName, ExtendedType)])]
findClass` :: (ClassLocation [TypeVar] [(FunctionName,ExtendedType)] -> Bool) TypeDB
		-> [(ClassLocation, [TypeVar], [(FunctionName, ExtendedType)])]

findClassMembers` :: (ClassLocation [TypeVar] FunctionName ExtendedType -> Bool) TypeDB
		-> [(ClassLocation, [TypeVar], FunctionName, ExtendedType)]
findClassMembers`` :: [ClassLocation [TypeVar] FunctionName ExtendedType -> Bool]
		TypeDB -> [(ClassLocation, [TypeVar], FunctionName, ExtendedType)]


searchExact :: Type TypeDB -> [(FunctionLocation, ExtendedType)]
searchUnifiable :: Type TypeDB
        -> [(FunctionLocation, ExtendedType, [TVAssignment], [TVAssignment])]

newDb :: TypeDB
openDb :: *File -> *(Maybe TypeDB, *File)
saveDb :: TypeDB *File -> *File

