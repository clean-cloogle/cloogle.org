definition module TypeDB

// Standard libraries
from StdOverloaded import class <, class zero
from StdClass import class Ord

from Data.Map import ::Map
from Data.Maybe import ::Maybe

from GenEq import generic gEq

// CleanTypeUnifier
from Type import ::Type, ::TypeVar, ::TVAssignment, ::TypeDef, class print(..),
  ::ClassContext, ::ClassRestriction, ::ClassOrGeneric

:: TypeDB
instance zero TypeDB
derive gEq TypeDB

:: FunctionLocation = FL Library Module FunctionName
instance print FunctionLocation

:: TypeExtras = { te_priority      :: Maybe TE_Priority
                , te_isconstructor :: Bool
                , te_generic_vars  :: Maybe [TypeVar]
                }
instance zero TypeExtras

:: TE_Priority = LeftAssoc Int | RightAssoc Int | NoAssoc Int
instance print TE_Priority

:: ExtendedType = ET Type TypeExtras

instance print (FunctionName, ExtendedType)

:: ClassLocation = CL Library Module Class

:: Library :== String
:: Module :== String
:: FunctionName :== String
:: Class :== String

:: TypeLocation = TL Library Module TypeName

:: TypeName :== String

getFunction :: FunctionLocation TypeDB -> Maybe ExtendedType
putFunction :: FunctionLocation ExtendedType TypeDB -> TypeDB
putFunctions :: [(FunctionLocation, ExtendedType)] TypeDB -> TypeDB
findFunction :: FunctionName TypeDB -> [(FunctionLocation, ExtendedType)]
findFunction` :: (FunctionLocation ExtendedType -> Bool) TypeDB
		-> [(FunctionLocation, ExtendedType)]
findFunction`` :: [(FunctionLocation ExtendedType -> Bool)] TypeDB
		-> [(FunctionLocation, ExtendedType)]

getInstances :: Class TypeDB -> [Type]
putInstance :: Class Type TypeDB -> TypeDB
putInstances :: Class [Type] TypeDB -> TypeDB
putInstancess :: [(Class, [Type])] TypeDB -> TypeDB

getClass :: ClassLocation TypeDB -> Maybe ([TypeVar],ClassContext,[(FunctionName,ExtendedType)])
putClass :: ClassLocation [TypeVar] ClassContext [(FunctionName,ExtendedType)] TypeDB -> TypeDB
putClasses :: [(ClassLocation, [TypeVar], ClassContext, [(FunctionName,ExtendedType)])] TypeDB -> TypeDB
findClass :: Class TypeDB -> [(ClassLocation, [TypeVar], ClassContext, [(FunctionName, ExtendedType)])]
findClass` :: (ClassLocation [TypeVar] ClassContext [(FunctionName,ExtendedType)] -> Bool) TypeDB
		-> [(ClassLocation, [TypeVar], ClassContext, [(FunctionName, ExtendedType)])]

findClassMembers` :: (ClassLocation [TypeVar] ClassContext FunctionName ExtendedType -> Bool) TypeDB
		-> [(ClassLocation, [TypeVar], ClassContext, FunctionName, ExtendedType)]
findClassMembers`` :: [ClassLocation [TypeVar] ClassContext FunctionName ExtendedType -> Bool]
		TypeDB -> [(ClassLocation, [TypeVar], ClassContext, FunctionName, ExtendedType)]

getType :: TypeLocation TypeDB -> Maybe TypeDef
putType :: TypeLocation TypeDef TypeDB -> TypeDB
putTypes :: [(TypeLocation, TypeDef)] TypeDB -> TypeDB
findType :: TypeName TypeDB -> [(TypeLocation, TypeDef)]
findType` :: (TypeLocation TypeDef -> Bool) TypeDB
		-> [(TypeLocation, TypeDef)]

searchExact :: Type TypeDB -> [(FunctionLocation, ExtendedType)]

newDb :: TypeDB
openDb :: *File -> *(Maybe TypeDB, *File)
saveDb :: TypeDB *File -> *File
