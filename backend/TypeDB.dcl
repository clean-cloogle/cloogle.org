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

:: TE_Priority = LeftAssoc Int | RightAssoc Int | NoAssoc Int

:: TypeExtras = { te_priority      :: Maybe TE_Priority
                , te_isconstructor :: Bool
                , te_isrecordfield :: Bool
                , te_generic_vars  :: Maybe [TypeVar]
                }

:: ExtendedType = ET Type TypeExtras

:: Macro = { macro_as_string :: String
           , macro_extras :: TypeExtras
           }

:: Location = Location Library Module LineNr Name
            | Builtin                        Name

:: Name         :== String
:: Library      :== String
:: Module       :== String
:: Class        :== String
:: LineNr       :== Maybe Int

derive gEq TypeDB

instance zero TypeDB
instance zero TypeExtras

instance print TE_Priority
instance print (Name, ExtendedType)

getName :: Location -> Name

getFunction :: Location TypeDB -> Maybe ExtendedType
putFunction :: Location ExtendedType TypeDB -> TypeDB
putFunctions :: [(Location, ExtendedType)] TypeDB -> TypeDB
findFunction :: Name TypeDB -> [(Location, ExtendedType)]
findFunction` :: (Location ExtendedType -> Bool) TypeDB
		-> [(Location, ExtendedType)]
findFunction`` :: [(Location ExtendedType -> Bool)] TypeDB
		-> [(Location, ExtendedType)]

getMacro :: Location TypeDB -> Maybe Macro
putMacro :: Location Macro TypeDB -> TypeDB
putMacros :: [(Location, Macro)] TypeDB -> TypeDB
findMacro` :: (Location Macro -> Bool) TypeDB -> [(Location, Macro)]

getInstances :: Class TypeDB -> [Type]
putInstance :: Class Type TypeDB -> TypeDB
putInstances :: Class [Type] TypeDB -> TypeDB
putInstancess :: [(Class, [Type])] TypeDB -> TypeDB

getClass :: Location TypeDB -> Maybe ([TypeVar],ClassContext,[(Name,ExtendedType)])
putClass :: Location [TypeVar] ClassContext [(Name,ExtendedType)] TypeDB -> TypeDB
putClasses :: [(Location, [TypeVar], ClassContext, [(Name,ExtendedType)])] TypeDB -> TypeDB
findClass :: Class TypeDB -> [(Location, [TypeVar], ClassContext, [(Name, ExtendedType)])]
findClass` :: (Location [TypeVar] ClassContext [(Name,ExtendedType)] -> Bool) TypeDB
		-> [(Location, [TypeVar], ClassContext, [(Name, ExtendedType)])]

findClassMembers` :: (Location [TypeVar] ClassContext Name ExtendedType -> Bool) TypeDB
		-> [(Location, [TypeVar], ClassContext, Name, ExtendedType)]
findClassMembers`` :: [Location [TypeVar] ClassContext Name ExtendedType -> Bool]
		TypeDB -> [(Location, [TypeVar], ClassContext, Name, ExtendedType)]

getType :: Location TypeDB -> Maybe TypeDef
putType :: Location TypeDef TypeDB -> TypeDB
putTypes :: [(Location, TypeDef)] TypeDB -> TypeDB
findType :: Name TypeDB -> [(Location, TypeDef)]
findType` :: (Location TypeDef -> Bool) TypeDB -> [(Location, TypeDef)]

getDerivations :: Name TypeDB -> [Type]
putDerivation :: Name Type TypeDB -> TypeDB
putDerivations :: Name [Type] TypeDB -> TypeDB
putDerivationss :: [(Name, [Type])] TypeDB -> TypeDB

searchExact :: Type TypeDB -> [(Location, ExtendedType)]

newDb :: TypeDB
openDb :: *File -> *(Maybe TypeDB, *File)
saveDb :: TypeDB *File -> *File
