definition module TypeDB

// Standard libraries
from StdOverloaded import class <, class zero
from StdClass import class Ord

from Data.Map import ::Map
from Data.Maybe import ::Maybe

from GenEq import generic gEq

// CleanTypeUnifier
from Type import ::Type, ::TypeVar, ::TVAssignment, ::TypeDef, class print(..),
  ::ClassContext, ::ClassRestriction, ::ClassOrGeneric, ::Priority

:: TypeDB

:: TypeExtras = { te_priority       :: Maybe Priority
                , te_isconstructor  :: Bool
                , te_isrecordfield  :: Bool
                , te_generic_vars   :: Maybe [TypeVar]
                , te_representation :: Maybe String
                }

:: ExtendedType = ET Type TypeExtras

:: Macro = { macro_as_string :: String
           , macro_extras :: TypeExtras
           }

:: Location = Location Library Module LineNr LineNr Name
            | Builtin                               Name

:: Name         :== String
:: Library      :== String
:: Module       :== String
:: Class        :== String
:: LineNr       :== Maybe Int

derive gEq TypeDB

instance zero TypeDB
instance zero TypeExtras

instance print (Name, ExtendedType)

getName :: Location -> Name

functionCount :: TypeDB -> Int
macroCount :: TypeDB -> Int
classCount :: TypeDB -> Int
instanceCount :: TypeDB -> Int
typeCount :: TypeDB -> Int
deriveCount :: TypeDB -> Int

filterLocations :: (Location -> Bool) TypeDB -> TypeDB

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
findMacro`` :: [(Location Macro -> Bool)] TypeDB -> [(Location, Macro)]

getInstances :: Class TypeDB -> [([Type], [Location])]
putInstance :: Class [Type] Location TypeDB -> TypeDB
putInstances :: [(Class, [Type], Location)] TypeDB -> TypeDB

getClass :: Location TypeDB -> Maybe ([TypeVar],ClassContext,[(Name,ExtendedType)])
putClass :: Location [TypeVar] ClassContext [(Name,ExtendedType)] TypeDB -> TypeDB
putClasses :: [(Location, [TypeVar], ClassContext, [(Name,ExtendedType)])] TypeDB -> TypeDB
findClass :: Class TypeDB -> [(Location, [TypeVar], ClassContext, [(Name, ExtendedType)])]
findClass` :: (Location [TypeVar] ClassContext [(Name,ExtendedType)] -> Bool) TypeDB
		-> [(Location, [TypeVar], ClassContext, [(Name, ExtendedType)])]
findClass`` :: [(Location [TypeVar] ClassContext [(Name,ExtendedType)] -> Bool)] TypeDB
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
findType`` :: [(Location TypeDef -> Bool)] TypeDB -> [(Location, TypeDef)]

getDerivations :: Name TypeDB -> [(Type, [Location])]
putDerivation :: Name Type Location TypeDB -> TypeDB
putDerivations :: Name [(Type, Location)] TypeDB -> TypeDB
putDerivationss :: [(Name, [(Type, Location)])] TypeDB -> TypeDB

searchExact :: Type TypeDB -> [(Location, ExtendedType)]

getTypeInstances :: Name TypeDB -> [(Class, [Type], [Location])]
getTypeDerivations :: Name TypeDB -> [(Name, [Location])]

newDb :: TypeDB
openDb :: *File -> *(Maybe TypeDB, *File)
saveDb :: !TypeDB !*File -> *File
