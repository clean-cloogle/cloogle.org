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

:: FunctionLocation = FL         Library Module FunctionName LineNr
                    | FL_Builtin                FunctionName
:: MacroLocation    = ML         Library Module MacroName    LineNr
:: ClassLocation    = CL         Library Module Class        LineNr
:: TypeLocation     = TL         Library Module TypeName     LineNr
                    | TL_Builtin                TypeName

:: Library      :== String
:: Module       :== String
:: FunctionName :== String
:: MacroName    :== String
:: Class        :== String
:: GenericName  :== String
:: TypeName     :== String
:: LineNr       :== Maybe Int

derive gEq TypeDB

instance zero TypeDB
instance zero TypeExtras

instance print TE_Priority
instance print (FunctionName, ExtendedType)

getFunction :: FunctionLocation TypeDB -> Maybe ExtendedType
putFunction :: FunctionLocation ExtendedType TypeDB -> TypeDB
putFunctions :: [(FunctionLocation, ExtendedType)] TypeDB -> TypeDB
findFunction :: FunctionName TypeDB -> [(FunctionLocation, ExtendedType)]
findFunction` :: (FunctionLocation ExtendedType -> Bool) TypeDB
		-> [(FunctionLocation, ExtendedType)]
findFunction`` :: [(FunctionLocation ExtendedType -> Bool)] TypeDB
		-> [(FunctionLocation, ExtendedType)]

getMacro :: MacroLocation TypeDB -> Maybe Macro
putMacro :: MacroLocation Macro TypeDB -> TypeDB
putMacros :: [(MacroLocation, Macro)] TypeDB -> TypeDB
findMacro` :: (MacroLocation Macro -> Bool) TypeDB -> [(MacroLocation, Macro)]

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

getDerivations :: GenericName TypeDB -> [Type]
putDerivation :: GenericName Type TypeDB -> TypeDB
putDerivations :: GenericName [Type] TypeDB -> TypeDB
putDerivationss :: [(GenericName, [Type])] TypeDB -> TypeDB

searchExact :: Type TypeDB -> [(FunctionLocation, ExtendedType)]

newDb :: TypeDB
openDb :: *File -> *(Maybe TypeDB, *File)
saveDb :: TypeDB *File -> *File
