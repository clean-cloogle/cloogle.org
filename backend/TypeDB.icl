implementation module TypeDB

// Standard libraries
import StdEnv
from Data.Func import $
from Data.List import intercalate
import Data.Map
import Data.Maybe
import Text.JSON

// CleanTypeUnifier
import Type

:: TypeDB
	= { functionmap :: Map FunctionLocation ExtendedType
	  , macromap    :: Map MacroLocation Macro
	  , classmap    :: Map ClassLocation ([TypeVar],ClassContext,[(FunctionName, ExtendedType)])
	  , instancemap :: Map Class [Type]
	  , typemap     :: Map TypeLocation TypeDef
	  , derivemap   :: Map GenericName [Type]
	  }

printersperse :: Bool a [b] -> [String] | print a & print b
printersperse ia a bs = intercalate (print False a) (map (print ia) bs)

(--) infixr 5 :: a b -> [String] | print a & print b
(--) a b = print False a ++ print False b

derive gEq ClassOrGeneric, FunctionLocation, ClassLocation, Type, TypeDB,
	TypeExtras, TE_Priority, ExtendedType, TypeDef, TypeLocation, TypeDefRhs,
	RecordField, Constructor, Kind, MacroLocation, Macro
derive JSONEncode ClassOrGeneric, FunctionLocation, ClassLocation, Type,
	TypeDB, TypeExtras, TE_Priority, ExtendedType, TypeDef, TypeLocation,
	TypeDefRhs, RecordField, Constructor, Kind, MacroLocation, Macro
derive JSONDecode ClassOrGeneric, FunctionLocation, ClassLocation, Type,
	TypeDB, TypeExtras, TE_Priority, ExtendedType, TypeDef, TypeLocation,
	TypeDefRhs, RecordField, Constructor, Kind, MacroLocation, Macro

instance zero TypeDB
where
	zero = { functionmap   = newMap
	       , macromap      = newMap
	       , classmap      = newMap
	       , instancemap   = newMap
	       , typemap       = newMap
	       , derivemap     = newMap
	       }

instance < FunctionLocation where (<) (FL a b c) (FL d e f) = (a,b,c) < (d,e,f)
instance print FunctionLocation
where print _ (FL lib mod fn) = fn -- " in " -- mod -- " in " -- lib

instance < MacroLocation where (<) (ML a b c) (ML d e f) = (a,b,c) < (d,e,f)
instance print MacroLocation
where print _ (ML lib mod mn) = mn -- " in " -- mod -- " in " -- lib

instance < ClassLocation where (<) (CL a b c) (CL d e f) = (a,b,c) < (d,e,f)

instance < TypeLocation
where
	(<) (TL_Builtin a) (TL_Builtin b) = a < b
	(<) (TL_Builtin _) (TL _ _ _)     = True
	(<) (TL _ _ _)     (TL_Builtin _) = False
	(<) (TL a b c)     (TL d e f)     = (a,b,c) < (d,e,f)

instance zero TypeExtras
where
	zero = { te_priority      = Nothing
	       , te_isconstructor = False
	       , te_generic_vars  = Nothing
	       }

instance print TypeExtras
where
	print b {te_priority=Just p} = print b p -- " "
	print b {te_generic_vars=Just vars} = printersperse b " " vars -- " "
	print _ _ = []

instance print TE_Priority
where
	print _ (LeftAssoc i)  = "infixl " -- i
	print _ (RightAssoc i) = "infixr " -- i
	print _ (NoAssoc i)    = "infix " -- i

instance print (FunctionName, ExtendedType)
where
	print _ (f, (ET t e=:{te_generic_vars=Just _}))
		= "generic " -- f -- " " -- e -- " :: " -- t
	print _ (f, (ET t e=:{te_priority=Just _}))
		= "(" -- f -- ") " -- e -- " :: " -- t
	print _ (f, (ET t e=:{te_priority=Nothing}))
		= f -- " " -- e -- " :: " -- t

getFunction :: FunctionLocation TypeDB -> Maybe ExtendedType
getFunction loc {functionmap} = get loc functionmap

putFunction :: FunctionLocation ExtendedType TypeDB -> TypeDB
putFunction fl t tdb=:{functionmap} = { tdb & functionmap = put fl t functionmap }

putFunctions :: [(FunctionLocation, ExtendedType)] TypeDB -> TypeDB
putFunctions ts tdb = foldr (\(loc,t) db -> putFunction loc t db) tdb ts

findFunction :: FunctionName TypeDB -> [(FunctionLocation, ExtendedType)]
findFunction f db=:{functionmap}
	= toList $ filterWithKey (\(FL _ _ f`) _->f==f`) functionmap

findFunction` :: (FunctionLocation ExtendedType -> Bool) TypeDB
	-> [(FunctionLocation, ExtendedType)]
findFunction` f {functionmap} = toList $ filterWithKey f functionmap

findFunction`` :: [(FunctionLocation ExtendedType -> Bool)] TypeDB
	-> [(FunctionLocation, ExtendedType)]
findFunction`` fs {functionmap} = toList $ foldr filterWithKey functionmap fs

getMacro :: MacroLocation TypeDB -> Maybe Macro
getMacro loc {macromap} = get loc macromap

putMacro :: MacroLocation Macro TypeDB -> TypeDB
putMacro ml m db=:{macromap} = { db & macromap = put ml m macromap }

putMacros :: [(MacroLocation, Macro)] TypeDB -> TypeDB
putMacros ms db = foldr (\(loc,m) db -> putMacro loc m db) db ms

findMacro` :: (MacroLocation Macro -> Bool) TypeDB -> [(MacroLocation, Macro)]
findMacro` f {macromap} = toList $ filterWithKey f macromap

getInstances :: Class TypeDB -> [Type]
getInstances c {instancemap} = if (isNothing ts) [] (fromJust ts)
where ts = get c instancemap

putInstance :: Class Type TypeDB -> TypeDB
putInstance c t db=:{instancemap} = {db & instancemap=put c ts instancemap}
where ts = removeDup [t : getInstances c db]

putInstances :: Class [Type] TypeDB -> TypeDB
putInstances c ts db = foldr (\t db -> putInstance c t db) db ts

putInstancess :: [(Class, [Type])] TypeDB -> TypeDB
putInstancess is db = foldr (\(c,ts) db -> putInstances c ts db) db is

getClass :: ClassLocation TypeDB -> Maybe ([TypeVar],ClassContext,[(FunctionName,ExtendedType)])
getClass loc {classmap} = get loc classmap

putClass :: ClassLocation [TypeVar] ClassContext [(FunctionName, ExtendedType)] TypeDB -> TypeDB
putClass cl tvs cc fs db=:{classmap} = {db & classmap = put cl (tvs,cc,fs) classmap}

putClasses :: [(ClassLocation, [TypeVar], ClassContext, [(FunctionName, ExtendedType)])] TypeDB -> TypeDB
putClasses cs db = foldr (\(cl,tvs,cc,fs) db -> putClass cl tvs cc fs db) db cs

findClass :: Class TypeDB -> [(ClassLocation, [TypeVar], ClassContext, [(FunctionName, ExtendedType)])]
findClass c {classmap} = map (\(k,(x,y,z))->(k,x,y,z)) results
where results = toList $ filterWithKey (\(CL _ _ c`) _->c==c`) classmap

findClass` :: (ClassLocation [TypeVar] ClassContext [(FunctionName,ExtendedType)] -> Bool) TypeDB
		-> [(ClassLocation, [TypeVar], ClassContext, [(FunctionName,ExtendedType)])]
findClass` f {classmap} = map (\(k,(x,y,z))->(k,x,y,z)) results
where results = toList $ filterWithKey (\cl (vs,cc,fs)->f cl vs cc fs) classmap

findClassMembers` :: (ClassLocation [TypeVar] ClassContext FunctionName ExtendedType -> Bool) TypeDB
		-> [(ClassLocation, [TypeVar], ClassContext, FunctionName, ExtendedType)]
findClassMembers` f {classmap} = filter (app5 f) $ flatten members
where
	members = map (\(cl,(vs,cc,fs))->[(cl,vs,cc,f,t) \\ (f,t)<-fs]) $ toList classmap

findClassMembers`` :: [(ClassLocation [TypeVar] ClassContext FunctionName ExtendedType -> Bool)]
		TypeDB -> [(ClassLocation, [TypeVar], ClassContext, FunctionName, ExtendedType)]
findClassMembers`` fs {classmap} = foldr (filter o app5) all_members fs
where
	all_members = [(cl,vs,cc,f,t) \\ (cl,(vs,cc,fs)) <- toList classmap, (f,t) <- fs]

getType :: TypeLocation TypeDB -> Maybe TypeDef
getType loc {typemap} = get loc typemap

putType :: TypeLocation TypeDef TypeDB -> TypeDB
putType tl td db=:{typemap} = {db & typemap = put tl td typemap}

putTypes :: [(TypeLocation, TypeDef)] TypeDB -> TypeDB
putTypes ts db = foldr (\(loc,td) -> putType loc td) db ts

findType :: TypeName TypeDB -> [(TypeLocation, TypeDef)]
findType t db=:{typemap}
	= toList $ filterWithKey (\tl _ -> getName tl == t) typemap
where
	getName (TL _ _ t)     = t
	getName (TL_Builtin t) = t

findType` :: (TypeLocation TypeDef -> Bool) TypeDB
		-> [(TypeLocation, TypeDef)]
findType` f {typemap} = toList $ filterWithKey f typemap

getDerivations :: GenericName TypeDB -> [Type]
getDerivations gen {derivemap} = if (isNothing ts) [] (fromJust ts)
where ts = get gen derivemap

putDerivation :: GenericName Type TypeDB -> TypeDB
putDerivation gen t db=:{derivemap} = {db & derivemap=put gen ts derivemap}
where ts = removeDup [t : getDerivations gen db]

putDerivations :: GenericName [Type] TypeDB -> TypeDB
putDerivations gen ts db = foldr (\t db -> putDerivation gen t db) db ts

putDerivationss :: [(GenericName, [Type])] TypeDB -> TypeDB
putDerivationss ds db = foldr (\(g,ts) db -> putDerivations g ts db) db ds

searchExact :: Type TypeDB -> [(FunctionLocation, ExtendedType)]
searchExact t db = filter ((\(ET t` _)->t==t`) o snd) $ toList db.functionmap

newDb :: TypeDB
newDb = zero

openDb :: *File -> *(Maybe TypeDB, *File)
openDb f
# (data, f) = freadline f
= (fromJSON $ fromString data, f)

saveDb :: TypeDB *File -> *File
saveDb db f = fwrites (toString $ toJSON db) f

app5 f (a,b,c,d,e) :== f a b c d e
