implementation module TypeDB

// Standard libraries
import StdEnv
from Data.Func import $
from Data.List import intercalate
import Data.Map
import Data.Maybe
from Text import class Text(concat), instance Text String
import Text.JSON

// CleanTypeUnifier
import Type

:: TypeDB
	= { functionmap :: Map Location ExtendedType
	  , macromap    :: Map Location Macro
	  , classmap    :: Map Location ([TypeVar],ClassContext,[(Name, ExtendedType)])
	  , instancemap :: Map Class [Type]
	  , typemap     :: Map Location TypeDef
	  , derivemap   :: Map Name [Type]
	  }

printersperse :: Bool a [b] -> [String] | print a & print b
printersperse ia a bs = intercalate (print False a) (map (print ia) bs)

(--) infixr 5 :: a b -> [String] | print a & print b
(--) a b = print False a ++ print False b

derive gEq ClassOrGeneric, Location, Type, TypeDB, TypeExtras, TE_Priority,
	ExtendedType, TypeDef, TypeDefRhs, RecordField, Constructor, Kind, Macro
derive JSONEncode ClassOrGeneric, Location, Type, TypeDB, TypeExtras,
	TE_Priority, ExtendedType, TypeDef, TypeDefRhs, RecordField, Constructor,
	Kind, Macro
derive JSONDecode ClassOrGeneric, Location, Type, TypeDB, TypeExtras,
	TE_Priority, ExtendedType, TypeDef, TypeDefRhs, RecordField, Constructor,
	Kind, Macro

instance zero TypeDB
where
	zero = { functionmap   = newMap
	       , macromap      = newMap
	       , classmap      = newMap
	       , instancemap   = newMap
	       , typemap       = newMap
	       , derivemap     = newMap
	       }

instance < (Maybe a) | < a
where
	(<) (Just a) (Just b) = a < b
	(<) (Just _) Nothing  = True
	(<) Nothing  _        = False

instance < Location
where
	(<) (Location a b c d) (Location e f g h) = ((a,b),(c,d)) < ((e,f),(g,h))
	(<) (Location _ _ _ _) (Builtin _)        = True
	(<) (Builtin _)        (Location _ _ _ _) = False
	(<) (Builtin a)        (Builtin b)        = a < b

instance zero TypeExtras
where
	zero = { te_priority      = Nothing
	       , te_isconstructor = False
	       , te_isrecordfield = False
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

instance print (Name, ExtendedType)
where
	print _ (f, (ET t e))
		= gen -- fname -- " " -- e -- " :: " -- t
	where
		gen = if (isJust e.te_generic_vars) "generic " ""
		fname
		| isJust e.te_priority = concat ("(" -- f -- ")")
		| e.te_isrecordfield   = "." +++ f
		| otherwise            = f

getName :: Location -> Name
getName (Location _ _ _ name) = name
getName (Builtin name)        = name

filterLocations :: (Location -> Bool) TypeDB -> TypeDB
filterLocations f db
	= { db
	  & functionmap = filterLoc db.functionmap
	  , macromap    = filterLoc db.macromap
	  , classmap    = filterLoc db.classmap
	  , typemap     = filterLoc db.typemap
	  }
where
	filterLoc :: ((Map Location a) -> Map Location a)
	filterLoc = filterWithKey (const o f)

getFunction :: Location TypeDB -> Maybe ExtendedType
getFunction loc {functionmap} = get loc functionmap

putFunction :: Location ExtendedType TypeDB -> TypeDB
putFunction fl t tdb=:{functionmap} = { tdb & functionmap = put fl t functionmap }

putFunctions :: [(Location, ExtendedType)] TypeDB -> TypeDB
putFunctions ts tdb = foldr (\(loc,t) db -> putFunction loc t db) tdb ts

findFunction :: Name TypeDB -> [(Location, ExtendedType)]
findFunction f db=:{functionmap}
	= toList $ filterWithKey (\fl _-> f == getName fl) functionmap

findFunction` :: (Location ExtendedType -> Bool) TypeDB
	-> [(Location, ExtendedType)]
findFunction` f {functionmap} = toList $ filterWithKey f functionmap

findFunction`` :: [(Location ExtendedType -> Bool)] TypeDB
	-> [(Location, ExtendedType)]
findFunction`` fs {functionmap} = toList $ foldr filterWithKey functionmap fs

getMacro :: Location TypeDB -> Maybe Macro
getMacro loc {macromap} = get loc macromap

putMacro :: Location Macro TypeDB -> TypeDB
putMacro ml m db=:{macromap} = { db & macromap = put ml m macromap }

putMacros :: [(Location, Macro)] TypeDB -> TypeDB
putMacros ms db = foldr (\(loc,m) db -> putMacro loc m db) db ms

findMacro` :: (Location Macro -> Bool) TypeDB -> [(Location, Macro)]
findMacro` f {macromap} = toList $ filterWithKey f macromap

findMacro`` :: [(Location Macro -> Bool)] TypeDB -> [(Location, Macro)]
findMacro`` fs {macromap} = toList $ foldr filterWithKey macromap fs

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

getClass :: Location TypeDB -> Maybe ([TypeVar],ClassContext,[(Name,ExtendedType)])
getClass loc {classmap} = get loc classmap

putClass :: Location [TypeVar] ClassContext [(Name, ExtendedType)] TypeDB -> TypeDB
putClass cl tvs cc fs db=:{classmap} = {db & classmap = put cl (tvs,cc,fs) classmap}

putClasses :: [(Location, [TypeVar], ClassContext, [(Name, ExtendedType)])] TypeDB -> TypeDB
putClasses cs db = foldr (\(cl,tvs,cc,fs) db -> putClass cl tvs cc fs db) db cs

findClass :: Class TypeDB -> [(Location, [TypeVar], ClassContext, [(Name, ExtendedType)])]
findClass c {classmap} = map (\(k,(x,y,z))->(k,x,y,z)) results
where results = toList $ filterWithKey (\(Location _ _ _ c`) _->c==c`) classmap

findClass` :: (Location [TypeVar] ClassContext [(Name,ExtendedType)] -> Bool) TypeDB
		-> [(Location, [TypeVar], ClassContext, [(Name,ExtendedType)])]
findClass` f {classmap} = map (\(k,(x,y,z))->(k,x,y,z)) results
where results = toList $ filterWithKey (\cl (vs,cc,fs)->f cl vs cc fs) classmap

findClass`` :: [(Location [TypeVar] ClassContext [(Name,ExtendedType)] -> Bool)] TypeDB
		-> [(Location, [TypeVar], ClassContext, [(Name, ExtendedType)])]
findClass`` fs {classmap} = map (\(k,(x,y,z)) -> (k,x,y,z)) $ toList
	$ foldr (\f -> filterWithKey (\cl (vs,cc,fs) -> f cl vs cc fs)) classmap fs

findClassMembers` :: (Location [TypeVar] ClassContext Name ExtendedType -> Bool) TypeDB
		-> [(Location, [TypeVar], ClassContext, Name, ExtendedType)]
findClassMembers` f {classmap} = filter (app5 f) $ flatten members
where
	members = map (\(cl,(vs,cc,fs))->[(cl,vs,cc,f,t) \\ (f,t)<-fs]) $ toList classmap

findClassMembers`` :: [(Location [TypeVar] ClassContext Name ExtendedType -> Bool)]
		TypeDB -> [(Location, [TypeVar], ClassContext, Name, ExtendedType)]
findClassMembers`` fs {classmap} = foldr (filter o app5) all_members fs
where
	all_members = [(cl,vs,cc,f,t) \\ (cl,(vs,cc,fs)) <- toList classmap, (f,t) <- fs]

getType :: Location TypeDB -> Maybe TypeDef
getType loc {typemap} = get loc typemap

putType :: Location TypeDef TypeDB -> TypeDB
putType tl td db=:{typemap} = {db & typemap = put tl td typemap}

putTypes :: [(Location, TypeDef)] TypeDB -> TypeDB
putTypes ts db = foldr (\(loc,td) -> putType loc td) db ts

findType :: Name TypeDB -> [(Location, TypeDef)]
findType t db=:{typemap}
	= toList $ filterWithKey (\tl _ -> getName tl == t) typemap

findType` :: (Location TypeDef -> Bool) TypeDB
		-> [(Location, TypeDef)]
findType` f {typemap} = toList $ filterWithKey f typemap

findType`` :: [(Location TypeDef -> Bool)] TypeDB -> [(Location, TypeDef)]
findType`` fs {typemap} = toList $ foldr filterWithKey typemap fs

getDerivations :: Name TypeDB -> [Type]
getDerivations gen {derivemap} = if (isNothing ts) [] (fromJust ts)
where ts = get gen derivemap

putDerivation :: Name Type TypeDB -> TypeDB
putDerivation gen t db=:{derivemap} = {db & derivemap=put gen ts derivemap}
where ts = removeDup [t : getDerivations gen db]

putDerivations :: Name [Type] TypeDB -> TypeDB
putDerivations gen ts db = foldr (\t db -> putDerivation gen t db) db ts

putDerivationss :: [(Name, [Type])] TypeDB -> TypeDB
putDerivationss ds db = foldr (\(g,ts) db -> putDerivations g ts db) db ds

searchExact :: Type TypeDB -> [(Location, ExtendedType)]
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
