implementation module TypeDB

// Standard libraries
import StdEnv
from Data.Func import $
import Data.Functor
from Data.List import intercalate, groupBy
import Data.Map
import Data.Maybe
from Text import class Text(concat), instance Text String
import Text.JSON

import GenLexOrd

// CleanTypeUnifier
import Type

:: TypeDB
	= { // Base maps
	    functionmap  :: Map Location ExtendedType
	  , macromap     :: Map Location Macro
	  , classmap     :: Map Location ([TypeVar],ClassContext,[(Name, ExtendedType)])
	  , instancemap  :: Map Class [([(Type,String)], [Location])]
	  , typemap      :: Map Location TypeDef
	  , derivemap    :: Map Name [(Type, [Location])]
	  , modulemap    :: Map (Library, Module) ModuleInfo
	    // Derived maps
	  , instancemap` :: Map Name [(Class, [(Type,String)], [Location])]
	  , derivemap`   :: Map Name [(Name, [Location])]
	  }

printersperse :: Bool a [b] -> [String] | print a & print b
printersperse ia a bs = intercalate (print False a) (map (print ia) bs)

(--) infixr 5 :: a b -> [String] | print a & print b
(--) a b = print False a ++ print False b

derive gEq ClassOrGeneric, Location, Type, TypeExtras, Priority,
	ExtendedType, TypeDef, TypeDefRhs, RecordField, Constructor, Kind, Macro
derive JSONEncode ClassOrGeneric, Location, Type, TypeDB, TypeExtras,
	Priority, ExtendedType, TypeDef, TypeDefRhs, RecordField, Constructor,
	Kind, Macro, ModuleInfo
derive JSONDecode ClassOrGeneric, Location, Type, TypeDB, TypeExtras,
	Priority, ExtendedType, TypeDef, TypeDefRhs, RecordField, Constructor,
	Kind, Macro, ModuleInfo

instance zero TypeDB
where
	zero = { functionmap  = newMap
	       , macromap     = newMap
	       , classmap     = newMap
	       , instancemap  = newMap
	       , typemap      = newMap
	       , derivemap    = newMap
	       , modulemap    = newMap
	       , instancemap` = newMap
	       , derivemap`   = newMap
	       }

derive gLexOrd Location, Maybe, ClassOrGeneric, Kind, Type
instance < Location where (<) a b = (a =?= b) === LT
instance < (Maybe a) | gLexOrd{|*|} a where (<) a b = (a =?= b) === LT
instance < Type where (<) a b = (a =?= b) === LT
instance < (a,b,c,d) | gLexOrd{|*|} a & gLexOrd{|*|} b & gLexOrd{|*|} c & gLexOrd{|*|} d
	where (<) a b = (a =?= b) === LT

instance == Location
where
	(==) a b = gEq{|*|} a b

instance zero TypeExtras
where
	zero = { te_priority       = Nothing
	       , te_isconstructor  = False
	       , te_isrecordfield  = False
	       , te_generic_vars   = Nothing
	       , te_representation = Nothing
	       }

instance zero ModuleInfo where zero = {is_core = False}

instance print TypeExtras
where
	print b {te_priority=Just p} = print b p -- " "
	print b {te_generic_vars=Just vars} = printersperse b " " vars -- " "
	print _ _ = []

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
getName (Location _ _ _ _ name) = name
getName (Builtin name)          = name

isBuiltin :: Location -> Bool
isBuiltin (Builtin _) = True
isBuiltin _           = False

functionCount :: TypeDB -> Int
functionCount {functionmap} = mapSize functionmap

macroCount :: TypeDB -> Int
macroCount {macromap} = mapSize macromap

classCount :: TypeDB -> Int
classCount {classmap} = mapSize classmap

instanceCount :: TypeDB -> Int
instanceCount {instancemap} = sum $ map length $ elems instancemap

typeCount :: TypeDB -> Int
typeCount {typemap} = mapSize typemap

deriveCount :: TypeDB -> Int
deriveCount {derivemap} = sum $ map length $ elems derivemap

moduleCount :: TypeDB -> Int
moduleCount {modulemap} = mapSize modulemap


filterLocations :: (Location -> Bool) TypeDB -> TypeDB
filterLocations f db
	= { db
	  & functionmap = filterLoc db.functionmap
	  , macromap    = filterLoc db.macromap
	  , classmap    = filterLoc db.classmap
	  , typemap     = filterLoc db.typemap
	  , instancemap = filtInstLocs <$> db.instancemap
	  , derivemap   = filtInstLocs <$> db.derivemap
	  , modulemap   = filtModules db.modulemap
	  }
where
	filterLoc :: ((Map Location a) -> Map Location a)
	filterLoc = filterWithKey (const o f)

	filtInstLocs :: [(a, [Location])] -> [(a, [Location])]
	filtInstLocs [] = []
	filtInstLocs [(t,ls):rest] = case ls` of
		[] =          filtInstLocs rest
		_  = [(t,ls`):filtInstLocs rest]
	where
		ls` = filter f ls

	filtModules :: ((Map (Library, Module) a) -> Map (Library, Module) a)
	filtModules = filterWithKey (\(l,m) _ -> f (Location l m Nothing Nothing undef))

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

getInstances :: Class TypeDB -> [([(Type,String)], [Location])]
getInstances c {instancemap} = if (isNothing ts) [] (fromJust ts)
where ts = get c instancemap

putInstance :: Class [(Type,String)] Location TypeDB -> TypeDB
putInstance c t l db=:{instancemap}
	= {db & instancemap=put c (update (getInstances c db)) instancemap}
where
	update :: [([(Type,String)], [Location])] -> [([(Type,String)], [Location])]
	update []   = [(t,[l])]
	update [(t`,ls):rest]
	| t` == t   = [(t`, removeDup [l:ls]):rest]
	| otherwise = [(t`,ls):update rest]

putInstances :: [(Class, [(Type,String)], Location)] TypeDB -> TypeDB
putInstances is db = foldr (\(c,ts,l) db -> putInstance c ts l db) db is

getClass :: Location TypeDB -> Maybe ([TypeVar],ClassContext,[(Name,ExtendedType)])
getClass loc {classmap} = get loc classmap

putClass :: Location [TypeVar] ClassContext [(Name, ExtendedType)] TypeDB -> TypeDB
putClass cl tvs cc fs db=:{classmap} = {db & classmap = put cl (tvs,cc,fs) classmap}

putClasses :: [(Location, [TypeVar], ClassContext, [(Name, ExtendedType)])] TypeDB -> TypeDB
putClasses cs db = foldr (\(cl,tvs,cc,fs) db -> putClass cl tvs cc fs db) db cs

findClass :: Class TypeDB -> [(Location, [TypeVar], ClassContext, [(Name, ExtendedType)])]
findClass c {classmap} = map (\(k,(x,y,z))->(k,x,y,z)) results
where results = toList $ filterWithKey (\tl _ -> getName tl == c) classmap

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

getDerivations :: Name TypeDB -> [(Type, [Location])]
getDerivations gen {derivemap} = if (isNothing ts) [] (fromJust ts)
where ts = get gen derivemap

putDerivation :: Name Type Location TypeDB -> TypeDB
putDerivation gen t loc db=:{derivemap} = {db & derivemap=put gen ts derivemap}
where ts = removeDup [(t, [loc]) : getDerivations gen db]

putDerivations :: Name [(Type, Location)] TypeDB -> TypeDB
putDerivations gen ts db = foldr (\(t,l) db -> putDerivation gen t l db) db ts

putDerivationss :: [(Name, [(Type, Location)])] TypeDB -> TypeDB
putDerivationss ds db = foldr (\(g,ts) db -> putDerivations g ts db) db ds

searchExact :: Type TypeDB -> [(Location, ExtendedType)]
searchExact t db = filter ((\(ET t` _)->t==t`) o snd) $ toList db.functionmap

getTypeInstances :: Name TypeDB -> [(Class, [(Type,String)], [Location])]
getTypeInstances n db = case get n db.instancemap` of (Just cs) = cs; _ = []

getTypeDerivations :: Name TypeDB -> [(Name, [Location])]
getTypeDerivations n db = case get n db.derivemap` of (Just gs) = gs; _ = []

getModule :: Library Module TypeDB -> Maybe ModuleInfo
getModule lib mod {modulemap} = get (lib,mod) modulemap

putModule :: Library Module ModuleInfo TypeDB -> TypeDB
putModule lib mod info db = {db & modulemap = put (lib,mod) info db.modulemap}

findModule` :: (Library Module ModuleInfo -> Bool) TypeDB -> [(Library, Module, ModuleInfo)]
findModule` f {modulemap} = map (\((l,m),i) -> (l,m,i)) $ toList $
	filterWithKey (uncurry f) modulemap

newDb :: TypeDB
newDb = zero

openDb :: *File -> *(Maybe TypeDB, *File)
openDb f
# (data, f) = freadline f
= (fromJSON $ fromString data, f)

saveDb :: !TypeDB !*File -> *File
saveDb db f = f <<< (toJSON $ syncDb db)

syncDb :: TypeDB -> TypeDB
syncDb db=:{instancemap,derivemap}
	= { db
	  & instancemap` = insts
	  , derivemap`   = derivs
	  }
where
	insts = fromList $ map (\cs=:[(t,_,_,_):_] -> (t,[(c,ts,ls) \\ (_,c,ls,ts) <- cs])) $
		groupBy (\a b -> fst4 a == fst4 b) $ sort
		[(t,c,ls,ts`)
			\\ (c,ts) <- toList instancemap, (ts`,ls) <- ts, (Type t [],_) <- ts`]
	derivs = fromList $ map (\gs=:[(t,_,_):_] -> (t,[(g,ls) \\ (_,g,ls) <- gs])) $
		groupBy (\a b -> fst3 a == fst3 b) $ sort
		[(t,g,ls) \\ (g,ts) <- toList derivemap, (Type t [],ls) <- ts]

app5 f (a,b,c,d,e) :== f a b c d e
fst4 (a,_,_,_) :== a
