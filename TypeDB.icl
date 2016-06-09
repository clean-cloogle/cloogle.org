implementation module TypeDB

// Standard libraries
import StdEnv
from Data.Func import $
import Data.Map
import Data.Maybe
import Text.JSON

// CleanTypeUnifier
import Type

:: TypeDB = { functionmap :: Map FunctionLocation ExtendedType
            , classmap    :: Map ClassLocation ([TypeVar],[(FunctionName, ExtendedType)])
            , instancemap :: Map Class [Type]
            , typemap     :: Map TypeLocation TypeDef
            }

(--) infixr 5 :: a b -> [String] | print a & print b
(--) a b = print False a ++ print False b

derive gEq ClassOrGeneric, FunctionLocation, ClassLocation, Type, TypeDB,
	TypeExtras, TE_Priority, ExtendedType, TypeDef, TypeLocation, TypeDefRhs,
	RecordField, Constructor
derive JSONEncode ClassOrGeneric, FunctionLocation, ClassLocation, Type,
	TypeDB, TypeExtras, TE_Priority, ExtendedType, TypeDef, TypeLocation,
	TypeDefRhs, RecordField, Constructor
derive JSONDecode ClassOrGeneric, FunctionLocation, ClassLocation, Type,
	TypeDB, TypeExtras, TE_Priority, ExtendedType, TypeDef, TypeLocation,
	TypeDefRhs, RecordField, Constructor

instance zero TypeDB where zero = { functionmap   = newMap
                                  , classmap      = newMap
                                  , instancemap   = newMap
                                  , typemap       = newMap
                                  }

instance < FunctionLocation where (<) (FL a b c) (FL d e f) = (a,b,c) < (d,e,f)
instance print FunctionLocation
where print _ (FL lib mod fn) = fn -- " in " -- mod -- " in " -- lib

instance < ClassLocation where (<) (CL a b c) (CL d e f) = (a,b,c) < (d,e,f)

instance < TypeLocation where (<) (TL a b c) (TL d e f) = (a,b,c) < (d,e,f)

instance print TE_Priority
where
	print _ (LeftAssoc i)  = "infixl " -- i
	print _ (RightAssoc i) = "infixr " -- i
	print _ (NoAssoc i)    = "infix " -- i

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

getInstances :: Class TypeDB -> [Type]
getInstances c {instancemap} = if (isNothing ts) [] (fromJust ts)
where ts = get c instancemap

putInstance :: Class Type TypeDB -> TypeDB
putInstance c t db=:{instancemap} = {db & instancemap=put c ts instancemap}
where
    ts = removeDup [t : getInstances  c db]

putInstances :: Class [Type] TypeDB -> TypeDB
putInstances c ts db = foldr (\t db -> putInstance c t db) db ts

putInstancess :: [(Class, [Type])] TypeDB -> TypeDB
putInstancess is db = foldr (\(c,ts) db -> putInstances c ts db) db is

getClass :: ClassLocation TypeDB -> Maybe ([TypeVar],[(FunctionName,ExtendedType)])
getClass loc {classmap} = get loc classmap

putClass :: ClassLocation [TypeVar] [(FunctionName, ExtendedType)] TypeDB -> TypeDB
putClass cl tvs fs db=:{classmap} = {db & classmap = put cl (tvs,fs) classmap}

putClasses :: [(ClassLocation, [TypeVar], [(FunctionName, ExtendedType)])] TypeDB -> TypeDB
putClasses cs db = foldr (\(cl,tvs,fs) db -> putClass cl tvs fs db) db cs

findClass :: Class TypeDB -> [(ClassLocation, [TypeVar], [(FunctionName, ExtendedType)])]
findClass c {classmap} = map (\(k,(x,y))->(k,x,y)) results
where results = toList $ filterWithKey (\(CL _ _ c`) _->c==c`) classmap

findClass` :: (ClassLocation [TypeVar] [(FunctionName,ExtendedType)] -> Bool) TypeDB
		-> [(ClassLocation, [TypeVar], [(FunctionName,ExtendedType)])]
findClass` f {classmap} = map (\(k,(x,y))->(k,x,y)) results
where results = toList $ filterWithKey (\cl (vs,fs)->f cl vs fs) classmap

findClassMembers` :: (ClassLocation [TypeVar] FunctionName ExtendedType -> Bool) TypeDB
		-> [(ClassLocation, [TypeVar], FunctionName, ExtendedType)]
findClassMembers` f {classmap} = filter (app4 f) $ flatten members
where
	members = map (\(cl,(vs,fs))->[(cl,vs,f,t) \\ (f,t)<-fs]) $ toList classmap

findClassMembers`` :: [(ClassLocation [TypeVar] FunctionName ExtendedType -> Bool)]
		TypeDB -> [(ClassLocation, [TypeVar], FunctionName, ExtendedType)]
findClassMembers`` fs {classmap} = foldr (filter o app4) all_members fs
where
	all_members = [(cl,vs,f,t) \\ (cl,(vs,fs)) <- toList classmap, (f,t) <- fs]

getType :: TypeLocation TypeDB -> Maybe TypeDef
getType loc {typemap} = get loc typemap

putType :: TypeLocation TypeDef TypeDB -> TypeDB
putType tl td db=:{typemap} = {db & typemap = put tl td typemap}

putTypes :: [(TypeLocation, TypeDef)] TypeDB -> TypeDB
putTypes ts db = foldr (\(loc,td) -> putType loc td) db ts

findType :: TypeName TypeDB -> [(TypeLocation, TypeDef)]
findType t db=:{typemap}
	= toList $ filterWithKey (\(TL _ _ t`) _->t==t`) typemap

findType` :: (TypeLocation TypeDef -> Bool) TypeDB
		-> [(TypeLocation, TypeDef)]
findType` f {typemap} = toList $ filterWithKey f typemap

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

app4 f (a,b,c,d) :== f a b c d
