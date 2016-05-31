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
            , classmap :: Map ClassLocation ([TypeVar],[(FunctionName, ExtendedType)])
            , instancemap :: Map Class [Type]
            }

(<+) infixr 5 :: a b -> [String] | print a & print b
(<+) a b = print a ++ print b

derive gEq ClassOrGeneric, FunctionLocation, ClassLocation, Type, TypeDB,
	TypeExtras, TE_Priority, ExtendedType
derive JSONEncode ClassOrGeneric, FunctionLocation, ClassLocation, Type,
	TypeDB, TypeExtras, TE_Priority, ExtendedType
derive JSONDecode ClassOrGeneric, FunctionLocation, ClassLocation, Type,
	TypeDB, TypeExtras, TE_Priority, ExtendedType

instance zero TypeDB where zero = { functionmap       = newMap
                                  , classmap      = newMap
                                  , instancemap   = newMap
                                  }

instance < FunctionLocation where (<) (FL a b c) (FL d e f) = (a,b,c) < (d,e,f)
instance print FunctionLocation
where print (FL lib mod fn) = fn <+ " in " <+ mod <+ " in " <+ lib

instance < ClassLocation where (<) (CL a b c) (CL d e f) = (a,b,c) < (d,e,f)

instance print TE_Priority
where
	print (LeftAssoc i)  = "infixl " <+ i
	print (RightAssoc i) = "infixr " <+ i
	print (NoAssoc i)    = "infix " <+ i

getType :: FunctionLocation TypeDB -> Maybe ExtendedType
getType loc {functionmap} = get loc functionmap

putType :: FunctionLocation ExtendedType TypeDB -> TypeDB
putType fl t tdb=:{functionmap} = { tdb & functionmap = put fl t functionmap }

putTypes :: [(FunctionLocation, ExtendedType)] TypeDB -> TypeDB
putTypes ts tdb = foldr (\(loc,t) db -> putType loc t db) tdb ts

findType :: FunctionName TypeDB -> [(FunctionLocation, ExtendedType)]
findType f db=:{functionmap} = toList $ filterWithKey (\(FL _ _ f`) _->f==f`) functionmap

findType` :: (FunctionLocation ExtendedType -> Bool) TypeDB
		-> [(FunctionLocation, ExtendedType)]
findType` f {functionmap} = toList $ filterWithKey f functionmap

findType`` :: [(FunctionLocation ExtendedType -> Bool)] TypeDB
		-> [(FunctionLocation, ExtendedType)]
findType`` fs {functionmap} = toList $ foldr filterWithKey functionmap fs

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

searchExact :: Type TypeDB -> [(FunctionLocation, ExtendedType)]
searchExact t db = filter ((\(ET t` _)->t==t`) o snd) $ toList db.functionmap

searchUnifiable :: Type TypeDB
        -> [(FunctionLocation, ExtendedType, [TVAssignment], [TVAssignment])]
searchUnifiable t db = search` $ toList db.functionmap
where
    search` :: [(FunctionLocation,ExtendedType)] 
            -> [(FunctionLocation,ExtendedType,[TVAssignment],[TVAssignment])]
    search` [] = []
    search` [(l,ET t` tes):list]
    # tvas = unify [] t t`
    | isNothing tvas = search` list
    # (tvas1,tvas2) = fromJust tvas
    = [(l,ET t` tes,tvas1,tvas2):search` list]

newDb :: TypeDB
newDb = zero

openDb :: *File -> *(Maybe TypeDB, *File)
openDb f
# (data, f) = freadline f
= (fromJSON $ fromString data, f)

saveDb :: TypeDB *File -> *File
saveDb db f = fwrites (toString $ toJSON db) f

app4 f (a,b,c,d) :== f a b c d

