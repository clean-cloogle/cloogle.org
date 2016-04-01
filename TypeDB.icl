implementation module TypeDB

// Standard libraries
import StdEnv
from Data.Func import $
import Data.Map
import Data.Maybe
import Text.JSON

// CleanTypeUnifier
import Type

:: TypeDB = { typemap :: Map FunctionLocation Type
            , classmap :: Map ClassLocation ([TypeVar],[(FunctionName, Type)])
            , instancemap :: Map Class [Type]
            }

(<+) infixr 5 :: a b -> [String] | print a & print b
(<+) a b = print a ++ print b

derive gEq ClassOrGeneric, FunctionLocation, ClassLocation, Type, TypeDB
derive JSONEncode ClassOrGeneric, FunctionLocation, ClassLocation, Type, TypeDB
derive JSONDecode ClassOrGeneric, FunctionLocation, ClassLocation, Type, TypeDB

instance zero TypeDB where zero = { typemap       = newMap
                                  , classmap      = newMap
                                  , instancemap   = newMap
                                  }

instance < FunctionLocation where (<) (FL a b c) (FL d e f) = (a,b,c) < (d,e,f)
instance print FunctionLocation
where print (FL lib mod fn) = fn <+ " in " <+ mod <+ " in " <+ lib

instance < ClassLocation where (<) (CL a b c) (CL d e f) = (a,b,c) < (d,e,f)

getType :: FunctionLocation TypeDB -> Maybe Type
getType loc {typemap} = get loc typemap

putType :: FunctionLocation Type TypeDB -> TypeDB
putType fl t tdb=:{typemap} = { tdb & typemap = put fl t typemap }

putTypes :: [(FunctionLocation, Type)] TypeDB -> TypeDB
putTypes ts tdb = foldr (\(loc,t) db -> putType loc t db) tdb ts

findType :: FunctionName TypeDB -> [(FunctionLocation, Type)]
findType f db=:{typemap} = toList $ filterWithKey (\(FL _ _ f`) _->f==f`) typemap

findType` :: (FunctionLocation Type -> Bool) TypeDB -> [(FunctionLocation, Type)]
findType` f {typemap} = toList $ filterWithKey f typemap

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

getClass :: ClassLocation TypeDB -> Maybe ([TypeVar],[(FunctionName,Type)])
getClass loc {classmap} = get loc classmap

putClass :: ClassLocation [TypeVar] [(FunctionName, Type)] TypeDB -> TypeDB
putClass cl tvs fs db=:{classmap} = {db & classmap = put cl (tvs,fs) classmap}

putClasses :: [(ClassLocation, [TypeVar], [(FunctionName, Type)])] TypeDB -> TypeDB
putClasses cs db = foldr (\(cl,tvs,fs) db -> putClass cl tvs fs db) db cs

findClass :: Class TypeDB -> [(ClassLocation, [TypeVar], [(FunctionName, Type)])]
findClass c {classmap} = map (\(k,(x,y))->(k,x,y)) results
where
    results = toList $ filterWithKey (\(CL _ _ c`) _->c==c`) classmap

findClass` :: (ClassLocation [TypeVar] [(FunctionName,Type)] -> Bool) TypeDB
        -> [(ClassLocation, [TypeVar], [(FunctionName, Type)])]
findClass` f {classmap} = map (\(k,(x,y))->(k,x,y)) results
where
    results = toList $ filterWithKey (\cl (vs,fs)->f cl vs fs) classmap

searchExact :: Type TypeDB -> [(FunctionLocation, Type)]
searchExact t db = filter ((==)t o snd) $ toList db.typemap

searchUnifiable :: Type TypeDB 
        -> [(FunctionLocation, Type, [TVAssignment], [TVAssignment])]
searchUnifiable t db = search` $ toList db.typemap
where
    search` :: [(FunctionLocation,Type)] 
            -> [(FunctionLocation,Type,[TVAssignment],[TVAssignment])]
    search` [] = []
    search` [(l,t`):list]
    # tvas = unify [] t t`
    | isNothing tvas = search` list
    # (tvas1,tvas2) = fromJust tvas
    = [(l,t`,tvas1,tvas2):search` list]

newDb :: TypeDB
newDb = zero

openDb :: *File -> *(Maybe TypeDB, *File)
openDb f
# (data, f) = freadline f
= (fromJSON $ fromString data, f)

saveDb :: TypeDB *File -> *File
saveDb db f = fwrites (toString $ toJSON db) f

db = { zero & typemap = put (FL "a" "b" "somefunc") (Var "x") newMap }

