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
            , instancemap :: Map Class Type
            }

(<+) infixr 5 :: a b -> [String] | print a & print b
(<+) a b = print a ++ print b

instance zero TypeDB where zero = {typemap=newMap, instancemap=newMap}
derive gEq ClassOrGeneric, ArrayKind, Strict, SpineStrictness, ListKind,
        FunctionLocation, Type, TypeDB
derive JSONEncode ClassOrGeneric, ArrayKind, Strict, SpineStrictness, ListKind,
        FunctionLocation, Type, TypeDB
derive JSONDecode ClassOrGeneric, ArrayKind, Strict, SpineStrictness, ListKind,
        FunctionLocation, Type, TypeDB

instance < FunctionLocation where (<) (FL a b c) (FL d e f) = (a,b,c) < (d,e,f)
instance print FunctionLocation
where print (FL lib mod fn) = fn <+ " in " <+ mod <+ " in " <+ lib

getType :: FunctionLocation TypeDB -> Maybe Type
getType loc {typemap} = get loc typemap

putType :: FunctionLocation Type TypeDB -> TypeDB
putType fl t tdb=:{typemap} = { tdb & typemap = put fl t typemap }

putTypes :: [(FunctionLocation, Type)] TypeDB -> TypeDB
putTypes ts tdb = foldr (\(loc,t) db=:{typemap} -> {db & typemap=put loc t typemap}) tdb ts

searchExact :: Type TypeDB -> [(FunctionLocation, Type)]
searchExact t db = filter ((==)t o snd) $ toList db.typemap

searchUnifiable :: Type TypeDB -> [(FunctionLocation, Type, [TypeVarAssignment], [TypeVarAssignment])]
searchUnifiable t db = search` $ toList db.typemap
where
    search` :: [(FunctionLocation,Type)] -> [(FunctionLocation,Type,[TypeVarAssignment],[TypeVarAssignment])]
    search` [] = []
    search` [(l,t`):list]
    # tvas = unify t t`
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
