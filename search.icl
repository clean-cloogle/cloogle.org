module search

import TypeDB
import Type

import Levenshtein

import StdArray, StdBool, StdFile, StdList, StdOrdList, StdMisc, StdString,
    StdFunc, StdTuple
import Data.Maybe
import Text

Start w
# (io, w) = stdio w
# (db, io) = openDb io
| isNothing db = abort "stdin does not have a TypeDB\n"
# db = fromJust db
//# io = uniSearch (Func [Var "a"] (Type "Int" []) []) db io
//# io = instanceSearch "<" db io
# io = classSearch "to" db io
//# io = funcSearch "length" db io
//# io = nameSearch "map" db io
# (ok, w) = fclose io w
= w
where
    nameSearch :: FunctionName TypeDB *File -> *File
    nameSearch n db f
    # f = fwrites "=== FUNCTIONS ===\n\n" f
    # f = funcSearch n db f
    # f = fwrites "\n=== CLASSES ===\n\n" f
    # f = classSearch n db f
    = f

    funcSearch :: FunctionName TypeDB *File -> *File
    funcSearch func db f
    # fs = findType` (\(FL _ _ f) _->indexOf func f <> -1) db
    # fs = sortBy (levsort (\(FL _ _ f,_)->f) func) fs
    # cs = findClass` (\_ _ fs -> any (\f -> indexOf func f <> -1) (map fst fs)) db
    # cfs = [(loc,tvs,f,t) \\ (loc, tvs, fs) <- cs, (f, t) <- fs | indexOf func f <> -1]
    # cfs = sortBy (levsort (\(_,_,f,_)->f) func) cfs
    # f = fwrites (concat (join "\n"
        [f <+ " in " <+ m <+ " in " <+ l <+ ":\t" <+ t \\ (FL l m f,t) <- fs]) +++ "\n\n") f
    = fwrites (concat (join "\n"
        [f <+ " from class " <+ c <+ " in " <+ m <+ " in " <+ l <+ ":\t" <+ t \\ (CL l m c,tvs,f,t) <- cfs]) +++ "\n") f

    uniSearch :: Type TypeDB *File -> *File
    uniSearch t db f
    # ts = searchUnifiable t db
    # ts = sortBy (\(_,_,a,b)(_,_,x,y)->length (a++b) < length (x++y)) ts
    = fwrites (concat (join "\n" [alignl 42 (concat (print t)) <+ "\t" <+ n \\ (FL _ _ n, t, _, _) <- ts]) +++ "\n") f

    instanceSearch :: Class TypeDB *File -> *File
    instanceSearch cls db f 
    # ts = getInstances cls db
    = fwrites (concat (join "\n" ts) +++ "\n") f

    classSearch :: Class TypeDB *File -> *File
    classSearch cls db f
    # cs = findClass` (\(CL _ _ c) _ fs -> indexOf cls c <> -1) db
    # cs = sortBy (levsort (\(CL _ _ c,_,_)->c) cls) cs
    = fwrites (concat (join "\n"
        [c <+ " " <+ join " " vs <+ " in " <+ m <+ " in " <+ l <+ ":\n    "
            <+ join "\n    "
                [alignl 12 (concat (print f)) <+ "\t" <+ t \\ (f,t) <- fs]
        \\ (CL l m c,vs,fs) <- cs]) +++ "\n") f

    levsort :: (a -> b) b a a -> Bool | levenshtein b
    levsort f e a b = levenshtein e (f a) < levenshtein e (f b)

(<+) infixr 5 :: a b -> [String] | print a & print b
(<+) a b = print a ++ print b

join :: a [b] -> [String] | print a & print b
join _ [] = []
join a [b:[]] = print b
join a [b:bs] = b <+ a <+ join a bs

alignl :: Int a -> [String] | print a
alignl i s
# s = print s
# len = sum (map size s)
| len >= i = s
| otherwise = s ++ [{' ' \\ i <- [0..i-len]}]

