module search

import TypeDB
import Type

import Levenshtein

import StdArray, StdBool, StdFile, StdList, StdOrdList, StdMisc, StdString,
    StdFunc, StdTuple
import Data.Maybe
from Data.Func import $
import Text
import System.CommandLine

Start w
# (io, w) = stdio w
# (cmdline, w) = getCommandLine w
| length cmdline <> 3 = help io w
# [_,cmd,search:_] = cmdline
# (db, io) = openDb io
| isNothing db = abort "stdin does not have a TypeDB\n"
# db = fromJust db
| cmd == "unify"
    # type = parseType (fromString search)
    | isNothing type
        # io = io <<< "Couldn't parse type: " <<< search <<< "\n"
        = snd $ fclose io w
    # type = fromJust type
    # io = io <<< "Searching for type: " <<< concat (print type) <<< "\n"
    # io = uniSearch type db io
    = snd $ fclose io w
| cmd == "instance"
    # io = io <<< "Searching for instance: " <<< search <<< "\n"
    # io = instanceSearch search db io
    = snd $ fclose io w
| cmd == "class"
    # io = io <<< "Searching for class: " <<< search <<< "\n"
    # io = classSearch search db io
    = snd $ fclose io w
| cmd == "func"
    # io = io <<< "Searching for function: " <<< search <<< "\n"
    # io = funcSearch search db io
    = snd $ fclose io w
| cmd == "name"
    # io = io <<< "Searching for name: " <<< search <<< "\n"
    # io = nameSearch search db io
    = snd $ fclose io w
= help io w
where
    help :: *File *World -> *World
    help io w
    # io = io <<< "Usage: ./search <cmd> <search-string>\n\n"
    # io = io <<< "  <cmd> = unify | instance | class | func | name\n"
    # (ok, w) = fclose io w
    = w

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
    # fs = levsort (\(FL _ _ f,_)->f) func fs
    # cs = findClass` (\_ _ fs -> any (\f -> indexOf func f <> -1) (map fst fs)) db
    # cfs = [(loc,tvs,f,t) \\ (loc, tvs, fs) <- cs, (f, t) <- fs | indexOf func f <> -1]
    # cfs = levsort (\(_,_,f,_)->f) func cfs
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
    # cs = levsort (\(CL _ _ c,_,_)->c) cls cs
    = fwrites (concat (join "\n"
        [c <+ " " <+ join " " vs <+ " in " <+ m <+ " in " <+ l <+ ":\n    "
            <+ join "\n    "
                [alignl 12 (concat (print f)) <+ "\t" <+ t \\ (f,t) <- fs]
        \\ (CL l m c,vs,fs) <- cs]) +++ "\n") f

    levsort :: (a -> b) b -> [a] -> [a] | levenshtein b
    levsort f e = sortBy (\a b -> levenshtein e (f a) < levenshtein e (f b))

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

