module search

import TypeDB
import Type

import StdArray, StdBool, StdFile, StdList, StdOrdList, StdMisc, StdString
import Data.Maybe
import Text

Start w
# (ok, f, w) = fopen "types.db" FReadText w
| not ok = abort "Couldn't open types.db for reading\n"
# (db, f) = openDb f
| isNothing db = abort "types.db does not contain a TypeDB\n"
# db = fromJust db
# types = searchUnifiable (Func [Var "a"] (Type "Int" []) []) db
# types = sortBy (\(_,_,a,b)(_,_,x,y)->length (a++b) < length (x++y)) types
# (io, w) = stdio w
# io = fwrites (concat (join "\n" [alignl 42 (concat (print t)) <+ "\t" <+ n \\ (FL _ _ n, t, _, _) <- types]) +++ "\n") io
# (ok, w) = fclose io w
= w

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

