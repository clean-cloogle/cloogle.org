implementation module BuiltinSyntax

import StdInt
import StdOverloaded
import StdString

import Data.Maybe
import Text

import Cloogle
import CloogleDB

builtin_syntax :: [([String], SyntaxEntry)]
builtin_syntax =
	[ bs_case
	, bs_import
	, bs_infix
	, bs_let
	, bs_let_before
	, bs_module
	, bs_otherwise
	, bs_strict
	, bs_where_class
	, bs_where_instance
	, bs_where_local
	, bs_with
	]

CLR :: Int String String -> SyntaxDocLocation
CLR f sec h = CleanLangReport
	{ clr_version = v
	, clr_file    = "CleanRep." + v + "_" <+ f <+ ".htm"
	, clr_section = sec
	, clr_heading = h
	}
where v = "2.2"

EX :: String String -> SyntaxExample 
EX t c = {example=c, cleanjs_type=t, cleanjs_start=Nothing}
EXs :: String String String -> SyntaxExample
EXs t s c = {example=c, cleanjs_type=t, cleanjs_start=Just s}

bs_case = (["case", "of", "case of"],
	{ syntax_title        = "case expression"
	, syntax_code         = ["case ... of ..."]
	, syntax_description  = "Pattern match on an expression and do something depending on the alternative of the matching pattern."
	, syntax_doc_location = [CLR 5 "3.4.2" "_Toc311798001"]
	, syntax_examples     =
		[ EXs "Function" "macro" "isJust m = case m of\n\tJust _ -> True\n\t_      -> False"
		]
	})

bs_import = (["import", "from", "qualified", "as"],
	{ syntax_title        = "imports"
	, syntax_code         = ["import [qualified] ... [as ...]", "from ... import ..."]
	, syntax_description  = "Imports code from other modules. With the `from` keyword, one can achieve more granularity. In case of name clashes, `qualified` can be used (undocumented)."
	, syntax_doc_location = [CLR 4 "2.5" "_Toc311797991"]
	, syntax_examples     =
		[ EX "Function" "import ..."
		, EX "Function" "import StdEnv                  // Import all code from the StdEnv definition module"
		, EX "Function" "from StdFunc import o          // Import only the o function from StdFunc"
		, EX "Function" "import qualified Data.Map as M // Import Data.Map such that functions are available as e.g. 'M'.get."
		]
	})

bs_infix = (["infix", "infixl", "infixr"],
	{ syntax_title        = "infix operator"
	, syntax_code         = ["infix[l,r]"]
	, syntax_description  = "Defines a function with arity 2 that can be used in infix position. `n` determines the precedence. `infixl` and `infixr` indicate associativity."
	, syntax_doc_location = [CLR 5 "3.7.2" "_Toc311798011"]
	, syntax_examples     =
		[ EX  "Function"         "(...) infix n :: ..."
		, EX  "Function"         "(bitor) infixl 6 :: !Int !Int -> Int // Left-associative infix function with precedence 6"
		, EXs "Function" "macro" "(o) infixr 9                         // Infix macro\n(o) f g :== \x -> f (g x)"
		, EX  "TypeDef"          ":: MyType = (:+:) infixl 6 Int Int   // Infix data constructor, can be used as (5 :+: 10)"
		]
	})

bs_let = (["let", "in", "let in"],
	{ syntax_title        = "let expression"
	, syntax_code         = ["let ... in ..."]
	, syntax_description  = "An expression that introduces new scope."
	, syntax_doc_location = [CLR 5 "3.5.1" "_Toc31178003"]
	, syntax_examples     =
		[ EXs "Function" "macro"    "fac n = let fs = [1:1:[(fs!!(i-1)) + (fs!!(i-2)) \\ i <- [2..]]] in fs !! n"
		, EXs "Function" "macrorhs" "let // Multi-line let expressions\n\tfunction args = body\n\tselector = expr\n\t// ...\nin expression"
		]
	})
bs_let_before = (["#", "#!"],
	{ syntax_title        = "let before"
	, syntax_code         = ["#  ... = ...", "#! ... = ..."]
	, syntax_description  = "A let expression that can be defined before a guard or function body, which eases the syntax of sequential actions."
	, syntax_doc_location = [CLR 5 "3.5.4" "_Toc311798006"]
	, syntax_examples     =
		[ EX "Function" "readchars :: *File -> *([Char], *File)\nreadchars f\n# (ok,c,f) = freadc file\n| not ok   = ([], f)\n# (cs,f)   = readchars f\n= ([c:cs], f)"
		]
	})

bs_module = (["module", "definition", "implementation", "system", "definition module", "implementation module", "system module"],
	{ syntax_title        = "module heading"
	, syntax_code         = ["[definition,implementation,system] module ..."]
	, syntax_description  = "The heading of a Clean file. Definition modules describe what things are exported (dcl files), implementation modules how they are implemented (icl files)."
	, syntax_doc_location = [CLR 4 "2.2" "_Toc311797983"]
	, syntax_examples     =
		[ EX "Function" "definition module ..."
		, EX "Function" "definition module StdList     // Exported definitions of list functions"
		, EX "Function" "implementation module StdList // The implementations of the functions"
		, EX "Function" "module test                   // An implementation module without corresponding dcl"
		, EX "Function" "system module StdInt          // The definitions of a module that contains foreign code (see section 2.6 of the language report)"
		]
	})

bs_otherwise = (["otherwise"],
	{ syntax_title        = "otherwise"
	, syntax_code         = ["otherwise"]
	, syntax_description  = "The (optional) last alternative in a guard. It caches all other cases, and makes sure your program does not crash if none of the cases matches."
	, syntax_doc_location = [CLR 5 "3.3" "_Toc311797998"]
	, syntax_examples     =
		[ EXs "Function" "macrorhs" "| otherwise = ..."
		, EXs "Function" "macro"    "sign :: !Int -> Int\nsign n\n| n  < 0    = -1 // Negative number\n| n == 0    =  0 // Zero\n| otherwise =  1 // Must be positive"
		]
	})

bs_strict = (["strict", "!"],
	{ syntax_title        = "strictness annotation"
	, syntax_code         = ["!"]
	, syntax_description  = "Override the lazy evaluation strategy: the argument must be evaluated to head normal form before the function is entered."
	, syntax_doc_location = [CLR 5 "3.7.5" "_Toc311798014", CLR 12 "10" "_Toc311798103"]
	, syntax_examples     = [EX "Function" "acker :: !Int !Int -> Int"]
	})

bs_where_class = (["where"],
	{ syntax_title        = "where"
	, syntax_code         = ["where"]
	, syntax_description  = "Introduces the members of a class definition."
	, syntax_doc_location = [CLR 8 "6.1"   "_Toc311798056"]
	, syntax_examples     = [EX "ClassDef" "class Arith a        // Class definition\nwhere\n\t(+) infixl 6 :: a a -> a\n\t(-) infixl 6 :: a a -> a"] // TODO highlighting
	})
bs_where_instance = (["where"],
	{ syntax_title        = "where"
	, syntax_code         = ["where"]
	, syntax_description  = "Introduces the implementation of an instance."
	, syntax_doc_location = [CLR 8 "6.1"   "_Toc311798056"]
	, syntax_examples     = [EX "Function" "instance Arith Int   // Instance definition\nwhere\n\t(+) x y = // ...\n\t(-) x y = // ..."]
	})
bs_where_local = (["where"],
	{ syntax_title        = "where"
	, syntax_code         = ["where"]
	, syntax_description  = "Introduces local definitions."
	, syntax_doc_location = [CLR 5 "3.5.2" "_Toc311798004"]
	, syntax_examples     = [EXs "Function" "macro" "primes = sieve [2..] // Local definitions\nwhere\n\tsieve [pr:r] = [pr:sieve (filter pr r)]"]
	})

bs_with = (["with"],
	{ syntax_title        = "with"
	, syntax_code         = ["with"]
	, syntax_description  = "Introduces guard-local definitions."
	, syntax_doc_location = [CLR 5 "3.5.3" "_Toc311798005"]
	, syntax_examples     = [EXs "Function" "macro" "f x y\n| guard1 = alt1\n\twith local = expr1\n| guard2 = alt2\n\twith local = expr2"]
	})
