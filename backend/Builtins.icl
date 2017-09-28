implementation module Builtins

import StdBool
import StdEnum
import StdInt
import StdList
import StdOverloaded
import StdString

from Data.Func import $
import Data.Maybe
import Text

import Type

import Cloogle
import Doc
import CloogleDB

builtin_functions :: [(Location, FunctionEntry)]
builtin_functions
	= [ ( Builtin "if"
	    , {zero & fe_type=Just $ Func [Type "Bool" [], Var "a", Var "a"] (Var "a") []}
	    )
	  , ( Builtin "dynamic"
	    , {zero & fe_type=Just $ Func [Var "a"] (Type "Dynamic" []) [Instance "TC" [Var "a"]]}
	    )
	  ]

builtin_classes :: [(Location, ClassEntry)]
builtin_classes
	= [ ( Builtin "TC"
	    , { ce_vars=["v"]
	      , ce_context=[]
	      , ce_documentation=Nothing
	      , ce_members=[]
	      , ce_instances=[]
	      , ce_derivations=[]
	      }
	    )
	  ]

builtin_types :: [(Location, TypeDefEntry)]
builtin_types
	= [ ( Builtin "Bool"
	    , { deft
	      & tde_typedef.td_name = "Bool"
	      , tde_typedef.td_rhs  = TDRCons False
	        [ { defc & cons_name="False" }
	        , { defc & cons_name="True" }
	        ]
	      }
	    )
	  , ( Builtin "Int",     { deft & tde_typedef.td_name = "Int"     } )
	  , ( Builtin "Real",    { deft & tde_typedef.td_name = "Real"    } )
	  , ( Builtin "Char",    { deft & tde_typedef.td_name = "Char"    } )
	  , ( Builtin "String",  { deft & tde_typedef.td_name = "String",
	      tde_typedef.td_rhs = TDRSynonym (Type "_#Array" [Type "Char" []]) } )
	  , ( Builtin "Dynamic", { deft & tde_typedef.td_name = "Dynamic" } )
	  , ( Builtin "File",    { deft & tde_typedef.td_name = "File"    } )
	  , ( Builtin "World",   { deft & tde_typedef.td_name = "World",
	      tde_typedef.td_uniq = True } )
	  : lists
	  ]
where
	deft = {tde_typedef={td_name="", td_uniq=False, td_args=[], td_rhs=TDRAbstract}, tde_doc=Nothing}
	defc = {cons_name="", cons_args=[], cons_exi_vars=[], cons_context=[], cons_priority=Nothing}

	lists = [make_list kind spine \\ kind <- [[], ['#'], ['!'], ['|']], spine <- [[], ['!']] | kind <> ['|'] || spine <> ['!']]
	where
		make_list :: [Char] [Char] -> (Location, TypeDefEntry)
		make_list k s = (Builtin higherorder,
			{ deft
			& tde_typedef.td_name = toString (['_':k] ++ ['List'] ++ s)
			, tde_typedef.td_args = [Var "a"]
			, tde_doc             = Just $ TypeDoc (Just $ "A" + kind + spine + " list.\n\n" + description) ["The type of the list elements."] Nothing
		//	, syntax_doc_locations = [CLR 6 "4.2" "_Toc311798019"]
		//	, syntax_examples      = map (EXs "Function" "macro") ["f :: " <+ lista <+ " -> a", "ints = " <+ listints]
			})
		where
			higherorder = toString (['[':k] ++ s` ++ [']'])
				with s` = if (s == ['!'] && k == []) [' !'] s
			lista       = toString (['[':k] ++ ['a':s] ++ [']'])
			listints    = toString (['[':k] ++ ['1,1,2,3,5':s] ++ [']'])
			listany     = toString (['[':k] ++ ['\\','w':s] ++ [']'])
			kind = case k of
				[]    ->  " normal"
				['#'] -> "n unboxed"
				['!'] ->  " head strict"
				['|'] -> "n overloaded"
			spine = case s of
				[]    -> ""
				['!'] -> " spine strict"

			description = "These types of list are available:\n" +
				"- {{`[a]`}}, a normal list\n" +
				"- {{`[#a]`}}, an unboxed head-strict list (elements are stored directly, without pointers)\n" +
				"- {{`[!a]`}}, a head-strict list (the first element is in root normal form)\n" +
				"- {{`[a!]`}}, a spine-strict list (the last element is known)\n" +
				"- {{`[#a!]`}}, an unboxed spine-strict list\n" +
				"- {{`[!a!]`}}, a head-strict spine-strict list\n" +
				"- {{`[|a]`}}, an overloaded list (one of the types above)"

builtin_syntax :: [([String], SyntaxEntry)]
builtin_syntax =
	  bs_arrays ++
	[ bs_case
	, bs_class
	, bs_code
	, bs_define_constant
	, bs_define_graph
	, bs_dotdot
	, bs_exists
	, bs_forall
	, bs_import
	, bs_infix
	, bs_instance
	, bs_let
	, bs_let_before
	, bs_macro
	, bs_module
	, bs_otherwise
	// TODO bs_selection (arrays and records)
	, bs_strict
	, bs_synonym
	, bs_synonym_abstract
	] ++ bs_tuples ++
	[ bs_update_array
	, bs_update_record
	, bs_where_class
	, bs_where_instance
	, bs_where_local
	, bs_with
	// TODO bs_zf
	]

CLR :: Int String String -> CleanLangReportLocation
CLR f sec h =
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

bs_arrays = [make_array kind \\ kind <- [[], ['!'], ['#']]]
where
	make_array :: [Char] -> ([SyntaxPattern], SyntaxEntry)
	make_array k = (["array", typec, toString (['{':k]++['\\w}'])],
		{ syntax_title         = kind + "array"
		, syntax_code          = [typec]
		, syntax_description   = "An array contains a finite number of elements of the same type. Access time is constant."
		, syntax_doc_locations = [CLR 6 "4.4" "_Toc311798029"]
		, syntax_examples      = [EX "Function" ("xs :: {" <+ k <+ "Int}\nxs = {" <+ k <+ "1,3,6,10}")]
		})
	where
		typec = toString (['{':k]++['}'])
		kind = case k of
			[]    -> ""
			['!'] -> "strict "
			['#'] -> "unboxed "

bs_case = (["case", "of", "case of"],
	{ syntax_title         = "case expression"
	, syntax_code          = ["case ... of ..."]
	, syntax_description   = "Pattern match on an expression and do something depending on the alternative of the matching pattern."
	, syntax_doc_locations = [CLR 5 "3.4.2" "_Toc311798001"]
	, syntax_examples      =
		[ EXs "Function" "macro" "isJust m = case m of\n\tJust _ -> True\n\t_      -> False"
		]
	})

bs_class = (["class"],
	{ syntax_title         = "class"
	, syntax_code          =
		[ "class ... ... :: ..."
		, "class ... ... where ..."
		]
	, syntax_description   =
		"Classes are (sets of) overloaded functions. For classes with only one member function, a simplified syntax exists.\n" +
		"Types can instantiate classes with the {{`instance`}} keyword."
	, syntax_doc_locations = [CLR 8 "6.1" "_Toc311798056"]
	, syntax_examples      = map (EX "ClassDef")
		[ "class zero a :: a // one member" // TODO highlighting
		, "class Text s      // multiple members\nwhere\n\ttextSize :: !s -> Int\n\tconcat :: ![s] -> s\n\t// ..." // TODO highlighting
		]
	})

bs_code = (["code", "inline", "code inline"],
	{ syntax_title         = "ABC code"
	, syntax_code          = ["... = code [inline] { ... }"]
	, syntax_description   =
		"A code block with raw ABC instructions, which can be used for primitive functions like integer addition, for linking with C, bypassing the type system... welcome down the rabbit hole!\n" +
		"When `inline` is used, the function will be inlined when applied in a strict context."
	, syntax_doc_locations = [CLR 13 "11.2" "_Toc311798115"]
	, syntax_examples      = map (EX "Function") // TODO highlighting
		[ "add :: !Int !Int -> Int                   // Primitive function\nadd a b = code inline {\n\taddI\n}"
		, "sleep :: !Int !*World -> *(!Int, !*World) // Linking with C\nsleep n w = code {\n\tccall sleep \"I:I:A\"\n}"
		, "cast :: !.a -> .b                         // Bypassing the type system\ncast _ = code {\n\tpop_a 1\n}"
		]
	})

bs_define_constant = (["=:"],
	{ syntax_title         = "graph definition"
	, syntax_code          = ["... =: ..."]
	, syntax_description   =
		"Defining constants with `=:` at the top level makes sure they are shared through out the program; hence, they are evaluated only once.\n" +
		"This is the default understanding of `=` in local scope.\n" +
		"The inverse is {{`=>`}}, which defines an identifier to be a constant function."
	, syntax_doc_locations = [CLR 5 "3.6" "_Toc311798007"]
	, syntax_examples      = [EXs "Function" "macro" "mylist =: [1..10000]"]
	})
bs_define_graph = (["=>"],
	{ syntax_title         = "constant function definition"
	, syntax_code          = ["... => ..."]
	, syntax_description   =
		"Defining constants with `=>` at the top level makes sure they are interpreted as constant functions; hence, they are evaluated every time they are needed.\n" +
		"This is the default understanding of `=` in global scope.\n" +
		"The inverse is {{`=:`}}, which defines an identifier to be a graph."
	, syntax_doc_locations = [CLR 5 "3.6" "_Toc311798007"]
	, syntax_examples      = [EXs "Function" "macro" "mylist => [1..10000]"]
	})

bs_dotdot = (["[\\e..]", "[\\e..\e]", "[\\e,\\e..]", "[[\\e,\\e..\\e]", "dotdot", "dot-dot"],
	{ syntax_title         = "dotdot expression"
	, syntax_code          = ["[i..]", "[i..k]", "[i,j..]", "[i,j..k]"]
	, syntax_description   =
		"A shorthand for lists of enumerable types.\n" +
		"To use these expressions, you must import {{`StdEnum`}}. The underlying functions are defined in {{`_SystemEnum`}}."
	, syntax_doc_locations = [CLR 6 "4.2.1" "_Toc311798023"]
	, syntax_examples      = map (EXs "Function" "macro")
		[ "xs = [0..]     // 0, 1, 2, 3, ..."
		, "xs = [0,2..]   // 0, 2, 4, 6, ..."
		, "xs = [0..10]   // 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10"
		, "xs = [0,2..10] // 0, 2, 4, 6, 8, 10"
		]
	})

bs_exists = (["E", "E.*"],
	{ syntax_title         = "existential quantifier"
	, syntax_code          = [":: ... = E. ...: ..."]
	, syntax_description   = "Existential quantifiers make it possible to define (recursive) objects of the same type with different types of content."
	, syntax_doc_locations = [CLR 7 "5.1.3" "_Toc311798042"]
	, syntax_examples      = [EX "Function" ":: List = E.e: Cons e List | Nil\nStart = Cons 5 (Cons 'a' (Cons \"abc\" Nil))"] // TODO highlighting
	})

bs_forall = (["A", "A.*"],
	{ syntax_title         = "universal quantifier"
	, syntax_code          = ["A. ...:"]
	, syntax_description   = "Explicitly marks polymorphic type variables. Clean does not yet allow universal quantifiers on the topmost level."
	, syntax_doc_locations = [CLR 5 "3.7.4" "_Toc311798013"]
	, syntax_examples      = map (EX "Function")
		[ "hd :: A.a: [a] -> a           // Not yet allowed: A. on the topmost level"
		, "h :: (A.a: [a] -> Int) -> Int // The quantifier is needed to apply the function to both a [Int] and a [Char]\nh f = f [1..100] + f ['a'..'z']"
		]
	})

bs_import = (["import", "from", "qualified", "as"],
	{ syntax_title         = "imports"
	, syntax_code          = ["import [qualified] ... [as ...]", "from ... import ..."]
	, syntax_description   =
		"Imports code from other modules.\n" +
		"With the `from` keyword, one can achieve more granularity.\n" +
		"In case of name clashes, `qualified` can be used (undocumented)."
	, syntax_doc_locations = [CLR 4 "2.5" "_Toc311797991"]
	, syntax_examples      = map (EX "Function")
		[ "import ..."
		, "import StdEnv                  // Import all code from the StdEnv definition module"
		, "from StdFunc import o          // Import only the o function from StdFunc"
		, "import qualified Data.Map as M // Import Data.Map such that functions are available as e.g. 'M'.get."
		]
	})

bs_infix = (["infix", "infixl", "infixr"],
	{ syntax_title         = "infix operator"
	, syntax_code          = ["infix[l,r] [...]"]
	, syntax_description   =
		"Defines a function with arity 2 that can be used in infix position.\n" +
		"The following number, if any, determines the precedence.\n" +
		"`infixl` and `infixr` indicate associativity."
	, syntax_doc_locations = [CLR 5 "3.7.2" "_Toc311798011"]
	, syntax_examples      =
		[ EX  "Function"         "(bitor) infixl 6 :: !Int !Int -> Int // Left-associative infix function with precedence 6"
		, EXs "Function" "macro" "(o) infixr 9                         // Infix macro\n(o) f g :== \\x -> f (g x)"
		, EX  "TypeDef"          ":: MyType = (:+:) infixl 6 Int Int   // Infix data constructor, can be used as (5 :+: 10)"
		]
	})

bs_instance = (["instance"],
	{ syntax_title         = "instance"
	, syntax_code          = ["instance ... ... where ..."]
	, syntax_description   = "Defines an instantiation of a {{class}} for a type."
	, syntax_doc_locations = [CLR 8 "6.1" "_Toc311798056"]
	, syntax_examples      = map (EX "Function")
		[ "instance zero Int\nwhere\n\tzero = 0"
		, "instance zero Real\nwhere\n\tzero = 0.0"
		]
	})

bs_let = (["let", "in", "let in"],
	{ syntax_title         = "let expression"
	, syntax_code          = ["let ... in ..."]
	, syntax_description   = "An expression that introduces new scope."
	, syntax_doc_locations = [CLR 5 "3.5.1" "_Toc311798003"]
	, syntax_examples      =
		[ EXs "Function" "macro"    "fac n = let fs = [1:1:[(fs!!(i-1)) + (fs!!(i-2)) \\ i <- [2..]]] in fs !! n"
		, EXs "Function" "macrorhs" "let // Multi-line let expressions\n\tfunction args = body\n\tselector = expr\n\t// ...\nin expression"
		]
	})
bs_let_before = (["#", "#!"],
	{ syntax_title         = "let before"
	, syntax_code          = ["#  ... = ...", "#! ... = ..."]
	, syntax_description   = "A {{`let`}} expression that can be defined before a guard or function body, which eases the syntax of sequential actions."
	, syntax_doc_locations = [CLR 5 "3.5.4" "_Toc311798006"]
	, syntax_examples      =
		[ EX "Function" "readchars :: *File -> *([Char], *File)\nreadchars f\n# (ok,c,f) = freadc file\n| not ok   = ([], f)\n# (cs,f)   = readchars f\n= ([c:cs], f)"
		]
	})

bs_macro = ([":==", "macro"],
	{ syntax_title         = "macro"
	, syntax_code          = ["... :== ..."]
	, syntax_description   =
		"A macro is a compile-time rewrite rule. It can be used for constants, inline subtitutions, renaming functions, conditional compilation, etc.\n" +
		"Macros can appear in patterns to match on constants."
	, syntax_doc_locations = [CLR 12 "10.3" "_Toc311798111"]
	, syntax_examples      = map (EXs "Function" "macro")
		[ "flip f a b :== f b a                    // Useful for currying"
		, "IF_INT_64_OR_32 int64 int32 :== int64   // Conditional compilation"
		, "(o) infixr 9                            // Function composition. Doing this at run-time would be slow\n(o) f g :== \\x -> f (g x)"
		]
	})

bs_module = (["module", "definition", "implementation", "system", "definition module", "implementation module", "system module"],
	{ syntax_title         = "module heading"
	, syntax_code          = ["[definition,implementation,system] module ..."]
	, syntax_description   = "The heading of a Clean file. Definition modules describe what things are exported (dcl files), implementation modules how they are implemented (icl files)."
	, syntax_doc_locations = [CLR 4 "2.2" "_Toc311797983"]
	, syntax_examples      = map (EX "Function")
		[ "definition module ..."
		, "definition module StdList     // Exported definitions of list functions"
		, "implementation module StdList // The implementations of the functions"
		, "module test                   // An implementation module without corresponding dcl"
		, "system module StdInt          // The definitions of a module that contains foreign code (see section 2.6 of the language report)"
		]
	})

bs_otherwise = (["otherwise"],
	{ syntax_title         = "otherwise"
	, syntax_code          = ["otherwise"]
	, syntax_description   = "The (optional) last alternative in a guard. It caches all other cases, and makes sure your program does not crash if none of the cases matches."
	, syntax_doc_locations = [CLR 5 "3.3" "_Toc311797998"]
	, syntax_examples      =
		[ EXs "Function" "macrorhs" "| otherwise = ..."
		, EXs "Function" "macro"    "sign :: !Int -> Int\nsign n\n| n  < 0    = -1 // Negative number\n| n == 0    =  0 // Zero\n| otherwise =  1 // Must be positive"
		]
	})

bs_strict = (["strict", "!"],
	{ syntax_title         = "strictness annotation"
	, syntax_code          = ["!"]
	, syntax_description   = "Override the lazy evaluation strategy: the argument must be evaluated to head normal form before the function is entered."
	, syntax_doc_locations = [CLR 5 "3.7.5" "_Toc311798014", CLR 12 "10" "_Toc311798103"]
	, syntax_examples      = [EX "Function" "acker :: !Int !Int -> Int"]
	})

bs_synonym = (["synonym", ":=="],
	{ syntax_title         = "synonym type definition"
	, syntax_code          = [":: ... :== ..."]
	, syntax_description   = "Defines a new type name for an existing type."
	, syntax_doc_locations = [CLR 7 "5.3" "_Toc311798052"]
	, syntax_examples      = [EX "TypeDef" ":: String :== {#Char}"]
	})
bs_synonym_abstract = (["synonym", ":=="],
	{ syntax_title         = "abstract synonym type definition"
	, syntax_code          = [":: ... (:== ...)"]
	, syntax_description   = "Defines a new type name for an existing type, while the type behaves as an abstract type for the programmer. This allows compiler optimisations on abstract types."
	, syntax_doc_locations = [CLR 7 "5.4.1" "_Toc311798054"]
	, syntax_examples      = [EX "TypeDef" ":: Stack a (:== [a])"]
	})

bs_tuples = [make_tuple n \\ n <- [1..31]]
where
	make_tuple :: Int -> ([SyntaxPattern], SyntaxEntry)
	make_tuple n = ([toString ['(':repeatn n ','++[')']], withargs, "tuple"],
		{ syntax_title         = ary + "ary tuple"
		, syntax_code          = [withvars]
		, syntax_description   =
			"Tuples allow bundling a finite number of expressions of different types into one object without defining a new data type.\n" +
			"Clean supports tuples of arity 2 to 32."
		, syntax_doc_locations = [CLR 6 "4.3" "_Toc311798026"]
		, syntax_examples      = []
		})
	where
		withargs = toString ['(\\w':foldl (++) [] [[',\\w'] \\ _ <- [1..n]] ++ [')']]
		withvars = toString ['(a'  :foldl (++) [] [[',':v]  \\ _ <- [1..n] & v <- map (\x->[x]) ['b'..'z'] ++ [[v,'`'] \\ v <- ['a'..]]] ++ [')']]
		ary = case n of
			1 -> "bin"
			2 -> "tren"
			n -> n+1 <+ "-"

bs_update_array = (["&", "{*&*[\\e]*=*}"],
	{ syntax_title         = "array update"
	, syntax_code          = ["{ a & [i]=x, [j]=y, ... } // Updates a by setting index i to x, j to y, ..."]
	, syntax_description   = "Updates an array by creating a copy and replacing one or more elements"
	, syntax_doc_locations = [CLR 6 "4.4.1" "_Toc311798032"]
	, syntax_examples      = []
	})
bs_update_record = (["&", "{*&*=*}"],
	{ syntax_title         = "record update"
	, syntax_code          = ["{ r & f1=x, f2=y, ... } // Updates r by setting f1 to x, f2 to y, ..."]
	, syntax_description   = "Updates a record by creating a copy and replacing one or more fields"
	, syntax_doc_locations = [CLR 7 "5.2.1" "_Toc311798049"]
	, syntax_examples      = []
	})

bs_where_class = (["where"],
	{ syntax_title         = "where"
	, syntax_code          = ["where"]
	, syntax_description   = "Introduces the members of a {{`class`}} definition."
	, syntax_doc_locations = [CLR 8 "6.1"   "_Toc311798056"]
	, syntax_examples      = [EX "ClassDef" "class Arith a        // Class definition\nwhere\n\t(+) infixl 6 :: a a -> a\n\t(-) infixl 6 :: a a -> a"] // TODO highlighting
	})
bs_where_instance = (["where"],
	{ syntax_title         = "where"
	, syntax_code          = ["where"]
	, syntax_description   = "Introduces the implementation of an {{`instance`}}."
	, syntax_doc_locations = [CLR 8 "6.1"   "_Toc311798056"]
	, syntax_examples      = [EX "Function" "instance Arith Int   // Instance definition\nwhere\n\t(+) x y = // ...\n\t(-) x y = // ..."]
	})
bs_where_local = (["where"],
	{ syntax_title         = "where"
	, syntax_code          = ["where"]
	, syntax_description   = "Introduces local definitions. For guard-local definitions, see {{`with`}}."
	, syntax_doc_locations = [CLR 5 "3.5.2" "_Toc311798004"]
	, syntax_examples      = [EXs "Function" "macro" "primes = sieve [2..] // Local definitions\nwhere\n\tsieve [pr:r] = [pr:sieve (filter pr r)]"]
	})

bs_with = (["with"],
	{ syntax_title         = "with"
	, syntax_code          = ["with"]
	, syntax_description   = "Introduces guard-local definitions. For function-local definitions, see {{`where`}}."
	, syntax_doc_locations = [CLR 5 "3.5.3" "_Toc311798005"]
	, syntax_examples      = [EXs "Function" "macro" "f x y\n| guard1 = alt1\n\twith local = expr1\n| guard2 = alt2\n\twith local = expr2"]
	})
