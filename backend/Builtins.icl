implementation module Builtins

import StdBool
import StdEnum
import StdInt
import StdList
import StdOverloaded
import StdString

from Data.Func import $
import Data.List
import Data.Maybe
import Text

import Type

import Cloogle
import Doc
import CloogleDB

builtin_functions :: [(Location, FunctionEntry)]
builtin_functions
	= [ ( Builtin "if" [CLR 5 "3.4.2" "_Toc311798001"]
	    , {zero & fe_type=Just $ Func [Type "Bool" [], Var "a", Var "a"] (Var "a") []}
	    )
	  , ( Builtin "dynamic" [CLR 10 "8.1" "_Toc311798076"]
	    , {zero & fe_type=Just $ Func [Var "a"] (Type "Dynamic" []) [Instance "TC" [Var "a"]]}
	    )
	  ]

builtin_classes :: [(Location, ClassEntry)]
builtin_classes
	= [ ( Builtin "TC" [CLR 10 "8.1.4" "_Toc311798080"]
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
	= [ ( Builtin "Bool" [CLR 6 "4.1" "_Toc311798017"]
	    , { deft
	      & tde_typedef.td_name = "Bool"
	      , tde_typedef.td_rhs  = TDRCons False
	        [ { defc & cons_name="False" }
	        , { defc & cons_name="True" }
	        ]
	      }
	    )
	  , ( Builtin "Int"     [CLR 6 "4.1" "_Toc311798017"], {deft & tde_typedef.td_name = "Int"})
	  , ( Builtin "Real"    [CLR 6 "4.1" "_Toc311798017"], {deft & tde_typedef.td_name = "Real"})
	  , ( Builtin "Char"    [CLR 6 "4.1" "_Toc311798017"], {deft & tde_typedef.td_name = "Char"})
	  , ( Builtin "String"  [CLR 6 "4.7" "_Toc311798037"], {deft & tde_typedef.td_name = "String",
	      tde_typedef.td_rhs = TDRSynonym (Type "_#Array" [Type "Char" []]) } )
	  , ( Builtin "Dynamic" [CLR 10 "8"  "_Toc311798077"], {deft & tde_typedef.td_name = "Dynamic"})
	  , ( Builtin "File"    [CLR 6 "4.7" "_Toc311798037"], {deft & tde_typedef.td_name = "File"})
	  , ( Builtin "World"   [CLR 6 "4.7" "_Toc311798037"], {deft & tde_typedef.td_name = "World",
	      tde_typedef.td_uniq = True,
	      tde_doc = Just $ TypeDoc
	        (Just "An object of this type is automatically created when the program is started, if needed. It makes efficient interfacing with the outside world possible. Its value is always `65536`.")
	        [] Nothing})
	  , ( Builtin "->"      [CLR 6 "4.6" "_Toc311798036"], {deft & tde_typedef.td_name = "(->)",
	      tde_typedef.td_args = [Var "a", Var "b"],
	      tde_doc = Just $ TypeDoc
	        (Just "The arrow type is used to denote functions.\n\nOften, function types can be written in an uncurried fashion, e.g. `a b -> c` is the same as `a -> b -> c`.")
	        ["The argument type", "The result type"]
	        Nothing})
	  , ( Builtin "()" [], {deft & tde_typedef.td_name="_Unit",
	      tde_doc = Just $ TypeDoc
	        (Just "The void / unit type.")
	        [] Nothing,
	      tde_typedef.td_rhs = TDRCons False [{defc & cons_name="()"}]})
	  :  lists
	  ++ arrays
	  ++ tuples
	  ]
where
	deft = {tde_typedef={td_name="", td_uniq=False, td_args=[], td_rhs=TDRAbstract Nothing}, tde_doc=Nothing}
	defc = {cons_name="", cons_args=[], cons_exi_vars=[], cons_context=[], cons_priority=Nothing}

	lists = [make_list kind spine \\ kind <- [[], ['#'], ['!'], ['|']], spine <- [[], ['!']] | kind <> ['|'] || spine <> ['!']]
	where
		make_list :: [Char] [Char] -> (Location, TypeDefEntry)
		make_list k s = (Builtin higherorder [CLR 6 "4.2" "_Toc311798019"],
			{ deft
			& tde_typedef.td_name = toString (['_':k] ++ ['List'] ++ s)
			, tde_typedef.td_args = [Var "a"]
			, tde_doc = Just $ TypeDoc
				(Just $ "A" + kind + spine + " list.\n\n" + description)
				["The type of the list elements."]
				Nothing
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

			description = "These types of list are available:\n\n" +
				"- {{`[a]`}}, a normal list\n" +
				"- {{`[#a]`}}, an unboxed head-strict list (elements are stored directly, without pointers)\n" +
				"- {{`[!a]`}}, a head-strict list (the first element is in root normal form)\n" +
				"- {{`[a!]`}}, a spine-strict list (the last element is known)\n" +
				"- {{`[#a!]`}}, an unboxed spine-strict list\n" +
				"- {{`[!a!]`}}, a head-strict spine-strict list\n" +
				"- {{`[|a]`}}, an overloaded list (one of the types above)"

	arrays = [make_array kind \\ kind <- [[], ['!'], ['#']]]
	where
		make_array :: [Char] -> (Location, TypeDefEntry)
		make_array k = (Builtin typec [CLR 6 "4.4" "_Toc311798029"],
			{ deft
			& tde_typedef.td_name = toString (['_':k] ++ ['Array'])
			, tde_typedef.td_args = [Var "a"]
			, tde_doc = Just $ TypeDoc
				(Just $ "An array contains a finite number of elements of the same type. Access time is constant.\n\n" + description)
				["The type of the array elements."]
				Nothing
			})
		where
			typec = toString (['{':k]++['}'])

			description = "These types of array are available:\n\n" +
				"- `{a}`, a normal array\n" +
				"- `{#a}`, an unboxed strict array (elements are stored directly, without pointers)\n" +
				"- `{!a}`, a strict array (the elements are in root normal form)"

	tuples = [make_tuple n \\ n <- [2..32]]
	where
		make_tuple :: Int -> (Location, TypeDefEntry)
		make_tuple n = (Builtin typec [CLR 6 "4.3" "_Toc311798026"],
			{ deft
			& tde_typedef.td_name = "_Tuple" <+ n
			, tde_typedef.td_args = [Var $ toString [v:repeatn (n / 26) '`'] \\ v <- cycle ['a'..'z'] & n <- [0..n-1]]
			, tde_doc = Just $ TypeDoc
				(Just $ article + " " + ary + "ary tuple.\n\n" +
				 "Tuples allow bundling a finite number of expressions of different types into one object without defining a new data type.\n\n" +
				 "Clean supports tuples of arity 2 to 32.")
				[] Nothing
			})
		where
			typec = toString ['(':repeatn (n-1) ',' ++ [')']]
			ary = case n of
				2 -> "bin"
				3 -> "tern"
				n -> n <+ "-"
			article = case n of
				11 -> "An"
				18 -> "An"
				_  -> "A"

builtin_syntax :: [([String], SyntaxEntry)]
builtin_syntax =
	[ bs_case
	, bs_class
	, bs_code
	, bs_define_constant
	, bs_define_graph
	, bs_dotdot
	, bs_exists
	, bs_forall
	, bs_generic
	, bs_import
	, bs_infix
	, bs_instance
	, bs_lambda
	, bs_layout_rule
	, bs_let
	, bs_let_before
	, bs_list_expressions
	, bs_macro
	, bs_module
	, bs_newtype
	, bs_overloaded_type_variable
	, bs_otherwise
	, bs_pattern_named
	, bs_selection_array
	, bs_selection_array_unique
	, bs_selection_record
	, bs_selection_record_unique
	, bs_strict
	, bs_synonym
	, bs_synonym_abstract
	, bs_update_array
	, bs_update_record
	, bs_where_class
	, bs_where_instance
	, bs_where_local
	, bs_with
	, bs_zf
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
		"Classes are (sets of) overloaded functions. For classes with only one member function, a simplified syntax exists.\n\n" +
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
		"A code block with raw ABC instructions, which can be used for primitive functions like integer addition, for linking with C, bypassing the type system... welcome down the rabbit hole!\n\n" +
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
		"Defining constants with `=:` at the top level makes sure they are shared through out the program; hence, they are evaluated only once.\n\n" +
		"This is the default understanding of `=` in local scope.\n\n" +
		"The inverse is {{`=>`}}, which defines an identifier to be a constant function."
	, syntax_doc_locations = [CLR 5 "3.6" "_Toc311798007"]
	, syntax_examples      = [EXs "Function" "macro" "mylist =: [1..10000]"]
	})
bs_define_graph = (["=>"],
	{ syntax_title         = "constant function definition"
	, syntax_code          = ["... => ..."]
	, syntax_description   =
		"Defining constants with `=>` at the top level makes sure they are interpreted as constant functions; hence, they are evaluated every time they are needed.\n\n" +
		"This is the default understanding of `=` in global scope.\n\n" +
		"The inverse is {{`=:`}}, which defines an identifier to be a graph."
	, syntax_doc_locations = [CLR 5 "3.6" "_Toc311798007"]
	, syntax_examples      = [EXs "Function" "macro" "mylist => [1..10000]"]
	})

bs_dotdot = (["[\\e..]", "[\\e..\e]", "[\\e,\\e..]", "[[\\e,\\e..\\e]", "dotdot", "dot-dot"],
	{ syntax_title         = "dotdot expression"
	, syntax_code          = ["[i..]", "[i..k]", "[i,j..]", "[i,j..k]"]
	, syntax_description   =
		"A shorthand for lists of enumerable types.\n\n" +
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
	, syntax_examples      =
		[ EX "Function" "hd :: A.a: [a] -> a           // Not yet allowed: A. on the topmost level"
		, EX "Function" "h :: (A.a: [a] -> Int) -> Int // The quantifier is needed to apply the function to both a [Int] and a [Char]\nh f = f [1..100] + f ['a'..'z']"
		, EX "TypeDef"  ":: T = C (A.a: a -> a)        // In a type"
		]
	})

bs_generic = (["generic", "derive", "of", "{|*|}"], // This * matches everything, which is intentional
	{ syntax_title         = "generic function definition"
	, syntax_code          = ["generic ... ... :: ...", "derive ... ..."]
	, syntax_description   = "With generics, a function can be defined once and derived for (almost) all possible types, to avoid very similar code snippets."
	, syntax_doc_locations = [CLR 9 "7.2" "_Toc311798069"]
	, syntax_examples      =
		[ EX  "Function"           "generic gEq a :: !a !a -> Bool        // The type of a generic function"
		, EXs "Function" "macro" $ "gEq{|Int|} x y = x == y               // Implementation of a generic\n" +
		  "gEq{|PAIR|} fx fy (PAIR x1 y1) (PAIR x2 y2) = fx x1 x2 && fy y1 y2" // TODO highlighting
		, EX  "Function"           "derive gEq []                         // Deriving the gEq generic for type []"
		, EXs "Function" "macro"   "gConsName{|CONS of d|} _ = d.gcd_name // Using type information"
		]
	})

bs_import = (["import", "from", "qualified", "as"],
	{ syntax_title         = "imports"
	, syntax_code          = ["import [qualified] ... [as ...]", "from ... import ..."]
	, syntax_description   =
		"Imports code from other modules.\n\n" +
		"With the `from` keyword, one can achieve more granularity.\n\n" +
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
		"Defines a function with arity 2 that can be used in infix position.\n\n" +
		"The following number, if any, determines the precedence.\n\n" +
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

bs_lambda = (["lambda", "\\*", "->", "."],
	{ syntax_title         = "lambda abstraction"
	, syntax_code          = ["\\... -> ...", "\\... . ...", "\\... = ..."]
	, syntax_description   = "An anonymous, inline function."
	, syntax_doc_locations = [CLR 5 "3.4.1" "_Toc311798000"]
	, syntax_examples      = map (EXs "Function" "macro")
		[ "(o) f g = \\x -> f (g x)         // Simple lambda expression"
		, "swapall = map (\\(x,y) -> (y,x)) // Pattern matching in lambda arguments"
		, "mul     = \\x y -> x * y         // Multiple arguments (of course, it would be better to write `mul x y = x * y` or `mul = (*)`)"
		]
	})

bs_layout_rule = ([";", "{", "}"],
	{ syntax_title         = "layout rule"
	, syntax_code          = ["...;", "{ ... }"]
	, syntax_description   =
		"Most Clean programs are written using the layout rule, which means that scopes are indicated with indent levels." +
		"The layout sensitive mode can be turned off by adding a semicolon `;` at the end of the {{module}} line." +
		"Then, scopes have to be indicated with `{ ... }` and definitions have to end with `;`."
	, syntax_doc_locations = [CLR 4 "2.3.3" "_Toc311797989"]
	, syntax_examples      = [EX "Module" $
		"module test;\n" +
		"import StdEnv;\n" +
		"Start :: [(Int,Int)];\n" +
		"Start = [(x,y) \\\\ x <- odds, y <- evens];\n" +
		"where\n" +
		"{\n" +
		"\todds  = [1,3..9];\n" +
		"\tevens = [0,2..8];\n" +
		"}"]
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

bs_list_expressions = (["list", "[]", "[:]", ":", "[\\e:\\e]", "['*"],
	{ syntax_title         = "list expression"
	, syntax_code          = ["[]", "[...:...]", "[..., ..., ...]", "['...']"]
	, syntax_description   =
		"A list can be composed of individual elements or a head and a tail. Special syntax is available for creating `[{{Char}}]` lists.\n\n" +
		"See also {{dotdot}} expressions.\n\n" +
		"The colon is not an operator in Clean, because it must always be surrounded by `[` and `]`. It can therefore not be curried, flipped, etc."
	, syntax_doc_locations = [CLR 6 "4.2.1" "_Toc311798021"]
	, syntax_examples      = map (EXs "Function" "macro")
		[ "abc = ['a', 'b', 'c']     // Individual elements"
		, "abc = ['a':['b':['c':[]]] // Head and tail, ending with the empty list"
		, "abc = ['abc']             // Special syntax for [Char] lists"
		]
	})

bs_macro = ([":==", "macro"],
	{ syntax_title         = "macro"
	, syntax_code          = ["... :== ..."]
	, syntax_description   =
		"A macro is a compile-time rewrite rule. It can be used for constants, inline subtitutions, renaming functions, conditional compilation, etc.\n\n" +
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

bs_newtype = (["=:", "newtype"],
	{ syntax_title         = "Newtype definition (experimental)"
	, syntax_code          = [":: ... =: ... ..."]
	, syntax_description   = "A newtype is a type synonym at run-time but treated as a real type at compile-time.\n"
	                       + "This allows the creation of separate instances without overhead."
	, syntax_doc_locations = []
	, syntax_examples      =
		[ EX "TypeDef" ":: T =: T Int"
		, EX "TypeDef" ":: T a =: T a"
		]
	})

bs_overloaded_type_variable = (["^", "a^"],
	{ syntax_title         = "Overloaded type variable"
	, syntax_code          = ["... :: ...^"]
	, syntax_description   = "A pattern match on the type of a dynamic depending on the type of the function."
	, syntax_doc_locations = [CLR 10 "8.2.5" "_Toc311798087"]
	, syntax_examples      = [EX "Function" "unpack :: Dynamic -> Maybe a\nunpack (x :: a^) = Just x // Only values of type a\nunpack _         = Nothing"]
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

bs_pattern_named = (["=:"],
	{ syntax_title         = "named pattern match"
	, syntax_code          = ["...=:(...)"]
	, syntax_description   = "Give a name to the expression of a pattern to be able to use the whole expression without creating new graphs."
	, syntax_doc_locations = [CLR 5 "3.2" "_Toc311797997"]
	, syntax_examples      =
		[ EXs "Function" "macro" "isJustU e=:(Just _) = (True, e) // On an ADT"
		, EX  "Function"         ":: Position = {px :: Int, py :: Int}\ngetx p=:{px} = (px, p) // On a record; this has type :: Position -> (Int, Position)"
		]
	})

bs_selection_array = ([".[]", ".[\\e]", ".[,*]", ".[\\e,*]"],
	{ syntax_title         = "array selection"
	, syntax_code          = [".[i]", ".[i,j,...]"]
	, syntax_description   = "Select an element from a (possibly multidimensional) array. The indexes must have the type {{`Int`}}."
	, syntax_doc_locations = [CLR 6 "4.4.1" "_Toc311798033"]
	, syntax_examples      = map (EXs "Function" "macro")
		[ "five = {1,2,3,4,5,6,7,8,9,10}.[4]    // Arrays are zero-indexed"
		, "five = {{1,2},{3,4,5},{6,7,8}}.[1,2] // This is equivalent to (...).[1].[2]"
		]
	})
bs_selection_array_unique = (["![]", "![\\e]", "![,*]", "![\\e,*]"],
	{ syntax_title         = "unique array selection"
	, syntax_code          = ["![i]", "![i,j,...]"]
	, syntax_description   = "Select an element from a (possibly multidimensional, possibly unique) array and return both the element and the array. The indexes must have the type {{`Int`}}."
	, syntax_doc_locations = [CLR 6 "4.4.1" "_Toc311798033"]
	, syntax_examples      = map (EXs "Function" "macro")
		[ "(five,arr) = {1,2,3,4,5,6,7,8,9,10}![4]"
		, "(five,arr) = {{1,2},{3,4,5},{6,7,8}}![1,2]"
		]
	})
bs_selection_record = (["."],
	{ syntax_title         = "record selection"
	, syntax_code          = ["."]
	, syntax_description   = "Select a field from a (possibly multilevel) record."
	, syntax_doc_locations = [CLR 7 "5.2.1" "_Toc311798050"]
	, syntax_examples      = map (EXs "Function" "macro")
		[ "five = {px=5, py=10}.px"
		, "five = {pxy={px=5, py=10}, pz=2}.pxy.px"
		, "five = {px=5, py=10}.Position.px // If multiple records have a field px, the type name can be used for disambiguation"
		]
	})
bs_selection_record_unique = (["!"],
	{ syntax_title         = "unique record selection"
	, syntax_code          = ["!"]
	, syntax_description   = "Select a field from a (possibly multilevel, possibly unique) record and return both the field data and the record."
	, syntax_doc_locations = [CLR 7 "5.2.1" "_Toc311798050"]
	, syntax_examples      = map (EXs "Function" "macro")
		[ "(five,rec) = {px=5, py=10}!px"
		, "(five,rec) = {pxy={px=5, py=10}, pz=2}!pxy.px // Only the first field should have the exclamation mark"
		, "(five,rec) = {px=5, py=10}!Position.px // If multiple records have a field px, the type name can be used for disambiguation\n" +
		  "                                       // The language report is erroneous here. It is !Position.px, not .Position!px."
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

bs_update_array = (["&", "{*&*[\\e]*=*}"],
	{ syntax_title         = "array update"
	, syntax_code          =
		[ "{ a & [i]=x, [j]=y, ... } // Updates a by setting index i to x, j to y, ..."
		, "# a & [i]=x, [j]=y, ...   // Same as # a = {a & [i]=x, [j]=y, ...}" // See https://clean.cs.ru.nl/Clean_2.3
		]
	, syntax_description   = "Updates an array by creating a copy and replacing one or more elements."
	, syntax_doc_locations = [CLR 6 "4.4.1" "_Toc311798032"]
	, syntax_examples      = []
	})
bs_update_record = (["&", "{*&*=*}"],
	{ syntax_title         = "record update"
	, syntax_code          =
		[ "{ r & f1=x, f2=y, ... } // Updates r by setting f1 to x, f2 to y, ..."
		, "# r & f1=x, f2=y, ...   // Same as # r = {r & f1=x, f2=y, ...}" // See https://clean.cs.ru.nl/Clean_2.3
		]
	, syntax_description   = "Updates a record by creating a copy and replacing one or more fields."
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

bs_zf = (["ZF-expression", "ZF", "zf", "comprehension", "<-", "<|-", "<-:", "\\\\", ",", "&", "|"],
	{ syntax_title         = "list comprehension"
	, syntax_code          = ["[... \\\\ ... <- ...]"]
	, syntax_description   = "Constructs a list composed of elements drawn from other lists or arrays."
	, syntax_doc_locations = [CLR 6 "4.2.1" "_Toc311798024"]
	, syntax_examples      = map (EXs "Function" "macro")
		[ "cartesian    = [(x,y) \\\\ x <- [1,2,3], y <- [10,20]] // Cartesian product: (1,10), (1,20), (2,10), (2,20), (3,10), (3,20)"
		, "zip xs ys    = [(x,y) \\\\ x <- xs & y <- ys]          // Pairwise zip through the lists"
		, "filter f xs  = [x \\\\ x <- xs | f x]                  // Guard to add conditions"
		, "catMaybes ms = [x \\\\ Just x <- ms]                   // Pattern matching in the selector"
		, "triangle     = [(x,y) \\\\ x <- [1,2,3], y <- [1..x]]  // Reusing x in the next generator: (1,1), (2,1), (2,2), (3,1), (3,2), (3,3)"
		, "arrToList a  = [x \\\\ x <-: a]                        // <-: for arrays"
		, "castList xs  = [|x \\\\ x <|- xs]                      // The two pipe characters make both xs and the result overloaded lists"
		]
	})
