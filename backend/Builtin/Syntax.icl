implementation module Builtin.Syntax

import StdArray
import StdList
import StdString

import Data.Error
from Data.Func import $
import Data.Maybe
import Text

import Regex
exact s = regex ("^" +++ s +++ "$")

import Cloogle.API
import Cloogle.DB

import Builtin.Predef

builtin_syntax :: [SyntaxEntry]
builtin_syntax =
	[ bs_basicvalues
	, bs_case
	, bs_class
	, bs_code
	, bs_comments
	, bs_context
	, bs_define_constant
	, bs_define_graph
	, bs_dotdot
	, bs_exists
	, bs_extensible_adt
	, bs_forall
	, bs_foreign
	, bs_funcdep
	, bs_function_definition
	, bs_guard
	, bs_generic
	, bs_hierarchical_modules
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
	, bs_pattern_named
	, bs_pattern_predicate
	, bs_qualified_identifier
	, bs_record_disambiguation
	, bs_selection_array
	, bs_selection_array_unique
	, bs_selection_record
	, bs_selection_record_unique
	, bs_strict
	, bs_synonym
	, bs_synonym_abstract
	, bs_type_definition
	, bs_type_specification
	, bs_unique
	, bs_update_array
	, bs_update_record
	, bs_where_class
	, bs_where_instance
	, bs_where_local
	, bs_with
	, bs_zf
	]

EX :: !String -> SyntaxExample 
EX c = {example=c, cleanjs_start=Nothing}
EXs :: !String !String -> SyntaxExample
EXs s c = {example=c, cleanjs_start=Just s}

bs_basicvalues =
	{ syntax_title         = "basicvalues"
	, syntax_patterns      = map exact ["'.'", "[+-]?\\d+", "[+-]?0[0-7]+", "[-+]?0x[0-9a-fA-F]+", "'.'", "True", "False", "E"]
	, syntax_code          = ["0x...", "0...", "True", "False", "'...'", "...E..."]
	, syntax_description   =
		"Constant basic values can be of type Int, Char, Real and Bool.\n\n" +
		"Integers can be either defined in decimal (default), octal (`0` prefix) or hexadecimal (`0x` prefix).\n\n" +
		"Characters can either be printable characters (except `'`) or an escape sequence.\n\n" +
		"Reals can be suffixed by a power of ten in the scientific notation.\n\n" +
		"Boolean values by their only inhabitants {{True}} and {{False}}.\n\n" +
		"Basic values can also be pattern matched with these notations."
	, syntax_doc_locations = [CLR 4 "1.1" "_Toc311798017"]
	, syntax_examples      = map EX
		[ "(42, -42, +42)          // Tuple with 42, -42 and 42 in decimal"
		, "(052, -052, +052)       // Tuple with 42, -42 and 42 in octal"
		, "(0x2a, -0x2a, +0x2A)    // Tuple with 42, -42 and 42 in hexadecimal"
		, "('a', '\\x2a', '\\052') // Tuple with a normal character, a literal quote and twice the character with ordinal 42"
		, "['\\n', '\\r', '\\f', '\\b', '\\t', '\\', '\'']\n" +
		  "                        //All character escapes"
		, "(True, False)           // All booleans in a tuple"
		, "(42.0, -42.0, 42E-10, +42.0E+10, -42.0E10)\n" +
		  "                        //Several reals"
		]
	}


bs_case =
	{ syntax_title         = "case expression"
	, syntax_patterns      = map exact ["case", "of", "case of", "->", "="]
	, syntax_code          = ["case ... of ..."]
	, syntax_description   = join " "
		[ "Pattern match on an expression and do something depending on the alternative of the matching pattern."
		, "Both `->` and `=` can be used to separate patterns and alternatives, however, they cannot be mixed."
		]
	, syntax_doc_locations = [CLR 5 "3.4.2" "_Toc311798001"]
	, syntax_examples      =
		[ EXs "macro" "isJust m = case m of\n\tJust _ -> True\n\t_      -> False"
		]
	}

bs_class =
	{ syntax_title         = "class"
	, syntax_patterns      = map exact ["class"]
	, syntax_code          =
		[ "class ... ... :: ..."
		, "class ... ... where ..."
		]
	, syntax_description   =
		"Classes are (sets of) overloaded functions. For classes with only one member function, a simplified syntax exists.\n\n" +
		"Types can instantiate classes with the {{`instance`}} keyword."
	, syntax_doc_locations = [CLR 8 "6.1" "_Toc311798056"]
	, syntax_examples      = map EX
		[ "class zero a :: a // one member"
		, "class Text s      // multiple members\nwhere\n\ttextSize :: !s -> Int\n\tconcat :: ![s] -> s\n\t// ..."
		]
	}

bs_code =
	{ syntax_title         = "ABC code"
	, syntax_patterns      = map exact ["code", "inline", "code inline"]
	, syntax_code          = ["... = code [inline] { ... }"]
	, syntax_description   =
		"A code block with raw ABC instructions, which can be used for primitive functions like integer addition, for linking with C, bypassing the type system... welcome down the rabbit hole!\n\n" +
		"When `inline` is used, the function will be inlined when applied in a strict context."
	, syntax_doc_locations = [CLR 13 "11.2" "_Toc311798115"]
	, syntax_examples      = map EX
		[ "add :: !Int !Int -> Int                   // Primitive function\nadd a b = code inline {\n\taddI\n}"
		, "sleep :: !Int !*World -> *(!Int, !*World) // Linking with C\nsleep n w = code {\n\tccall sleep \"I:I:A\"\n}"
		, "cast :: !.a -> .b                         // Bypassing the type system\ncast _ = code {\n\tno_op\n}"
		]
	}

bs_comments =
	{ syntax_title         = "comments"
	, syntax_patterns      = map exact ["//", "/\\*.*\\*/"]
	, syntax_code          = ["// ...", "/* ... */"]
	, syntax_description   = "`//` adds a single-line comment. `/*` and `*/` encapsulate a multi-line comment. Multi-line comments can be nested."
	, syntax_doc_locations = [CLR 15 "B.2" "_Toc311798132"]
	, syntax_examples      = []
	}

bs_context =
	{ syntax_title         = "type context"
	, syntax_patterns      = map exact ["\\|", "&", ",", "special"]
	, syntax_code          = [":: ... | ..., ... [special ...=...]", "| ... & ..., ..."]
	, syntax_description   = join "\n"
		[ "A type context indicates what {{class}}es must be instantiated by type variables.\n"
		, "For function types, the type context starts with `|`."
		, "Several classes can be given to the same variable with `,`."
		, "To add multiple restrictions, use `&`.\n"
		, "In constructors, the type context starts with `&`.\n"
		, "Uniqueness constraints can be given with `,`. For details, see under {{`,`}}.\n"
		, "With the `special` keyword, specialised instances for certain type instantiations are exported for efficiency.\n"
		, "The context of a generic function can only contain other generic functions."
		, "The generic context is written without kinds and separated by `,` as seen in the example."
		]
	, syntax_doc_locations = [CLR 8 "6.2" "_Toc311798057"]
	, syntax_examples      = map EX
		[ "add :: a a -> a | + a // a must instantiate +\nadd x y = x + y"
		, "sum :: [a] -> a | zero, + a // a must instantiate zero and +\nsum []     = zero\nsum [x:xs] = x + sum xs"
		, "(<+) infixr 5 :: a b -> String | toString a & toString b // a and b must instantiate toString\n(<+) x y = toString x +++ toString y"
		, "isMember :: a [a] -> Bool special a=Int // specialised instance for integer lists for efficiency"
		, "generic gFun a | gDefault a, gEq a :: a -> Int // generic context"
		]
	}

bs_define_constant =
	{ syntax_title         = "graph definition"
	, syntax_patterns      = map exact ["=:"]
	, syntax_code          = ["... =: ..."]
	, syntax_description   =
		"Defining constants with `=:` at the top level makes sure they are shared through out the program; hence, they are evaluated only once.\n\n" +
		"This is the default understanding of `=` in local scope.\n\n" +
		"The inverse is {{`=>`}}, which defines an identifier to be a constant function."
	, syntax_doc_locations = [CLR 5 "3.6" "_Toc311798007"]
	, syntax_examples      = [EXs "macro" "mylist =: [1..10000]"]
	}
bs_define_graph =
	{ syntax_title         = "constant function definition"
	, syntax_patterns      = map exact ["=>"]
	, syntax_code          = ["... => ..."]
	, syntax_description   =
		"Defining constants with `=>` makes sure they are interpreted as constant functions; hence, they are evaluated every time they are needed.\n\n" +
		"This is the default understanding of `=` in global scope.\n\n" +
		"The inverse is {{`=:`}}, which defines an identifier to be a graph."
	, syntax_doc_locations = [CLR 5 "3.6" "_Toc311798007"]
	, syntax_examples      = [EXs "macro" "mylist => [1..10000]"]
	}

bs_dotdot =
	{ syntax_title         = "dotdot expression"
	, syntax_patterns      = map exact ["\\[\\w\\.\\.\\]", "\\[\\w\\.\\.\\w\\]", "\\[\\w,\\w\\.\\.\\]", "\\[\\w,\\w\\.\\.\\w\\]", "dotdot", "dot-dot", "\\.\\."]
	, syntax_code          = ["[i..]", "[i..k]", "[i,j..]", "[i,j..k]"]
	, syntax_description   =
		"A shorthand for lists of enumerable types.\n\n" +
		"To use these expressions, you must import {{`StdEnum`}}. The underlying functions are defined in {{`_SystemEnum`}}."
	, syntax_doc_locations = [CLR 6 "4.2.1" "_Toc311798023"]
	, syntax_examples      = map (EXs "macro")
		[ "xs = [0..]     // 0, 1, 2, 3, ..."
		, "xs = [0,2..]   // 0, 2, 4, 6, ..."
		, "xs = [0..10]   // 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10"
		, "xs = [0,2..10] // 0, 2, 4, 6, 8, 10"
		]
	}

bs_exists =
	{ syntax_title         = "existential quantifier"
	, syntax_patterns      = map exact ["E", "E\\.*"]
	, syntax_code          = [":: ... = E. ...: ..."]
	, syntax_description   = "Existential quantifiers make it possible to define (recursive) objects of the same type with different types of content."
	, syntax_doc_locations = [CLR 7 "5.1.3" "_Toc311798042"]
	, syntax_examples      = [EX ":: List = E.e: Cons e List | Nil\nStart = Cons 5 (Cons 'a' (Cons \"abc\" Nil))"]
	}

bs_extensible_adt =
	{ syntax_title         = "extensible algebraic data type"
	, syntax_patterns      = map exact ["\\.\\.", "\\|"]
	, syntax_code          = [":: T | ...", ":: T = ... | ... | .."]
	, syntax_description   = join " "
		[ "Extensible algebraic data types are ADTs that can be extended in other modules."
		, "One module can declare the ADT as extendible by adding the `..` constructor."
		, "Other modules can then extend it."
		, "However, extensions can not be exported."
		, "It is also not possible to derive generic functions for EADTs."
		]
	, syntax_doc_locations = []
	, syntax_examples      = map EX
		[ ":: T = ..      // Declare T as an EADT"
		, ":: T = C1 | .. // Declare T to be an EADT with at least the constructor C1"
		, ":: T | C       // Extend the EADT T with constructor C"
		]
	}

bs_forall =
	{ syntax_title         = "universal quantifier"
	, syntax_patterns      = map exact ["A", "A\\.*"]
	, syntax_code          = ["A. ...:"]
	, syntax_description   = "Explicitly marks polymorphic type variables. Clean does not yet allow universal quantifiers on the topmost level."
	, syntax_doc_locations = [CLR 5 "3.7.4" "_Toc311798013"]
	, syntax_examples      = map EX
		[ "hd :: A.a: [a] -> a           // Not yet allowed: A. on the topmost level"
		, "h :: (A.a: [a] -> Int) -> Int // The quantifier is needed to apply the function to both a [Int] and a [Char]\nh f = f [1..100] + f ['a'..'z']"
		, ":: T = C (A.a: a -> a)        // In a type"
		]
	}

bs_foreign =
	{ syntax_title         = "foreign export"
	, syntax_patterns      = map exact ["foreign", "export", "ccall", "stdcall"]
	, syntax_code          =
		[ "foreign export [ccall | stdcall] ..."
		]
	, syntax_description   =
		"Exports the Clean function symbol to the binary.\n\n" +
		"This is only possible if the function is also in the definition module and all arguments are basic types or tuples and fully strict."
	, syntax_doc_locations = [CLR 13 "11.1" "_Toc311798114"]
	, syntax_examples      = map EX
		[ "foreign export factorial         // Export the factorial function"
		, "foreign export stdcall factorial // Idem but with the stdcall calling convention"
		]
	}

bs_funcdep =
	{ syntax_title         = "functional dependency"
	, syntax_patterns      = map exact ["~"]
	, syntax_code          =
		[ "class ... ~... ..."
		]
	, syntax_description   =
		"Lets you point the type checker to the type that determines the other types.\n\n" +
		"Most often this is the return type (undocumented and experimental)."
	, syntax_doc_locations = []
	, syntax_examples      = map EX
		[ "class plus a b c :: a b -> c\n" +
		  "instance plus Int Int Int where plus x y = x + y\n" +
		  "Start = plus 1 (plus 1 1) // Results in: internal overloading of \"plus\" could not be solved because the compiler doesn't know the type of the intermediate result."
		, "class plus a b ~c :: a b -> c\n" +
		  "instance plus Int Int Int where plus x y = x + y\n" +
		  "Start = plus 1 (plus 1 1) // Works! because we told the compiler that c determines the other types."
		]
	}

bs_function_definition =
	{ syntax_title         = "function definition"
	, syntax_patterns      = map exact ["="]
	, syntax_code          = ["... = ..."]
	, syntax_description   = join "\n"
		[ "Specifies the implementation of a function\n."
		, "Instead of `=`, also {{`=:`}} and {{`=>`}} may be used to separate the pattern from the right-hand side."
		, "These have different semantics."
		]
	, syntax_doc_locations = [CLR 5 "3" "_Toc311797995"]
	, syntax_examples      = map EX
		[ "map :: (a -> b) [a] -> [b]\nmap f []     = []\nmap f [x:xs] = [f x:map f xs]"
		]
	}

bs_guard =
	{ syntax_title         = "guard"
	, syntax_patterns      = map exact ["\\|", "=", "otherwise"]
	, syntax_code          = ["| ... = ...", "| otherwise = ..."]
	, syntax_description   = join "\n"
		[ "Guards let you specify function alternatives with boolean expressions."
		, "A final `otherwise` guard can be used to ensure the function is total.\n"
		, "Guards can be nested with indentation."
		, "However, only the toplevel guards may be partial.\n"
		, "To separate the guard from the alternative, both {{`=`}} and {{`=>`}} may be used (with different semantics; see {{`=>`}})."
		, "However, one must be consistent with this throughout the function."
		]
	, syntax_doc_locations = [CLR 5 "3.3" "_Toc311797998"]
	, syntax_examples      = map EX
		[ "sign :: !Int -> Int\nsign n\n| n  < 0    = -1 // Negative number\n| n == 0    =  0 // Zero\n| otherwise =  1 // Must be positive"
		]
	}

bs_generic =
	{ syntax_title         = "generic function definition"
	, syntax_patterns      = map exact ["generic", "derive", "of", "with", "\\{\\|.*\\|\\}"]
	, syntax_code          = ["generic ... ... [| ...] :: ... ", "derive ... ... [of ...] [with ...]"]
	, syntax_description   = "With generics, a function can be defined once and derived for (almost) all possible types, to avoid very similar code snippets."
	, syntax_doc_locations = [CLR 9 "7.2" "_Toc311798069"]
	, syntax_examples      =
		[ EX            "generic gEq a :: !a !a -> Bool        // The type of a generic function"
		, EXs "macro" $ "gEq{|Int|} x y = x == y               // Implementation of a generic\n" +
		  "gEq{|PAIR|} fx fy (PAIR x1 y1) (PAIR x2 y2) = fx x1 x2 && fy y1 y2"
		, EX            "derive gEq []                         // Deriving the gEq generic for type []"
		, EXs "macro"   "gConsName{|CONS of d|} _ = d.gcd_name // Using type information"
		, EX            "gFun{|CONS of {gcd_arity}|}      // Using a specific field of type information, the compiler will only provide this field which makes it a lot faster"
		, EXs "macro"   $
			"generic gFun a :: a -> Int | gDefault a  // A generic function with a generic context. The context will become an argument.\n" +
			"derive gFun CONS of {gcd_arity} with f _ // A derivation that does not use all arguments. The ompiler can optimize even more\n" +
			"gFun{|CONS of {gcd_arity}|} f _ =        // A derivation that does not use the context and only one field of the generic type descriptor, the compiler can optimize for this."
		]
	}

bs_hierarchical_modules =
	{ syntax_title         = "hierarchical module names"
	, syntax_patterns      = [exact "\\."]
	, syntax_code          = ["... . ..."]
	, syntax_description   = "Modules can be structured hierarchically. For instance, module `Control.Monad` can be found in `Control/Monad.[di]cl`."
	, syntax_doc_locations = []
	, syntax_examples      = [EX "definition module Control.Monad"]
	}

bs_import =
	{ syntax_title         = "imports"
	, syntax_patterns      = map exact ["import", "from", "qualified", "as", "=>", "code", "library"]
	, syntax_code          =
		[ "import [qualified] ... [as ...]"
		, "from ... import ..."
		, "import ... => qualified ..."
		, "import code from [library] ..."
		]
	, syntax_description   =
		"Imports code from other modules.\n\n" +
		"With the `from` keyword, one can achieve more granularity.\n\n" +
		"In case of name clashes, `qualified` can be used (undocumented).\n\n" +
		"Moreover, you can import from object files or windows DLLs."
	, syntax_doc_locations = [CLR 4 "2.5" "_Toc311797991"]
	, syntax_examples      = map EX
		[ "import StdEnv                          // Import all code from the StdEnv definition module"
		, "from StdFunc import o                  // Import only the o function from StdFunc"
		, "import qualified Data.Map              // Import Data.Map such that functions are available as e.g. 'Data.Map'.get."
		, "import qualified Data.Map as M         // Import Data.Map such that functions are available as e.g. 'M'.get."
		, "import Control.Monad => qualified join // Import all code from Control.Monad except for join. join is imported qualified"
		, "import code from \"tty.\"                // Import functions from the object file matching 'Clean System Files/tty.*'"
		, "import code from library \"msvcrt\"      // Import functions from linked DLLs according to the msvcrt file in Clean System Files.\n" +
		  "                                       // The file should start with the DLL name (e.g. msvcrt) and followed by one line per function you want to link."
		]
	}

bs_infix =
	{ syntax_title         = "infix operator"
	, syntax_patterns      = map exact ["infix", "infixl", "infixr"]
	, syntax_code          = ["infix[l,r] [...]"]
	, syntax_description   =
		"Defines a function with arity 2 that can be used in infix position.\n\n" +
		"The following number, if any, determines the precedence.\n\n" +
		"`infixl` and `infixr` indicate associativity."
	, syntax_doc_locations = [CLR 5 "3.7.2" "_Toc311798011"]
	, syntax_examples      =
		[ EX          "(bitor) infixl 6 :: !Int !Int -> Int // Left-associative infix function with precedence 6"
		, EXs "macro" "(o) infixr 9                         // Infix macro\n(o) f g :== \\x -> f (g x)"
		, EX          ":: MyType = (:+:) infixl 6 Int Int   // Infix data constructor, can be used as (5 :+: 10)"
		]
	}

bs_instance =
	{ syntax_title         = "instance"
	, syntax_patterns      = map exact ["instance"]
	, syntax_code          = ["instance ... ... where ..."]
	, syntax_description   = "Defines an instantiation of a {{class}} for a type."
	, syntax_doc_locations = [CLR 8 "6.1" "_Toc311798056"]
	, syntax_examples      = map EX
		[ "instance zero Int\nwhere\n\tzero = 0"
		, "instance zero Real\nwhere\n\tzero = 0.0"
		]
	}

bs_lambda =
	{ syntax_title         = "lambda abstraction"
	, syntax_patterns      = map exact ["lambda", "=", "->", "\\."]
	, syntax_code          = ["\\... -> ...", "\\... . ...", "\\... = ..."]
	, syntax_description   = "An anonymous, inline function."
	, syntax_doc_locations = [CLR 5 "3.4.1" "_Toc311798000"]
	, syntax_examples      = map (EXs "macro")
		[ "(o) f g = \\x -> f (g x)         // Simple lambda expression"
		, "swapall = map (\\(x,y) -> (y,x)) // Pattern matching in lambda arguments"
		, "mul     = \\x y -> x * y         // Multiple arguments (of course, it would be better to write `mul x y = x * y` or `mul = (*)`)"
		]
	}

bs_layout_rule =
	{ syntax_title         = "layout rule"
	, syntax_patterns      = map exact [";", "\\{", "\\}"]
	, syntax_code          = ["...;", "{ ... }"]
	, syntax_description   =
		"Most Clean programs are written using the layout rule, which means that scopes are indicated with indent levels." +
		"The layout sensitive mode can be turned off by adding a semicolon `;` at the end of the {{module}} line." +
		"Then, scopes have to be indicated with `{ ... }` and definitions have to end with `;`."
	, syntax_doc_locations = [CLR 4 "2.3.3" "_Toc311797989"]
	, syntax_examples      = [EX $
		"module test;\n" +
		"import StdEnv;\n" +
		"Start :: [(Int,Int)];\n" +
		"Start = [(x,y) \\\\ x <- odds, y <- evens];\n" +
		"where\n" +
		"{\n" +
		"\todds  = [1,3..9];\n" +
		"\tevens = [0,2..8];\n" +
		"}"]
	}

bs_let =
	{ syntax_title         = "let expression"
	, syntax_patterns      = map exact ["let", "in", "let in"]
	, syntax_code          = ["let ... in ..."]
	, syntax_description   = "An expression that introduces new scope."
	, syntax_doc_locations = [CLR 5 "3.5.1" "_Toc311798003"]
	, syntax_examples      =
		[ EXs "macro"    "fac n = let fs = [1:1:[(fs!!(i-1)) + (fs!!(i-2)) \\ i <- [2..]]] in fs !! n"
		, EXs "macrorhs" "let // Multi-line let expressions\n\tfunction args = body\n\tselector = expr\n\t// ...\nin expression"
		]
	}
bs_let_before =
	{ syntax_title         = "let before"
	, syntax_patterns      = map exact ["#", "#!"]
	, syntax_code          = ["#  ... = ...", "#! ... = ..."]
	, syntax_description   = "A {{`let`}} expression that can be defined before a guard or function body, which eases the syntax of sequential actions."
	, syntax_doc_locations = [CLR 5 "3.5.4" "_Toc311798006"]
	, syntax_examples      =
		[ EX "readchars :: *File -> *([Char], *File)\nreadchars f\n# (ok,c,f) = freadc file\n| not ok   = ([], f)\n# (cs,f)   = readchars f\n= ([c:cs], f)"
		]
	}

bs_list_expressions =
	{ syntax_title         = "list expression"
	, syntax_patterns      = map exact ["list", "\\[\\]", "\\[:\\]", ":", "\\[\\w:\\w\\]", "\\['.*"]
	, syntax_code          = ["[]", "[...:...]", "[..., ..., ...]", "['...']"]
	, syntax_description   =
		"A list can be composed of individual elements or a head and a tail. Special syntax is available for creating `[{{Char}}]` lists.\n\n" +
		"See also {{dotdot}} expressions.\n\n" +
		"The colon is not an operator in Clean, because it must always be surrounded by `[` and `]`. It can therefore not be curried, flipped, etc."
	, syntax_doc_locations = [CLR 6 "4.2.1" "_Toc311798021"]
	, syntax_examples      = map (EXs "macro")
		[ "abc = ['a', 'b', 'c']     // Individual elements"
		, "abc = ['a':['b':['c':[]]] // Head and tail, ending with the empty list"
		, "abc = ['abc']             // Special syntax for [Char] lists"
		, "abc ['abc':rest] = True   // The special syntax can als be used to patternmatch"
		]
	}

bs_macro =
	{ syntax_title         = "macro"
	, syntax_patterns      = map exact [":==", "macro"]
	, syntax_code          = ["... :== ..."]
	, syntax_description   =
		"A macro is a compile-time rewrite rule. It can be used for constants, inline subtitutions, renaming functions, conditional compilation, etc.\n\n" +
		"Macros can appear in patterns to match on constants."
	, syntax_doc_locations = [CLR 12 "10.3" "_Toc311798111"]
	, syntax_examples      = map (EXs "macro")
		[ "flip f a b :== f b a                    // Useful for currying"
		, "IF_INT_64_OR_32 int64 int32 :== int64   // Conditional compilation"
		, "(o) infixr 9                            // Function composition. Doing this at run-time would be slow\n(o) f g :== \\x -> f (g x)"
		]
	}

bs_module =
	{ syntax_title         = "module heading"
	, syntax_patterns      = map exact ["module", "definition", "implementation", "system", "definition module", "implementation module", "system module"]
	, syntax_code          = ["[definition,implementation,system] module ..."]
	, syntax_description   = "The heading of a Clean file. Definition modules describe what things are exported (dcl files), implementation modules how they are implemented (icl files)."
	, syntax_doc_locations = [CLR 4 "2.2" "_Toc311797983"]
	, syntax_examples      = map EX
		[ "definition module StdList     // Exported definitions of list functions"
		, "implementation module StdList // The implementations of the functions"
		, "module test                   // An implementation module without corresponding dcl"
		, "system module StdInt          // The definitions of a module that contains foreign code (see section 2.6 of the language report)"
		]
	}

bs_newtype =
	{ syntax_title         = "Newtype definition (experimental)"
	, syntax_patterns      = map exact ["=:", "newtype"]
	, syntax_code          = [":: ... =: ... ..."]
	, syntax_description   = "A newtype is a type synonym at run-time but treated as a real type at compile-time.\n"
	                       + "This allows the creation of separate instances without overhead."
	, syntax_doc_locations = []
	, syntax_examples      =
		[ EX ":: T =: T Int"
		, EX ":: T a =: T a"
		]
	}

bs_overloaded_type_variable =
	{ syntax_title         = "Overloaded type variable"
	, syntax_patterns      = map exact ["\\^", "\\w\\^"]
	, syntax_code          = ["... :: ...^"]
	, syntax_description   = "A pattern match on the type of a dynamic depending on the type of the function."
	, syntax_doc_locations = [CLR 10 "8.2.5" "_Toc311798087"]
	, syntax_examples      = [EX "unpack :: Dynamic -> Maybe a\nunpack (x :: a^) = Just x // Only values of type a\nunpack _         = Nothing"]
	}

bs_pattern_named =
	{ syntax_title         = "pattern match"
	, syntax_patterns      = map exact ["=:"]
	, syntax_code          = ["...=:(...)"]
	, syntax_description   = "Give a name to the expression of a pattern to be able to use the whole expression without creating new graphs."
	, syntax_doc_locations = [CLR 5 "3.2" "_Toc311797997"]
	, syntax_examples      = map EX
		[ "isJustU e=:(Just _) = (True, e) // On an ADT"
		, ":: Position = {px :: Int, py :: Int}\ngetx p=:{px} = (px, p) // On a record; this has type :: Position -> (Int, Position)"
		]
	}

bs_pattern_predicate =
	{ syntax_title         = "pattern predicate"
	, syntax_patterns      = map exact ["=:"]
	, syntax_code          = ["...=:(...)"]
	, syntax_description   = join " "
		[ "Check whether an expression matches a certain pattern (undocumented)."
		, "The result has type `Bool`."
		, "It is not possible to introduce new identifiers this way."
		, "For instance, one cannot use `if (mbx=:(Just x)) x 0`."
		, "Also, `=:` can not be used in prefix form because it is not an actual operator but a builtin."
		]
	, syntax_doc_locations = []
	, syntax_examples      = [EX "isSingleton l = l =: [_] // Match a value with a pattern"]
	}

bs_qualified_identifier =
	{ syntax_title         = "qualified identifier"
	, syntax_patterns      = map regex ["^'.+'", "^qualified$"]
	, syntax_code          = ["'...'. ..."]
	, syntax_description   = "The identifiers of {{`qualified`}} imports must be prepended with `'...'.`, where `...` is the name of the qualified import."
	, syntax_doc_locations = []
	, syntax_examples      = map EX
		[ "import qualified StdList\nStart = 'StdList'.sum [0..10]"
		, "import qualified StdList as L\nStart = 'L'.sum [0..10]"
		]
	}

bs_record_disambiguation =
	{ syntax_title         = "record disambiguation"
	, syntax_patterns      = map exact ["\\|"]
	, syntax_code          = ["{ ... | ... }"]
	, syntax_description   = join "\n"
		[ "Explicitly indicates the type of a record when it cannot be derived from the field names."
		]
	, syntax_doc_locations = []
	, syntax_examples      = map EX
		[ ":: R1 = {x :: Int, y :: Int}\n:: R2 = {x :: Int, y :: Int}\nStart = {R1 | x=37, y=42}"
		]
	}

bs_selection_array =
	{ syntax_title         = "array selection"
	, syntax_patterns      = map exact ["\\.\\[\\]", "\\.\\[.*\\]", "\\.\\[,.*\\]", "\\.\\[.*,.*\\]"]
	, syntax_code          = [".[i]", ".[i,j,...]"]
	, syntax_description   = "Select an element from a (possibly multidimensional) array. The indexes must have the type {{`Int`}}."
	, syntax_doc_locations = [CLR 6 "4.4.1" "_Toc311798033"]
	, syntax_examples      = map (EXs "macro")
		[ "five = {1,2,3,4,5,6,7,8,9,10}.[4]    // Arrays are zero-indexed"
		, "five = {{1,2},{3,4,5},{6,7,8}}.[1,2] // This is equivalent to (...).[1].[2]"
		]
	}
bs_selection_array_unique =
	{ syntax_title         = "unique array selection"
	, syntax_patterns      = map exact ["!\\[\\]", "!\\[.*\\]", "!\\[,.*\\]", "!\\[.*,.*\\]"]
	, syntax_code          = ["![i]", "![i,j,...]"]
	, syntax_description   = "Select an element from a (possibly multidimensional, possibly unique) array and return both the element and the array. The indexes must have the type {{`Int`}}."
	, syntax_doc_locations = [CLR 6 "4.4.1" "_Toc311798033"]
	, syntax_examples      = map (EXs "macro")
		[ "(five,arr) = {1,2,3,4,5,6,7,8,9,10}![4]"
		, "(five,arr) = {{1,2},{3,4,5},{6,7,8}}![1,2]"
		]
	}
bs_selection_record =
	{ syntax_title         = "record selection"
	, syntax_patterns      = map exact ["\\."]
	, syntax_code          = ["."]
	, syntax_description   = "Select a field from a (possibly multilevel) record."
	, syntax_doc_locations = [CLR 7 "5.2.1" "_Toc311798050"]
	, syntax_examples      = map (EXs "macro")
		[ "five = {px=5, py=10}.px"
		, "five = {pxy={px=5, py=10}, pz=2}.pxy.px"
		, "five = {px=5, py=10}.Position.px // If multiple records have a field px, the type name can be used for disambiguation"
		]
	}
bs_selection_record_unique =
	{ syntax_title         = "unique record selection"
	, syntax_patterns      = map exact ["!"]
	, syntax_code          = ["!"]
	, syntax_description   = "Select a field from a (possibly multilevel, possibly unique) record and return both the field data and the record."
	, syntax_doc_locations = [CLR 7 "5.2.1" "_Toc311798050"]
	, syntax_examples      = map (EXs "macro")
		[ "(five,rec) = {px=5, py=10}!px"
		, "(five,rec) = {pxy={px=5, py=10}, pz=2}!pxy.px // Only the first field should have the exclamation mark"
		, "(five,rec) = {px=5, py=10}!Position.px // If multiple records have a field px, the type name can be used for disambiguation\n" +
		  "                                       // The language report is erroneous here. It is !Position.px, not .Position!px."
		]
	}

bs_strict =
	{ syntax_title         = "strictness annotation"
	, syntax_patterns      = map exact ["strict", "!"]
	, syntax_code          = ["!"]
	, syntax_description   = "Override the lazy evaluation strategy: the argument must be evaluated to head normal form before the function is entered."
	, syntax_doc_locations = [CLR 5 "3.7.5" "_Toc311798014", CLR 12 "10" "_Toc311798103"]
	, syntax_examples      = [EX "acker :: !Int !Int -> Int"]
	}

bs_synonym =
	{ syntax_title         = "synonym type definition"
	, syntax_patterns      = map exact ["synonym", ":=="]
	, syntax_code          = [":: ... :== ..."]
	, syntax_description   = "Defines a new type name for an existing type."
	, syntax_doc_locations = [CLR 7 "5.3" "_Toc311798052"]
	, syntax_examples      = [EX ":: String :== {#Char}"]
	}
bs_synonym_abstract =
	{ syntax_title         = "abstract synonym type definition"
	, syntax_patterns      = map exact ["synonym", ":=="]
	, syntax_code          = [":: ... (:== ...)"]
	, syntax_description   = "Defines a new type name for an existing type, while the type behaves as an abstract type for the programmer. This allows compiler optimisations on abstract types."
	, syntax_doc_locations = [CLR 7 "5.4.1" "_Toc311798054"]
	, syntax_examples      = [EX ":: Stack a (:== [a])"]
	}

bs_type_definition =
	{ syntax_title         = "type definition"
	, syntax_patterns      = map exact ["::", "=", "\\|"]
	, syntax_code          = [":: ..."]
	, syntax_description   = "Defines a new type. There are too many possibilities to list hear; see the documentation."
	, syntax_doc_locations = [CLR 7 "5" "_Toc311798038"]
	, syntax_examples      = map EX
		[ join "\n\t" [":: Day // An algebraic data type","= Mon","| Tue","| Wed","| Thu","| Fri","| Sat","| Sun"]
		, ":: Position = // A record type\n\t{ x :: Int\n\t, y :: Int\n\t}"
		]
	}

bs_type_specification =
	{ syntax_title         = "type specification"
	, syntax_patterns      = map exact ["::"]
	, syntax_code          = ["... :: ..."]
	, syntax_description   = "Specifies the type of a function."
	, syntax_doc_locations = [CLR 5 "3.7" "_Toc311798009"]
	, syntax_examples      = map EX
		[ "map :: (a -> b) [a] -> [b] // map has arity 2\nmap f []     = []\nmap f [x:xs] = [f x:map f xs]"
		, "map :: (a -> b) -> [a] -> [b] // map has arity 1\nmap f = \\xs -> case xs of\n\t[]    -> []\n\t[x:xs] -> [f x:map f xs]"
		]
	}

bs_unique =
	{ syntax_title         = "uniqueness annotation"
	, syntax_patterns      = map exact ["\\*", "\\.", "\\w:", "\\[.*<=.*\\]", ",", "<="]
	, syntax_code          =
		[ "*..."
		, ". ..."
		, "...:..., [...<=...], [...<=...], ..."
		]
	, syntax_description   = join " "
		[ "Annotates a type with its uniqueness."
		, "A type can either be unique (`*`), not unique (not annotated), possibly unique (`.`) or relatively unique (identifier and `, [...<=...]` after the type context)."
		, "Because uniqueness is binary, there is only one case where `[u<=v]` is not satisfied; when `u` is unique but `v` is not."
		, "\n\nOn function types, *uniqueness propagation* is implicit (see section 9.2 of the language report)."
		, "However, when using unique types in a function or an ADT this has to be made explicit; for instance:"
		, "`T = T (Int, *File)` has to be `T = T *(Int, *File)`."
		, "Functions have to be split up into arity 1 and the subfunctions must be annotated as well; for instance:"
		, "`T = T (Int *Int -> *Int)` has to be `T = T (Int -> *(*Int -> *Int))`."
		]
	, syntax_doc_locations = [CLR 11 "9.1" "_Toc311798093"]
	, syntax_examples      = map EX
		[ "Start :: *World -> *World   // World is unique"
		, "copyArray :: *(a e) -> *(*a e, *a e) | Array a e // Add parentheses when needed"
		, "f :: .a -> .a               // f works on unique and non-unique values"
		, "f :: v:a u:b -> u:b, [v<=u] // f works when a is less unique than b"
		]
	}

bs_update_array =
	{ syntax_title         = "array update"
	, syntax_patterns      = map exact ["&", "\\{.*&.*\\[.*].*=.*\\}"]
	, syntax_code          =
		[ "{ a & [i]=x, [j]=y, ... }        // Updates a by setting index i to x, j to y, ..."
		, "# a & [i]=x, [j]=y, ...          // Same as # a = {a & [i]=x, [j]=y, ...}" // See https://clean.cs.ru.nl/Clean_2.3
		, "{ a & [i]=i \\\\ i <- [0,2..9] } // Fancy things can be done if you combine this with a ZF-expression. E.g. give the first 5 even indices the value of their index"
		]
	, syntax_description   = "Updates an array by creating a copy and replacing one or more elements."
	, syntax_doc_locations = [CLR 6 "4.4.1" "_Toc311798032"]
	, syntax_examples      = []
	}
bs_update_record =
	{ syntax_title         = "record update"
	, syntax_patterns      = map exact ["&", "\\|", "\\{.*&.*=.*\\}"]
	, syntax_code          =
		[ "{ r & f1=x, f2=y, ... } // Updates r by setting f1 to x, f2 to y, ..."
		, "{ MyRecord | r & f1=x, f2=y, ... } // explicitly stating the type"
		, "# r & f1=x, f2=y, ...   // Same as # r = {r & f1=x, f2=y, ...}" // See https://clean.cs.ru.nl/Clean_2.3
		]
	, syntax_description   = "Updates a record by creating a copy and replacing one or more fields."
	, syntax_doc_locations = [CLR 7 "5.2.1" "_Toc311798049"]
	, syntax_examples      = []
	}

bs_where_class =
	{ syntax_title         = "where"
	, syntax_patterns      = map exact ["where"]
	, syntax_code          = ["where"]
	, syntax_description   = "Introduces the members of a {{`class`}} definition."
	, syntax_doc_locations = [CLR 8 "6.1"   "_Toc311798056"]
	, syntax_examples      = [EX "class Arith a        // Class definition\nwhere\n\t(+) infixl 6 :: a a -> a\n\t(-) infixl 6 :: a a -> a"]
	}
bs_where_instance =
	{ syntax_title         = "where"
	, syntax_patterns      = map exact ["where"]
	, syntax_code          = ["where"]
	, syntax_description   = "Introduces the implementation of an {{`instance`}}."
	, syntax_doc_locations = [CLR 8 "6.1"   "_Toc311798056"]
	, syntax_examples      = [EX "instance Arith Int   // Instance definition\nwhere\n\t(+) x y = // ...\n\t(-) x y = // ..."]
	}
bs_where_local =
	{ syntax_title         = "where"
	, syntax_patterns      = map exact ["where"]
	, syntax_code          = ["where"]
	, syntax_description   = "Introduces local definitions. For guard-local definitions, see {{`with`}}."
	, syntax_doc_locations = [CLR 5 "3.5.2" "_Toc311798004"]
	, syntax_examples      = [EXs "macro" "primes = sieve [2..] // Local definitions\nwhere\n\tsieve [pr:r] = [pr:sieve (filter pr r)]"]
	}

bs_with =
	{ syntax_title         = "with"
	, syntax_patterns      = map exact ["with"]
	, syntax_code          = ["with"]
	, syntax_description   = "Introduces guard-local definitions. For function-local definitions, see {{`where`}}."
	, syntax_doc_locations = [CLR 5 "3.5.3" "_Toc311798005"]
	, syntax_examples      = [EXs "macro" "f x y\n| guard1 = alt1\n\twith local = expr1\n| guard2 = alt2\n\twith local = expr2"]
	}

bs_zf =
	{ syntax_title         = "list comprehension"
	, syntax_patterns      = map exact ["ZF-expression", "ZF", "zf", "*comprehension", "<-", "<\\|-", "<-:", "\\\\\\\\", ",", "&", "\\|"]
	, syntax_code          =
		[ "[... \\\\ ... <- ...]"
		, "{... \\\\ ... <- ...}"
		]
	, syntax_description   = "Constructs a list or array composed of elements drawn from other lists or arrays."
	, syntax_doc_locations = [CLR 6 "4.2.1" "_Toc311798024", CLR 6 "4.4.1" "_Toc311798032"]
	, syntax_examples      = map (EXs "macro")
		[ "cartesian    = [(x,y) \\\\ x <- [1,2,3], y <- [10,20]] // Cartesian product: (1,10), (1,20), (2,10), (2,20), (3,10), (3,20)"
		, "zip xs ys    = [(x,y) \\\\ x <- xs & y <- ys]          // Pairwise zip through the lists"
		, "filter f xs  = [x \\\\ x <- xs | f x]                  // Guard to add conditions"
		, "catMaybes ms = [x \\\\ Just x <- ms]                   // Pattern matching in the selector"
		, "triangle     = [(x,y) \\\\ x <- [1,2,3], y <- [1..x]]  // Reusing x in the next generator: (1,1), (2,1), (2,2), (3,1), (3,2), (3,3)"
		, "arrToList a  = [x \\\\ x <-: a]                        // <-: to draw elements from an array"
		, "listToArr l  = {x \\\\ x <- l}                         // {..} to create an array"
		, "castList xs  = [|x \\\\ x <|- xs]                      // The two pipe characters make both xs and the result overloaded lists"
		]
	}
