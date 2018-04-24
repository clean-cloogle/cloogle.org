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

CLR :: Int String String -> CleanLangReportLocation
CLR f sec h =
	{ clr_version = v
	, clr_file    = "CleanRep." + v + "_" <+ f <+ ".htm"
	, clr_section = sec
	, clr_heading = h
	}
where v = "2.2"

builtin_functions :: [FunctionEntry]
builtin_functions =
	[ { zero
	  & fe_loc=Builtin "if" [CLR 5 "3.4.2" "_Toc311798001"]
	  , fe_type=Just $ Func [Type "Bool" [], Var "a", Var "a"] (Var "a") []
	  }
	, { zero
	  & fe_loc=Builtin "dynamic" [CLR 10 "8.1" "_Toc311798076"]
	  , fe_type=Just $ Func [Var "a"] (Type "Dynamic" []) [Instance "TC" [Var "a"]]
	  }
	]

builtin_classes :: [ClassEntry]
builtin_classes =
	[ { ce_loc=Builtin "TC" [CLR 10 "8.1.4" "_Toc311798080"]
	  , ce_vars=["v"]
	  , ce_is_meta=False
	  , ce_context=[]
	  , ce_documentation=Nothing
	  , ce_members=[]
	  , ce_instances=[]
	  , ce_derivations=[]
	  , ce_usages=[]
	  }
	]

builtin_types :: [TypeDefEntry]
builtin_types =
	[ { deft
	  & tde_loc=Builtin "Bool" [CLR 6 "4.1" "_Toc311798017"]
	  , tde_typedef.td_name = "Bool"
	  , tde_typedef.td_rhs  = TDRCons False
	    [ { defc & cons_name="False" }
	    , { defc & cons_name="True" }
	    ]
	  }
	, { deft & tde_loc=Builtin "Int"     [CLR 6 "4.1" "_Toc311798017"], tde_typedef.td_name = "Int"}
	, { deft & tde_loc=Builtin "Real"    [CLR 6 "4.1" "_Toc311798017"], tde_typedef.td_name = "Real"}
	, { deft & tde_loc=Builtin "Char"    [CLR 6 "4.1" "_Toc311798017"], tde_typedef.td_name = "Char"}
	, { deft & tde_loc=Builtin "Dynamic" [CLR 10 "8"  "_Toc311798077"], tde_typedef.td_name = "Dynamic"}
	, { deft & tde_loc=Builtin "File"    [CLR 6 "4.7" "_Toc311798037"], tde_typedef.td_name = "File"}
	, { deft
	  & tde_loc=Builtin "String" [CLR 6 "4.7" "_Toc311798037"]
	  , tde_typedef.td_name = "String"
	  , tde_typedef.td_rhs = TDRSynonym (Type "_#Array" [Type "Char" []]) }
	, { deft
	  & tde_loc=Builtin "World" [CLR 6 "4.7" "_Toc311798037"], tde_typedef.td_name = "World"
	  , tde_typedef.td_uniq = True
	  , tde_doc = Just
	    { TypeDoc | gDefault{|*|}
	    & description = Just "An object of this type is automatically created when the program is started, if needed. It makes efficient interfacing with the outside world possible. Its value is always `65536`."
	    }
	  }
	, { deft
	  & tde_loc=Builtin "->" [CLR 6 "4.6" "_Toc311798036"]
	  , tde_typedef.td_name = "(->)"
	  , tde_typedef.td_args = [Var "a", Var "b"]
	  , tde_doc = Just
	    { TypeDoc | gDefault{|*|}
	    & description = Just "The arrow type is used to denote functions.\n\nOften, function types can be written in an uncurried fashion, e.g. `a b -> c` is the same as `a -> b -> c`."
	    , vars = ["The argument type", "The result type"]
	    }
	  }
	, { deft
	  & tde_loc=Builtin "()" []
	  , tde_typedef.td_name="_Unit"
	  , tde_doc = Just
	    { TypeDoc | gDefault{|*|}
	    & description = Just "The void / unit type."
	    }
	  , tde_typedef.td_rhs = TDRCons False [{defc & cons_name="()"}]
	  }
	:  lists
	++ arrays
	++ tuples
	]
where
	deft =
		{ tde_loc=zero
		, tde_typedef=
			{ td_name=""
			, td_uniq=False
			, td_args=[]
			, td_rhs=TDRAbstract Nothing
			}
		, tde_doc=Nothing
		, tde_instances=[]
		, tde_derivations=[]
		, tde_usages=[]
		}
	defc =
		{ cons_name=""
		, cons_args=[]
		, cons_exi_vars=[]
		, cons_context=[]
		, cons_priority=Nothing
		}

	lists = [make_list kind spine \\ kind <- [[], ['#'], ['!'], ['|']], spine <- [[], ['!']] | kind <> ['|'] || spine <> ['!']]
	where
		make_list :: [Char] [Char] -> TypeDefEntry
		make_list k s =
			{ deft
			& tde_loc = Builtin higherorder [CLR 6 "4.2" "_Toc311798019"]
			, tde_typedef.td_name = toString (['_':k] ++ ['List'] ++ s)
			, tde_typedef.td_args = [Var "a"]
			, tde_doc = Just
				{ TypeDoc | gDefault{|*|}
				& description = Just $ "A" + kind + spine + " list.\n\n" + description
				, vars = ["The type of the list elements."]
				}
			}
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
		make_array :: [Char] -> TypeDefEntry
		make_array k =
			{ deft
			& tde_loc = Builtin typec [CLR 6 "4.4" "_Toc311798029"]
			, tde_typedef.td_name = toString (['_':k] ++ ['Array'])
			, tde_typedef.td_args = [Var "a"]
			, tde_doc = Just
				{ TypeDoc | gDefault{|*|}
				& description = Just $ "An array contains a finite number of elements of the same type. Access time is constant.\n\n" + description
				, vars = ["The type of the array elements."]
				}
			}
		where
			typec = toString (['{':k]++['}'])

			description = "These types of array are available:\n\n" +
				"- `{a}`, a normal array\n" +
				"- `{#a}`, an unboxed strict array (elements are stored directly, without pointers)\n" +
				"- `{!a}`, a strict array (the elements are in root normal form)"

	tuples = [make_tuple n \\ n <- [2..32]]
	where
		make_tuple :: Int -> TypeDefEntry
		make_tuple n =
			{ deft
			& tde_loc = Builtin typec [CLR 6 "4.3" "_Toc311798026"]
			, tde_typedef.td_name = "_Tuple" <+ n
			, tde_typedef.td_args = [Var $ toString [v:repeatn (n / 26) '`'] \\ v <- cycle ['a'..'z'] & n <- [0..n-1]]
			, tde_doc = Just
				{ TypeDoc | gDefault{|*|}
				& description = Just $ article + " " + ary + "ary tuple.\n\n" +
					"Tuples allow bundling a finite number of expressions of different types into one object without defining a new data type.\n\n" +
					"Clean supports tuples of arity 2 to 32."
				}
			}
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
