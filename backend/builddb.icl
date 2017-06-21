module builddb

import StdArray
import StdBool
import StdFile
from StdFunc import const, o
import StdList
import StdMisc
import StdString
import StdTuple

import Data.Either
from Data.Func import $, mapSt
import Data.Functor
import Data.Maybe
import System.CommandLine
from Text import class Text(concat,startsWith), instance Text String

import TypeDB
import Type
from TypeDBFactory import findModules, getModuleTypes, constructor_functions,
	record_functions

:: CLI = { help    :: Bool
         , version :: Bool
         , root    :: String
         , libs    :: [(String, String -> Bool)]
         , exclude :: [String]
         }

instance zero CLI where
	zero = { version = False
	       , help    = False
	       , root    = "/opt/clean/lib/"
	       , libs    = [ ("ArgEnv", const False)
	                   , ("CleanInotify", const False)
	                   , ("CleanPrettyPrint", const False)
	                   , ("CleanSerial", const False)
	                   , ("CleanSnappy", const False)
	                   , ("CleanTypeUnifier", const False)
	                   , ("Cloogle", const False)
	                   , ("Directory", const False)
	                   , ("Dynamics", const False)
	                   , ("Gast", const False)
	                   , ("Generics", const False)
	                   , ("GraphCopy", const False)
	                   , ("MersenneTwister", const False)
	                   , ("ObjectIO", not o startsWith "Std")
	                   , ("Platform", const False)
	                   , ("Sapl", const False)
	                   , ("SoccerFun", const False)
	                   , ("StdEnv", const False)
	                   , ("StdLib", const False)
	                   , ("TCPIP", const False)
	                   , ("iTasks", const False)
	                   , ("libcloogle", const False)
	                   ]
	       , exclude = [ "StdEnv/_startup"
	                   , "StdEnv/_system"
	                   , "Platform/Deprecated"
	                   , "Platform/Data/Graph/Inductive/Query"
	                   , "SoccerFun/RefereeCoach_"
	                   , "SoccerFun/Team_"
	                   , "Cloogle/compiler-patch"
	                   ]
	       }


VERSION :== "Cloogle's builddb version 0.1\n"
USAGE :== concat [
	VERSION, "\n",
	"Usage: ./builddb [opts] > types.json\n\n",
	"\t-h, --help Show this help\n",
	"\t-r PATH    Change the library root to PATH\n",
	"\t-l PATH    Add PATH to the librarypaths relative to the root\n"]

Start :: *World -> *World
Start w
# (args, w) = getCommandLine w
# (f, w) = stdio w
# (ok, w) = case parseCLI (tl args) of
	(Left e) = fclose (f <<< e) w
	(Right cli)
	| cli.help    = fclose (f <<< USAGE) w
	| cli.version = fclose (f <<< VERSION) w
	# (modss, w)  = mapSt (flip (uncurry $ findModules cli.exclude cli.root) "") cli.libs w
	# mods        = flatten modss
	#! (db, w)    = loop cli.root mods newDb w
	#! db         = putFunctions predefFunctions db
	#! db         = putClasses predefClasses db
	#! db         = putTypes predefTypes db
	#! db         = putFunctions (flatten $ map constructor_functions predefTypes) db
	#! db         = putFunctions (flatten $ map record_functions predefTypes) db
	# io          = stderr
	# io          = printStats db io
	# (ok1,w)     = fclose io w
	# f           = saveDb db f
	# (ok2,w)     = fclose f w
	= (ok1 && ok2,w)
| not ok = abort "Couldn't close stdio"
= w
where
	loop :: String [(String,String,Bool)] !TypeDB !*World -> *(!TypeDB, !*World)
	loop _ [] db w = (db,w)
	loop root [(lib,mod,iscore):list] db w
	#! w = snd (fclose (stderr <<< lib <<< ": " <<< mod <<< "\n") w)
	#! (db, w) = getModuleTypes root mod lib iscore db w
	= loop root list db w

	parseCLI :: [String] -> Either String CLI
	parseCLI [] = Right zero
	parseCLI [x:a] = case (x,a) of
		("--help", xs) = (\c->{c & help=True}) <$> parseCLI xs
		("--version", xs) = (\c->{c & version=True}) <$> parseCLI xs
		("-l", []) = Left "'-l' requires an argument"
		("-r", []) = Left "'-r' requires an argument"
		("-r", [x:xs]) = (\c->{c & root=x}) <$> parseCLI xs
		("-l", [x:xs]) = (\c->{c & libs=[(x,const False):c.libs]}) <$> parseCLI xs
		(x, _) = Left $ "Unknown option '" +++ x +++ "'"

	printStats :: !TypeDB !*File -> *File
	printStats db f = f
		<<< "+-------------+------+\n"
		<<< "| Modules     | " <<< modules <<< " |\n"
		<<< "| Functions   | " <<< funs    <<< " |\n"
		<<< "| Types       | " <<< types   <<< " |\n"
		<<< "| Macros      | " <<< macros  <<< " |\n"
		<<< "| Classes     | " <<< classes <<< " |\n"
		<<< "| Instances   | " <<< insts   <<< " |\n"
		<<< "| Derivations | " <<< derives <<< " |\n"
		<<< "+-------------+------+\n"
	where
		[modules,funs,macros,types,classes,insts,derives:_]
			= map (pad 4)
				[ moduleCount db
				, functionCount db
				, macroCount db
				, typeCount db
				, classCount db
				, instanceCount db
				, deriveCount db
				]
		pad n i = {' ' \\ _ <- [0..n-size (toString i)-1]} +++ toString i

predefFunctions :: [(Location, ExtendedType)]
predefFunctions
	= [ ( Builtin "if"
	    , ET (Func [Type "Bool" [], Var "a", Var "a"] (Var "a") []) zero
	    )
	  , ( Builtin "dynamic"
	    , ET (Func [Var "a"] (Type "Dynamic" []) []) zero
	    )
	  ]

predefClasses :: [(Location, Class)]
predefClasses
	= [ ( Builtin "TC"
	    , { class_vars=["v"]
	      , class_context=[]
	      , class_documentation=Nothing
	      , class_members=[]
	      }
	    )
	  ]

predefTypes :: [(Location, ExtendedTypeDef)]
predefTypes
	= [ ( Builtin "Bool"
	    , { deft
	      & etd_typedef.td_name = "Bool"
	      , etd_typedef.td_rhs  = TDRCons False
	        [ { defc & cons_name="False" }
	        , { defc & cons_name="True" }
	        ]
	      }
	    )
	  , ( Builtin "Int",     { deft & etd_typedef.td_name = "Int"     } )
	  , ( Builtin "Real",    { deft & etd_typedef.td_name = "Real"    } )
	  , ( Builtin "Char",    { deft & etd_typedef.td_name = "Char"    } )
	  , ( Builtin "String",  { deft & etd_typedef.td_name = "String",
	      etd_typedef.td_rhs = TDRSynonym (Type "_#Array" [Type "Char" []]) } )
	  , ( Builtin "Dynamic", { deft & etd_typedef.td_name = "Dynamic" } )
	  , ( Builtin "File",    { deft & etd_typedef.td_name = "File"    } )
	  , ( Builtin "World",   { deft & etd_typedef.td_name = "World",
	      etd_typedef.td_uniq = True } )
	  ]
where
	deft = {etd_typedef={td_name="", td_uniq=False, td_args=[], td_rhs=TDRAbstract}, etd_doc=Nothing}
	defc = {cons_name="", cons_args=[], cons_exi_vars=[], cons_context=[], cons_priority=Nothing}
