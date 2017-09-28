module builddb

import StdArray
import StdBool
import StdFile
from StdFunc import const, id, o
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

import CloogleDB
import Type
from CloogleDBFactory import :: TemporaryDB, newTemporaryDb, finaliseDb,
	findModules, indexModule, constructor_functions, record_functions

import Builtins

:: CLI = { help    :: Bool
         , version :: Bool
         , root    :: String
         , libs    :: [(String, String ModuleEntry -> ModuleEntry)]
         , exclude :: [String]
         }

instance zero CLI where
	zero = { version = False
	       , help    = False
	       , root    = "/opt/clean/lib/"
	       , libs    = [ ("ArgEnv",           const id)
	                   , ("CleanInotify",     const id)
	                   , ("CleanPrettyPrint", const id)
	                   , ("CleanSerial",      const id)
	                   , ("CleanSnappy",      const id)
	                   , ("CleanTypeUnifier", const id)
	                   , ("Cloogle",          const id)
	                   , ("Directory",        const id)
	                   , ("Dynamics",         const id)
	                   , ("Gast",             const id)
	                   , ("Generics",         const id)
	                   , ("GraphCopy",        const id)
	                   , ("MersenneTwister",  const id)
	                   , ("ObjectIO",         \s me -> {me & me_is_core=not (startsWith "Std" s)})
	                   , ("Platform",         const id)
	                   , ("Sapl",             const id)
	                   , ("SoccerFun",        const id)
	                   , ("StdEnv",           const id)
	                   , ("StdLib",           const id)
	                   , ("TCPIP",            const id)
	                   , ("iTasks",           const id)
	                   , ("clean-compiler",   const \me -> {me & me_is_app=True})
	                   , ("clean-ide",        const \me -> {me & me_is_app=True})
	                   , ("libcloogle",       const id)
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
	#! (db, w)    = loop cli.root mods newTemporaryDb w
	#! db         = finaliseDb db newDb
	#! db         = putFunctions builtin_functions db
	#! db         = putClasses builtin_classes db
	#! db         = putTypes builtin_types db
	#! db         = putFunctions (flatten $ map constructor_functions builtin_types) db
	#! db         = putFunctions (flatten $ map record_functions builtin_types) db
	#! db         = putSyntaxElems builtin_syntax db
	#! db         = syncDb 2 db
	#! (ok1,w)    = fclose (printStats db stderr) w
	#! f          = saveDb db f
	#! (ok2,w)    = fclose f w
	= (ok1 && ok2,w)
| not ok = abort "Couldn't close stdio"
= w
where
	loop :: String [(String,String,String ModuleEntry -> ModuleEntry)] !TemporaryDB !*World -> *(!TemporaryDB, !*World)
	loop _ [] db w = (db,w)
	loop root [(lib,mod,modf):list] db w
	#! w = snd (fclose (stderr <<< lib <<< ": " <<< mod <<< "\n") w)
	#! (db, w) = indexModule root mod lib modf db w
	#! db = eval_all_nodes db
	= loop root list db w

	eval_all_nodes :: !.a -> .a // From GraphCopy
	eval_all_nodes g = code {
		push_a 0
		.d 1 0
		jsr	_eval_to_nf
		.o 0 0
	}

	parseCLI :: [String] -> Either String CLI
	parseCLI [] = Right zero
	parseCLI [x:a] = case (x,a) of
		("--help", xs) = (\c->{c & help=True}) <$> parseCLI xs
		("--version", xs) = (\c->{c & version=True}) <$> parseCLI xs
		("-l", []) = Left "'-l' requires an argument"
		("-r", []) = Left "'-r' requires an argument"
		("-r", [x:xs]) = (\c->{c & root=x}) <$> parseCLI xs
		("-l", [x:xs]) = (\c->{c & libs=[(x,const id):c.libs]}) <$> parseCLI xs
		(x, _) = Left $ "Unknown option '" +++ x +++ "'"

	printStats :: !CloogleDB !*File -> *File
	printStats db f = f
		<<< "+-------------------+-------+\n"
		<<< "| Modules           | " <<< modules <<< " |\n"
		<<< "| Functions         | " <<< funs    <<< " |\n"
		<<< "| Types             | " <<< types   <<< " |\n"
		<<< "| Classes           | " <<< classes <<< " |\n"
		<<< "| Derivations       | " <<< derives <<< " |\n"
		<<< "| Syntax constructs | " <<< syntaxs <<< " |\n"
		<<< "+-------------------+-------+\n"
	where
		[modules,funs,types,classes,derives,syntaxs:_]
			= map (pad 5)
				[ moduleCount db
				, functionCount db
				, typeCount db
				, classCount db
				, deriveCount db
				, syntaxCount db
				]
		pad n i = {' ' \\ _ <- [0..n-size (toString i)-1]} +++ toString i
