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
import Data.List
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
	                   , ("SoccerFun",        const \me -> {me & me_is_app=True})
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
	#! db         = finaliseDb builtins db
	#! (db,err)   = printStats db stderr
	#! (ok1,w)    = fclose err w
	#! (db,f)     = saveDB db f
	#! (ok2,w)    = fclose f w
	#! (_,dbg,w)  = fopen "typetree.dot" FWriteText w
	#! (db,dbg)   = writeTypeTree db dbg
	#! (_,w)      = fclose dbg w
	= (ok1 && ok2,w)
| not ok = abort "Couldn't close stdio"
= w
where
	loop :: String [(String,String,String ModuleEntry -> ModuleEntry)] !TemporaryDB !*World -> *(!TemporaryDB, !*World)
	loop _ [] db w = (db,w)
	loop root [(lib,mod,modf):list] db w
	#! w = snd (fclose (stderr <<< lib <<< ": " <<< mod <<< "\n") w)
	#! (db, w) = indexModule False root mod lib modf db w
	#! db = eval_all_nodes db
	= loop root list db w

	builtins =
		map FunctionEntry builtin_functions ++
		map ClassEntry builtin_classes ++
		map TypeDefEntry builtin_types ++
		map FunctionEntry (concatMap constructor_functions builtin_types) ++
		map FunctionEntry (concatMap record_functions builtin_types) ++
		map SyntaxEntry builtin_syntax

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

	printStats :: !*CloogleDB !*File -> *(*CloogleDB, *File)
	printStats db f
	# (s,db) = dbStats db
	= (db, f
		<<< "+-------------------+-------+\n"
		<< "Modules"           <-- s.n_modules
		<< "Functions"         <-- s.n_functions
		<< "With types"        <-- s.n_functions_with_type
		<< "Unique types"      <-- s.n_unique_types
		<< "Type tree depth"   <-- s.type_tree_depth
		<< "Type definitions"  <-- s.n_type_definitions
		<< "Classes"           <-- s.n_classes
		<< "Instances"         <-- s.n_instances
		<< "Derivations"       <-- s.n_derivations
		<< "Syntax constructs" <-- s.n_syntax_constructs
		<<< "+-------------------+-------+\n")
	where
		(<<) infixl :: *File String -> *File
		(<<) f s = f <<< "| " <<< rpad 17 s <<< " | "

		(<--) infixl :: *File Int -> *File
		(<--) f i = f <<< lpad 5 i <<< " |\n"

		lpad n s = {' ' \\ _ <- [0..n-size (toString s)-1]} +++ toString s
		rpad n s = toString s +++ {' ' \\ _ <- [0..n-size (toString s)-1]}
