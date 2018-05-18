module builddb

import StdArray
import StdBool
import StdFile
from StdFunc import const, flip, id, o
import StdList
import StdMisc
import StdString
import StdTuple

import Control.Monad => qualified join
import Data.Either
import Data.Error
from Data.Func import $, hyperstrict, mapSt
import Data.Functor
import Data.List
import Data.Maybe
import System.CommandLine
import System.File
from Text import class Text(join,startsWith), instance Text String
import Text.GenJSON

import Clean.Types
import Cloogle.DB
from Cloogle.DB.Factory import :: TemporaryDB, newTemporaryDB, finaliseDB,
	findModules, indexModule, constructor_functions, record_functions,
	:: IndexItem, :: SourceURL, :: PathPattern

import Builtin.ABC
import Builtin.Predef
import Builtin.Syntax

:: CLI =
	{ help      :: !Bool
	, root      :: !String
	, libs_file :: !String
	}

derive JSONDecode IndexItem, SourceURL, PathPattern

instance zero CLI
where
	zero =
		{ help      = False
		, root      = "/opt/clean/lib/"
		, libs_file = "libs.json"
		}


USAGE :== join "\n"
	[ "Cloogle builddb\n"
	, "Usage: ./builddb [opts] > types.json\n"
	, "Options:"
	, "  --help    Show this help"
	, "  -r PATH   Change the library root to PATH (default: /opt/clean/lib)"
	, "  -l PATH   Use PATH for a list of libraries to index (default: libs.json)"
	, ""]

Start :: *World -> *World
Start w
# (args, w) = getCommandLine w
# (f, w) = stdio w
# (ok, w) = case parseCLI (tl args) of
	(Left e) = fclose (f <<< e) w
	(Right cli)
	| cli.help    = fclose (f <<< USAGE) w
	# (libsf, w)  = readFile cli.libs_file w
	# libsjson    = fromString $ fromOk libsf
	# libs        = case libsjson of
		JSONObject groups -> sequence $ [fromJSON i \\ (_,JSONArray g) <- groups, i <- g]
		_                 -> Nothing
	| isError libsf || isNothing libs
		# err = stderr <<< "Could not read " <<< cli.libs_file <<< "\n"
		# (_,w) = fclose err w
		= fclose f w
	# libs        = fromJust libs
	# (mods, w)   = mapSt (flip (findModules cli.root) "") libs w
	# mods        = flatten mods
	#! (db, w)    = loop cli.root mods newTemporaryDB w
	#! (ok,w)     = fclose (stderr <<< "Linking database entries; this may take up to 10 minutes...\n") w
	| not ok      = abort "Couldn't close stderr\n"
	#! db         = finaliseDB builtins db
	#! (db,err)   = printStats db stderr
	#! (ok1,w)    = fclose err w
	#! (db,f)     = saveDB db f
	#! (ok2,w)    = fclose f w
	#! (_,dbg,w)  = fopen "typetree.dot" FWriteText w
	#! (db,dbg)   = writeTypeTree db dbg
	#! (_,w)      = fclose dbg w
	= (ok1 && ok2,w)
| not ok = abort "Couldn't close stdio\n"
= w
where
	loop :: String [ModuleEntry] !TemporaryDB !*World -> *(!TemporaryDB, !*World)
	loop _ [] db w = (db,w)
	loop root [mod:list] db w
	#! (_, w) = fclose (stderr <<< lib <<< ": " <<< modname <<< "\n") w
	#! (db, w) = indexModule False root mod db w
	#! db = hyperstrict db
	= loop root list db w
	where
		lib = fromJust (getLibrary mod.me_loc)
		modname = getName mod.me_loc

	builtins =
		map FunctionEntry builtin_functions ++
		map ClassEntry builtin_classes ++
		map TypeDefEntry builtin_types ++
		map FunctionEntry (concatMap constructor_functions builtin_types) ++
		map FunctionEntry (concatMap record_functions builtin_types) ++
		map SyntaxEntry builtin_syntax ++
		map ABCInstructionEntry builtin_abc_instructions

	parseCLI :: [String] -> Either String CLI
	parseCLI [] = Right zero
	parseCLI [x:a] = case (x,a) of
		("--help", xs) = (\c->{c & help=True}) <$> parseCLI xs
		("-r", []) = Left "'-r' requires an argument"
		("-r", [x:xs]) = (\c->{c & root=x}) <$> parseCLI xs
		("-l", []) = Left "'-l' requires an argument"
		("-l", [x:xs]) = (\c->{c & libs_file=x}) <$> parseCLI xs
		(x, _) = Left $ "Unknown option '" +++ x +++ "'"

	printStats :: !*CloogleDB !*File -> *(*CloogleDB, *File)
	printStats db f
	# (s,db) = dbStats db
	= (db, f
		<<< "| Table             | Size  |\n"
		<<< "|-------------------|------:|\n"
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
		<< "ABC instructions"  <-- s.n_abc_instructions)
	where
		(<<) infixl :: *File String -> *File
		(<<) f s = f <<< "| " <<< rpad 17 s <<< " | "

		(<--) infixl :: *File Int -> *File
		(<--) f i = f <<< lpad 5 i <<< " |\n"

		lpad n s = {' ' \\ _ <- [0..n-size (toString s)-1]} +++ toString s
		rpad n s = toString s +++ {' ' \\ _ <- [0..n-size (toString s)-1]}
