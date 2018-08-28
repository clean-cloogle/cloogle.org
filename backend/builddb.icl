module builddb

import StdArray
import StdBool
import StdFile
from StdFunc import const, flip, id, o
import StdList
import StdMisc
import StdString
import StdTuple

import Clean.Types
import Control.Monad => qualified join
import Data.Either
import Data.Error
from Data.Func import $, hyperstrict, mapSt
import Data.Functor
import Data.List
import Data.Maybe
import System.CommandLine
import System.File
import System.Options
from Text import class Text(join,startsWith), instance Text String
import Text.GenJSON

import Regex.Match
import Regex.Parse

import Cloogle.DB
from Cloogle.DB.Factory import :: TemporaryDB, newTemporaryDB, finaliseDB,
	findModules, indexModule, constructor_functions, record_functions,
	:: IndexItem, :: SourceURL, :: PathPattern

import Builtin.ABC
import Builtin.Predef
import Builtin.Syntax

:: Options =
	{ root             :: !String
	, libs_file        :: !String
	, module_filter    :: !Maybe Regex
	, include_builtins :: !Bool
	}

derive JSONDecode IndexItem, SourceURL, PathPattern

instance zero Options
where
	zero =
		{ root             = "/opt/clean/lib/"
		, libs_file        = "libs.json"
		, module_filter    = Nothing
		, include_builtins = True
		}

optionDescription :: Option Options
optionDescription = WithHelp True $ Options
	[ Shorthand "-r" "--root" $ Option
		"--root"
		(\dir opts -> Ok {opts & root=dir})
		"PATH"
		"Use PATH as the root directory for libraries (default: /opt/clean/lib)"
	, Shorthand "-l" "--libraries" $ Option
		"--libraries"
		(\file opts -> Ok {opts & libs_file=file})
		"FILE"
		"Use FILE for a list of libraries to index (default: libs.json)"
	, Option
		"--module-filter"
		(\filter opts -> case compile filter of
			Error e -> Error ["Regex parsing failed: " +++ e]
			Ok rgx  -> Ok {opts & module_filter=Just rgx})
		"REGEX"
		"Only index modules matching the regular expression REGEX"
	, Flag
		"--exclude-builtins"
		(\opts -> Ok {opts & include_builtins=False})
		"Exclude builtins from the index"
	]

Start :: *World -> *World
Start w
# ([prog:args], w) = getCommandLine w
# opts = parseOptions optionDescription args zero
| isError opts
	# err = stderr <<< join "\n" (fromError opts) <<< "\n"
	# (_,w) = fclose err w
	= w
# opts = fromOk opts
# (libsf, w) = readFile opts.libs_file w
# libsjson   = fromString $ fromOk libsf
# libs       = case libsjson of
	JSONObject groups -> sequence $ [fromJSON i \\ (_,JSONArray g) <- groups, i <- g]
	_                 -> Nothing
| isError libsf || isNothing libs
	# err = stderr <<< "Could not read " <<< opts.libs_file <<< "\n"
	# (_,w) = fclose err w
	= w
# libs       = fromJust libs
# (mods, w)  = mapSt (flip (findModules opts.root) "") libs w
# mods       = flatten mods
# mods       = case opts.module_filter of
	Just ftr -> filter (\m -> not $ isEmpty $ match ftr $ fromString $ getName m.me_loc) mods
	Nothing  -> mods
#! (db, w)   = loop opts.root mods newTemporaryDB w
#! (ok,w)    = fclose (stderr <<< "Linking database entries; this may take up to 10 minutes...\n") w
| not ok     = abort "Couldn't close stderr\n"
#! db        = finaliseDB (if opts.include_builtins builtins []) db
#! (db,err)  = printStats db stderr
#! (ok1,w)   = fclose err w
#! (f, w)    = stdio w
#! (db,f)    = saveDB db f
#! (ok2,w)   = fclose f w
#! (_,dbg,w) = fopen "typetree.dot" FWriteText w
#! (db,dbg)  = writeTypeTree db dbg
#! (_,w)     = fclose dbg w
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
