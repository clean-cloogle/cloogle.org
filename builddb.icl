module builddb

// Project libraries
import qualified TypeDB as DB
from TypeDB import ::TypeExtras{..}

// StdEnv
import StdFile, StdList, StdMisc, StdArray, StdBool, StdString, StdTuple

// CleanPlatform
import Data.Maybe, Data.Either, Data.Error, Data.Func, Data.Tuple, Data.Functor
from Text import class Text(concat), instance Text String
import System.Directory, System.CommandLine

// CleanTypeUnifier
import qualified Type as T
from Type import class print(print), instance print [a], instance print String
import CoclUtils

// frontend
//import Heap, compile, parse, predef
import Heap
from hashtable import ::HashTable, ::QualifiedIdents(NoQualifiedIdents), ::IdentClass(IC_Module), ::BoxedIdent{..}, putIdentInHashTable
from predef import init_identifiers
from compile import empty_cache, ::DclCache{hash_table}
from general import ::Optional(..)
from syntax import ::SymbolTable, ::SymbolTableEntry, ::Ident{..}, ::SymbolPtr, ::Position(NoPos), ::Module{mod_ident,mod_defs}, ::ParsedDefinition(PD_TypeSpec,PD_Instance,PD_Class), ::FunSpecials, ::Priority, ::ParsedModule, ::SymbolType, ::ParsedInstanceAndMembers{..}, ::ParsedInstance{pi_ident,pi_types}, ::Type, ::ClassDef{class_ident,class_args}, ::TypeVar
from scanner import ::Priority(..), ::Assoc(..)
from parse import wantModule

:: CLI = {
	help :: Bool,
	version :: Bool,
	root :: String,
	libs :: [String]}

instance zero CLI where
	zero = { version = False
	       , help = False
	       , root = "/opt/clean/lib/"
	       , libs = [ "StdEnv"
	                , "StdLib"
	                , "ArgEnv"
	                , "Directory"
	                , "Dynamics"
	                , "Gast"
	                , "Generics"
	                , "MersenneTwister"
	                , "TCPIP"
	                , "clean-platform/OS-Independent"
	                , "clean-platform/OS-Linux"
	                , "clean-platform/OS-Linux-32"
	                , "clean-platform/OS-Linux-64"
	                , "clean-platform/OS-Mac"
	                , "clean-platform/OS-Posix"
	                , "clean-platform/OS-Windows"
	                , "clean-platform/OS-Windows-32"
	                , "clean-platform/OS-Windows-64"
	                , "iTasks-SDK/Dependencies/graph_copy"
	                , "iTasks-SDK/Dependencies/clean-sapl/src"
	                , "iTasks-SDK/Server"
	                , "iTasks-SDK/Tests"
	                ]
	       }


VERSION :== "Cloogle's builddb version 0.1\n"
USAGE :== concat [
	VERSION, "\n",
	"Usage: ./builddb [opts] > builddb.json\n\n",
	"\t-h, --help Show this help\n",
	"\t-r PATH    Change the library root to PATH\n",
	"\t-l PATH    Add PATH to the librarypaths relative to the root\n"]

Start w
# (args, w) = getCommandLine w
# (f, w) = stdio w
# (ok, w) = case parseCLI (tl args) of
	(Left e) = fclose (f <<< e) w
	(Right cli)
	| cli.help = fclose (f <<< USAGE) w
	| cli.version = fclose (f <<< VERSION) w
	# (mods, w) = findModules` cli.libs cli.root w
	# (st, w) = init_identifiers newHeap w
	# cache = empty_cache st
	# (db, w) = loop cli.root mods 'DB'.newDb cache w
	# f = 'DB'.saveDb db f
	= fclose f w
| not ok = abort "Couldn't close stdio"
= w
where
	loop :: String [(String,String)] 'DB'.TypeDB *DclCache *World -> *('DB'.TypeDB, *World)
	loop _ [] db _ w = (db,w)
	loop root [(lib,mod):list] db cache w
	# (db, cache, w) = getModuleTypes root mod lib cache db w
	= loop root list db cache w

	parseCLI :: [String] -> Either String CLI
	parseCLI [] = Right zero
	parseCLI [x:a] = case (x,a) of
		("--help", xs) = (\c->{c & help=True}) <$> parseCLI xs
		("--version", xs) = (\c->{c & version=True}) <$> parseCLI xs
		("-l", []) = Left "'-l' requires an argument"
		("-r", []) = Left "'-r' requires an argument"
		("-r", [x:xs]) = (\c->{c & root=x}) <$> parseCLI xs
		("-l", [x:xs]) = (\c->{c & libs=[x:c.libs]}) <$> parseCLI xs
		(x, _) = Left $ "Unknown option '" +++ x +++ "'"

//              Libraries                Library Module
findModules` :: ![String] !String !*World -> *(![(String,String)], !*World)
findModules` [] _ w = ([], w)
findModules` [lib:libs] root w
#! (mods, w) = findModules lib root w
#! (moremods, w) = findModules` libs root w
= (removeDup (mods ++ moremods), w)

findModules :: !String !String !*World -> *(![(String,String)], !*World)
findModules lib root w
#! (fps, w) = readDirectory (root +++ "/" +++ lib) w
| isError fps = ([], w)
#! fps = fromOk fps
#! mods = map (\s->(lib, s%(0,size s-5))) $ filter isDclModule fps
#! (moremods, w) = findModules` (map ((+++) (lib+++"/")) (filter isDirectory fps)) root w
= (removeDup (mods ++ moremods), w)
where
	isDclModule :: String -> Bool
	isDclModule s = s % (size s - 4, size s - 1) == ".dcl"

	isDirectory :: String -> Bool
	isDirectory s = not $ isMember '.' $ fromString s

getModuleTypes :: String String String *DclCache 'DB'.TypeDB *World -> *('DB'.TypeDB, *DclCache, *World)
getModuleTypes root mod lib cache db w
# filename = root +++ "/" +++ lib +++ "/" +++ mkdir mod +++ ".dcl"
# (ok,f,w) = fopen filename FReadText w
| not ok = abort ("Couldn't open file " +++ filename +++ ".\n")
# (mod_id, ht) = putIdentInHashTable mod (IC_Module NoQualifiedIdents) cache.hash_table
  cache = {cache & hash_table=ht}
# ((b1,b2,pm,ht,f),w) = accFiles (wantModule` f "" False mod_id.boxed_ident NoPos True cache.hash_table stderr) w
  cache = {cache & hash_table=ht}
# (ok,w) = fclose f w
| not ok = abort ("Couldn't close file " +++ filename +++ ".\n")
# mod = pm.mod_ident.id_name
# lib = cleanlib mod lib
# db = 'DB'.putTypes (pd_typespecs lib mod pm.mod_defs) db
# db = 'DB'.putInstancess (pd_instances pm.mod_defs) db
# db = 'DB'.putClasses (pd_classes lib mod pm.mod_defs) db
= (db,cache,w)
where
	mkdir :: String -> String
	mkdir s = { if (c == '.') '/' c \\ c <-: s }

	cleanlib :: !String !String -> String // Remove module dirs from lib
	cleanlib mod lib = toString $ cl` (fromString $ mkdir mod) (fromString lib)
	where
		cl` :: ![Char] ![Char] -> [Char]
		cl` mod lib
			| not (isMember '/' mod) = lib
			# mod = reverse $ tl $ dropWhile ((<>)'/') $ reverse mod
			| drop (length lib - length mod) lib == mod
				= take (length lib - length mod - 1) lib
			= lib

	pd_typespecs :: String String [ParsedDefinition] -> [('DB'.FunctionLocation, 'DB'.ExtendedType)]
	pd_typespecs lib mod pds
		= [('DB'.FL lib mod id_name, 'DB'.ET ('T'.toType t) {te_priority=toPrio p})
		   \\ PD_TypeSpec pos id=:{id_name} p (Yes t) funspecs <- pds]
	where
		toPrio :: Priority -> Maybe 'DB'.TE_Priority
		toPrio (Prio LeftAssoc i)  = Just $ 'DB'.LeftAssoc i
		toPrio (Prio RightAssoc i) = Just $ 'DB'.RightAssoc i
		toPrio (Prio NoAssoc i)    = Just $ 'DB'.NoAssoc i
		toPrio _                   = Nothing

	pd_instances :: [ParsedDefinition] -> [('DB'.Class, ['DB'.Type])]
	pd_instances pds
		= [(pi_ident.id_name, map 'T'.toType pi_types)
		   \\ PD_Instance {pim_pi={pi_ident,pi_types}} <- pds]

	pd_classes :: String String [ParsedDefinition]
		-> [('DB'.ClassLocation, ['T'.TypeVar], [('DB'.FunctionName, 'DB'.ExtendedType)])]
	pd_classes lib mod pds
	# pds = filter (\pd->case pd of (PD_Class _ _)=True; _=False) pds
	= map (\(PD_Class {class_ident={id_name},class_args} pds)
		-> let typespecs = pd_typespecs lib mod pds
		in ('DB'.CL lib mod id_name, map 'T'.toTypeVar class_args, 
			[(f,et) \\ ('DB'.FL _ _ f, et) <- typespecs])) pds

unigroups :: (Type Type -> Bool) [(a,Type)] -> [([a],Type)]
unigroups f ts = unigroups` ts []
where
	unigroups` [] groups = groups
	unigroups` [(a,t):ts] [] = unigroups` ts [([a],t)]
	unigroups` [(a,t):ts] [(ns,ut):groups]
	| f t ut	= unigroups` ts [([a:ns],ut):groups]
	| otherwise = unigroups` ts [(ns,ut):unigroups` [(a,t)] groups]

(<+) infixr 5 :: a b -> [String] | print a & print b
(<+) a b = print a ++ print b

join :: a [b] -> [String] | print a & print b
join _ [] = []
join a [b:[]] = print b
join a [b:bs] = b <+ a <+ join a bs

alignl :: Int a -> [String] | print a
alignl i s
# s = print s
# len = sum (map size s)
| len >= i = s
| otherwise = s ++ [{' ' \\ i <- [0..i-len]}]

wantModule` :: !*File !{#Char} !Bool !Ident !Position !Bool !*HashTable !*File !*Files
	-> ((!Bool,!Bool,!ParsedModule, !*HashTable, !*File), !*Files)
wantModule` f s b1 i p b2 ht io fs
# (b1,b2,pm,ht,f,fs) = wantModule f s b1 i p b2 ht io fs
= ((b1,b2,pm,ht,f),fs)
