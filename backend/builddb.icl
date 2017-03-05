module builddb

// Project libraries
import qualified TypeDB as DB
from TypeDB import ::TypeExtras{..}, ::Macro{..}, ::ModuleInfo{..},
	instance zero TypeExtras, instance zero ModuleInfo

// StdEnv
from StdFunc import const, flip, o
import StdFile, StdList, StdMisc, StdArray, StdBool, StdString, StdTuple

// CleanPlatform
import Data.Maybe, Data.Either, Data.Error, Data.Func, Data.Tuple, Data.Functor
import Control.Applicative, Control.Monad
from Text import class Text(concat,replaceSubString,indexOf,startsWith),
	instance Text String
import System.Directory, System.CommandLine

// CleanTypeUnifier
import qualified Type as T
from Type import class print(print), instance print [a], instance print String,
	instance print Type, instance print Priority, instance == Type
from Type import qualified ::TypeDef{..}, ::Constructor{..}
import CoclUtils

// CleanPrettyPrint
import CleanPrettyPrint

// frontend
import Heap
from hashtable import ::HashTable, ::QualifiedIdents(NoQualifiedIdents),
	::IdentClass(IC_Module), ::BoxedIdent{..}, putIdentInHashTable
from predef import init_identifiers
from compile import empty_cache, ::DclCache{hash_table}
from general import ::Optional(..)
from syntax import ::SymbolTable, ::SymbolTableEntry, ::Ident{..}, ::SymbolPtr,
	::Position(..), ::LineNr, ::FileName, ::FunctName,
	::Module{mod_ident,mod_defs},
	::ParsedDefinition(PD_TypeSpec,PD_Instance,PD_Instances,PD_Class,PD_Type,PD_Generic,PD_Derive,PD_Function),
	::FunSpecials, ::Priority, ::ParsedModule, ::SymbolType,
	::ParsedInstanceAndMembers{..}, ::ParsedInstance{pi_ident,pi_pos,pi_types},
	::Type, ::ClassDef{class_ident,class_pos,class_args,class_context},
	::TypeVar, ::ParsedTypeDef, ::TypeDef{td_pos,td_ident},
	::GenericDef{gen_ident,gen_pos,gen_type,gen_vars},
	::GenericCaseDef{gc_type,gc_pos,gc_gcf}, ::GenericCaseFunctions(GCF), ::GCF,
	::FunKind(FK_Macro),
	::Rhs, ::ParsedExpr
from scanner import ::Priority(..), ::Assoc(..)
from parse import wantModule

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
	                   , ("CleanSerial", const False)
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
	                   ]
	       , exclude = [ "StdEnv/_startup"
	                   , "StdEnv/_system"
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
	# (st, w)     = init_identifiers newHeap w
	# cache       = empty_cache st
	# (db, w)     = loop cli.root mods 'DB'.newDb cache w
	# db          = 'DB'.putFunctions predefFunctions db
	# db          = 'DB'.putClasses predefClasses db
	# db          = 'DB'.putTypes predefTypes db
	# db          = 'DB'.putFunctions (flatten $ map constructor_functions predefTypes) db
	# db          = 'DB'.putFunctions (flatten $ map record_functions predefTypes) db
	# io          = stderr
	# io          = printStats db io
	# (ok1,w)     = fclose io w
	# f           = 'DB'.saveDb db f
	# (ok2,w)     = fclose f w
	= (ok1 && ok2,w)
| not ok = abort "Couldn't close stdio"
= w
where
	loop :: String [(String,String,Bool)] 'DB'.TypeDB
		*DclCache *World -> *('DB'.TypeDB, *World)
	loop _ [] db _ w = (db,w)
	loop root [(lib,mod,iscore):list] db cache w
	# (db, cache, w) = getModuleTypes root mod lib iscore cache db w
	# w = snd (fclose (stderr <<< lib <<< ": " <<< mod <<< "\n") w)
	= loop root list db cache w

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

	printStats :: !'DB'.TypeDB !*File -> *File
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
				[ 'DB'.moduleCount db
				, 'DB'.functionCount db
				, 'DB'.macroCount db
				, 'DB'.typeCount db
				, 'DB'.classCount db
				, 'DB'.instanceCount db
				, 'DB'.deriveCount db
				]
		pad n i = {' ' \\ _ <- [0..n-size (toString i)-1]} +++ toString i

predefFunctions :: [('DB'.Location, 'DB'.ExtendedType)]
predefFunctions
	= [ ( 'DB'.Builtin "if"
	    , 'DB'.ET ('T'.Func ['T'.Type "Bool" [], 'T'.Var "a", 'T'.Var "a"] ('T'.Var "a") []) zero
	    )
	  , ( 'DB'.Builtin "dynamic"
	    , 'DB'.ET ('T'.Func ['T'.Var "a"] ('T'.Type "Dynamic" []) []) zero
	    )
	  ]

predefClasses :: [('DB'.Location, ['T'.TypeVar], 'T'.ClassContext, [('DB'.Name, 'DB'.ExtendedType)])]
predefClasses
	= [ ( 'DB'.Builtin "TC", ["v"], [], [])
	  ]

predefTypes :: [('DB'.Location, 'T'.TypeDef)]
predefTypes
	= [ ( 'DB'.Builtin "Bool"
	    , { deft
	      & 'Type'.td_name = "Bool"
	      , 'Type'.td_rhs  = 'T'.TDRCons False
	        [ { defc & 'Type'.cons_name="False" }
	        , { defc & 'Type'.cons_name="True" }
	        ]
	      }
	    )
	  , ( 'DB'.Builtin "Int",     { deft & 'Type'.td_name = "Int"     } )
	  , ( 'DB'.Builtin "Real",    { deft & 'Type'.td_name = "Real"    } )
	  , ( 'DB'.Builtin "Char",    { deft & 'Type'.td_name = "Char"    } )
	  , ( 'DB'.Builtin "String",  { deft & 'Type'.td_name = "String",
	      'Type'.td_rhs = 'T'.TDRSynonym ('T'.Type "_#Array" ['T'.Type "Char" []]) } )
	  , ( 'DB'.Builtin "Dynamic", { deft & 'Type'.td_name = "Dynamic" } )
	  , ( 'DB'.Builtin "File",    { deft & 'Type'.td_name = "File"    } )
	  , ( 'DB'.Builtin "World",   { deft & 'Type'.td_name = "World",
	                                       'Type'.td_uniq = True      } )
	  ]
where
	deft = {'Type'.td_name="", 'Type'.td_uniq=False, 'Type'.td_args=[], 'Type'.td_rhs='T'.TDRAbstract}
	defc = {'Type'.cons_name="", 'Type'.cons_args=[], 'Type'.cons_exi_vars=[], 'Type'.cons_context=[], 'Type'.cons_priority=Nothing}

//             Exclude   Root    Library        Check for core       Base module
findModules :: ![String] !String !'DB'.Library ('DB'.Module -> Bool) !String !*World
	-> *(![('DB'.Library, 'DB'.Module, Bool)], !*World)
findModules ex root lib iscore base w
| any (\e -> indexOf e path <> -1) ex = ([], w)
#! (fps, w)   = readDirectory path w
| isError fps = ([], w)
#! fps        = fromOk fps
#! mods       = map (\s -> let mod = basedot +++ s % (0, size s - 5) in
	(lib, mod, iscore mod)) $ filter included $ filter isDclModule fps
#! (moremodss,w) = mapSt (\d -> findModules ex root lib iscore (basedot +++ d)) (filter isDirectory fps) w
= (removeDup (mods ++ flatten moremodss), w)
where
	path = root +++ "/" +++ lib +++ if (base == "") "" "/" +++ replaceSubString "." "/" base
	basedot = if (base == "") "" (base +++ ".")

	included :: String -> Bool
	included s = not (any (\e -> indexOf e (path +++ "/" +++ s) <> -1) ex)

	isDclModule :: String -> Bool
	isDclModule s = s % (size s - 4, size s - 1) == ".dcl"

	isDirectory :: String -> Bool
	isDirectory s = not $ isMember '.' $ fromString s

getModuleTypes :: String 'DB'.Module 'DB'.Library Bool
	*DclCache 'DB'.TypeDB *World -> *('DB'.TypeDB, *DclCache, *World)
getModuleTypes root mod lib iscore cache db w
# (Right dcl,cache,w) = readModule False cache w
# (icl,cache,w) = readModule True cache w
# icl = case icl of (Left _) = Nothing; (Right x) = Just x
# mod = dcl.mod_ident.id_name
# lib = cleanlib mod lib
# db  = 'DB'.putFunctions (pd_typespecs lib mod dcl.mod_defs icl) db
# db  = 'DB'.putInstances (pd_instances lib mod dcl.mod_defs icl) db
# db  = 'DB'.putClasses (pd_classes lib mod dcl.mod_defs icl) db
# typedefs = pd_types lib mod dcl.mod_defs icl
# db  = 'DB'.putTypes typedefs db
# db  = 'DB'.putFunctions (flatten $ map constructor_functions typedefs) db
# db  = 'DB'.putFunctions (flatten $ map record_functions typedefs) db
# db  = 'DB'.putFunctions (pd_generics lib mod dcl.mod_defs icl) db
# db  = 'DB'.putDerivationss (pd_derivations lib mod dcl.mod_defs) db
# db  = 'DB'.putMacros (pd_macros lib mod dcl.mod_defs) db
# db  = 'DB'.putModule lib mod {zero & is_core=iscore} db
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

	pd_macros :: String String [ParsedDefinition] -> [('DB'.Location, 'DB'.Macro)]
	pd_macros lib mod dcl
		= [( 'DB'.Location lib mod (toLine pos) Nothing id.id_name
		   , { macro_as_string = priostring id +++ cpp pd
		     , macro_extras = {zero & te_priority = findPrio id >>= 'T'.toMaybePriority}
		     }
		   ) \\ pd=:(PD_Function pos id isinfix args rhs FK_Macro) <- dcl]
	where
		priostring :: Ident -> String
		priostring id = case findTypeSpec id dcl of
			Nothing    = ""
			(Just pri) = cpp pri +++ "\n"

		findPrio :: Ident -> Maybe Priority
		findPrio id = (\(PD_TypeSpec _ _ p _ _) -> p) <$> findTypeSpec id dcl

		findTypeSpec :: Ident [ParsedDefinition] -> Maybe ParsedDefinition
		findTypeSpec _  []          = Nothing
		findTypeSpec id [pd=:(PD_TypeSpec _ id` prio _ _):dcl]
		| id`.id_name == id.id_name = Just pd
		findTypeSpec id [_:dcl]     = findTypeSpec id dcl

	pd_derivations :: String String [ParsedDefinition]
		-> [('DB'.Name, [('DB'.Type, 'DB'.Location)])]
	pd_derivations lib mod dcl
		= [( id.id_name
		   , [('T'.toType gc_type, 'DB'.Location lib mod (toLine gc_pos) Nothing "")]
		   ) \\ PD_Derive gcdefs <- dcl, {gc_type,gc_pos,gc_gcf=GCF id _} <- gcdefs]

	pd_generics :: String String [ParsedDefinition] (Maybe ParsedModule)
		-> [('DB'.Location, 'DB'.ExtendedType)]
	pd_generics lib mod dcl icl
		= [( 'DB'.Location lib mod (toLine gen_pos) (findIclLine id_name =<< icl) id_name
		   , 'DB'.ET ('T'.toType gen_type)
		       {zero & te_generic_vars=Just $ map 'T'.toTypeVar gen_vars
		             , te_representation=Just $ cpp gen}
		   ) \\ gen=:(PD_Generic {gen_ident={id_name},gen_pos,gen_type,gen_vars}) <- dcl]
	where
		findIclLine :: String ParsedModule -> Maybe Int
		findIclLine name {mod_defs=pms}
			= case [g.gen_pos \\ PD_Generic g <- pms | g.gen_ident.id_name == name] of
				[FunPos _ l _:_] = Just l
				[LinePos _ l:_] = Just l
				_ = Nothing

	pd_typespecs :: String String [ParsedDefinition] (Maybe ParsedModule)
		-> [('DB'.Location, 'DB'.ExtendedType)]
	pd_typespecs lib mod dcl icl
		= [( 'DB'.Location lib mod (toLine pos) (findIclLine id_name =<< icl) id_name
		   , 'DB'.ET ('T'.toType t)
		       { zero & te_priority = 'T'.toMaybePriority p
		              , te_representation = Just $ cpp ts}
		   ) \\ ts=:(PD_TypeSpec pos id=:{id_name} p (Yes t) funspecs) <- dcl]
	where
		findIclLine :: String ParsedModule -> Maybe Int
		findIclLine name {mod_defs=pms}
			= case [pos \\ PD_TypeSpec pos id _ _ _ <- pms | id.id_name == name] of
				[FunPos _ l _:_] = Just l
				[LinePos _ l:_] = Just l
				_ = Nothing

	pd_instances :: String String [ParsedDefinition] (Maybe ParsedModule)
		-> [('DB'.Class, ['DB'.Type], 'DB'.Location)]
	pd_instances lib mod dcl icl
		= [( id
		   , types
		   , 'DB'.Location lib mod (toLine pos) (findIclLine id types =<< icl) ""
		   ) \\ (id,types,pos) <- instances]
	where
		instances = map (appSnd3 (map 'T'.toType)) $
			[(i.pi_ident.id_name, i.pi_types, i.pi_pos) \\ PD_Instance {pim_pi=i} <- dcl]
			++ [(i.pi_ident.id_name, i.pi_types, i.pi_pos) \\ PD_Instances pis <- dcl, {pim_pi=i} <- pis]

		findIclLine :: String ['T'.Type] ParsedModule -> Maybe Int
		findIclLine name types {mod_defs=pms}
			= case [pi_pos
					\\ PD_Instance {pim_pi={pi_pos,pi_ident,pi_types}} <- pms
					| (pi_ident.id_name == name && map 'T'.toType pi_types == types)] of
				[LinePos _ l:_] = Just l
				_ = Nothing

	pd_classes :: String String [ParsedDefinition] (Maybe ParsedModule)
		-> [('DB'.Location, ['T'.TypeVar], 'T'.ClassContext,
			[('DB'.Name, 'DB'.ExtendedType)])]
	pd_classes lib mod dcl icl
	# dcl = filter (\pd->case pd of (PD_Class _ _)=True; _=False) dcl
	= map (\(PD_Class {class_ident={id_name},class_pos,class_args,class_context} dcl)
		-> let
			typespecs = pd_typespecs lib mod dcl icl
			macros = pd_macros lib mod dcl
			getMacro n = case filter ((==) n o 'DB'.getName o fst) macros of
				[]        = Nothing
				[(_,m):_] = Just m.macro_as_string
			updateRepresentation n ('DB'.ET t te)
				= 'DB'.ET t {te & te_representation = getMacro n <|> te.te_representation}
		in ('DB'.Location lib mod (toLine class_pos) (findIclLine id_name =<< icl) id_name
		   , map 'T'.toTypeVar class_args
		   , flatten $ map 'T'.toClassContext class_context
		   , [(f,updateRepresentation f et) \\ ('DB'.Location _ _ _ _ f, et) <- typespecs])) dcl
	where
		findIclLine :: String ParsedModule -> Maybe Int
		findIclLine name {mod_defs=pms}
			= case [class_pos \\ PD_Class {class_ident,class_pos} _ <- pms | class_ident.id_name == name] of
				[LinePos _ l:_] = Just l
				_ = Nothing

	pd_types :: String String [ParsedDefinition] (Maybe ParsedModule)
		-> [('DB'.Location, 'DB'.TypeDef)]
	pd_types lib mod dcl icl
		= [let name = 'T'.td_name td in
			('DB'.Location lib mod (toLine ptd.td_pos) (findIclLine name =<< icl) name, td)
		   \\ PD_Type ptd <- dcl, td <- ['T'.toTypeDef ptd]]
	where
		findIclLine :: String ParsedModule -> Maybe Int
		findIclLine name {mod_defs=pms}
			= case [td_pos \\ PD_Type {td_ident,td_pos} <- pms | td_ident.id_name == name] of
				[LinePos _ l:_] = Just l
				_ = Nothing

	toLine :: Position -> 'DB'.LineNr
	toLine (FunPos _ l _) = Just l
	toLine (LinePos _ l)  = Just l
	toLine _              = Nothing

	readModule :: Bool *DclCache *World -> *(Either String ParsedModule, *DclCache, *World)
	readModule icl cache w
	# filename = root +++ "/" +++ lib +++ "/" +++ mkdir mod +++ if icl ".icl" ".dcl"
	# (ok,f,w) = fopen filename FReadText w
	| not ok = (Left $ "Couldn't open " +++ filename, cache, w)
	# (mod_id, ht) = putIdentInHashTable mod (IC_Module NoQualifiedIdents) cache.hash_table
	  cache = {cache & hash_table=ht}
	# ((b1,b2,pm,ht,f),w) = accFiles (wantModule` f "" icl mod_id.boxed_ident NoPos True cache.hash_table stderr) w
	  cache = {cache & hash_table=ht}
	# (ok,w) = fclose f w
	| not ok = (Left $ "Couldn't open " +++ filename, cache, w)
	= (Right pm, cache, w)

constructor_functions :: ('DB'.Location, 'DB'.TypeDef)
	-> [('DB'.Location, 'DB'.ExtendedType)]
constructor_functions ('DB'.Builtin _, td)
	= [('DB'.Builtin c, 'DB'.ET f
		{zero & te_isconstructor=True
		      , te_representation=Just $ concat $
		          [c] ++ print_prio p ++ [" :: "] ++ print False f
		      , te_priority=p})
	   \\ (c,f,p) <- 'T'.constructorsToFunctions td]
constructor_functions ('DB'.Location lib mod line iclline _, td)
	= [('DB'.Location lib mod line iclline c, 'DB'.ET f
		{zero & te_isconstructor=True
		      , te_representation=Just $ concat $
		          [c] ++ print_prio p ++ [" :: "] ++ print False f
		      , te_priority=p})
	   \\ (c,f,p) <- 'T'.constructorsToFunctions td]

print_prio :: (Maybe 'T'.Priority) -> [String]
print_prio Nothing  = []
print_prio (Just p) = [" "] ++ print False p

record_functions :: ('DB'.Location, 'DB'.TypeDef)
	-> [('DB'.Location, 'DB'.ExtendedType)]
record_functions ('DB'.Builtin _, td)
	= [('DB'.Builtin f, 'DB'.ET t
		{zero & te_isrecordfield=True
		      , te_representation=Just $ concat $ [".", f, " :: " : print False t]})
		\\ (f,t) <- 'T'.recordsToFunctions td]
record_functions ('DB'.Location lib mod line iclline _, td)
	= [('DB'.Location lib mod line iclline f, 'DB'.ET t
		{zero & te_isrecordfield=True
		      , te_representation=Just $ concat $ [".", f, " :: " : print False t]})
		\\ (f,t) <- 'T'.recordsToFunctions td]

wantModule` :: !*File !{#Char} !Bool !Ident !Position !Bool !*HashTable !*File !*Files
	-> ((!Bool,!Bool,!ParsedModule, !*HashTable, !*File), !*Files)
wantModule` f s b1 i p b2 ht io fs
# (b1,b2,pm,ht,f,fs) = wantModule f s b1 i p b2 ht io fs
= ((b1,b2,pm,ht,f),fs)
