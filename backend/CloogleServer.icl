module CloogleServer

import StdArray
import StdBool
import StdFile
import StdList
import StdOrdList
import StdOverloaded
import StdString
import StdTuple
from StdFunc import const, flip, id, o, seq
from StdMisc import abort

from TCPIP import :: IPAddress, :: Port, instance toString IPAddress

from Data.Func import $
import Data.List
import Data.Tuple
import Data.Maybe
import System.CommandLine
import Data.Functor
import Control.Applicative
import Control.Monad
from Text import class Text(concat,trim,indexOf,toLowerCase,split),
	instance Text String, instance + String
import Text.JSON

import System.Time

from SimpleTCPServer import :: LogMessage{..}, serve, :: Logger
import qualified SimpleTCPServer
import TypeDB
import Type
import Cache
import Cloogle

MAX_RESULTS    :== 15
CACHE_PREFETCH :== 5

DEFAULT_INCLUDE_BUILTINS :== True
DEFAULT_INCLUDE_CORE :== False

:: RequestCacheKey
	= { c_unify            :: Maybe Type
	  , c_name             :: Maybe String
	  , c_className        :: Maybe String
	  , c_typeName         :: Maybe String
	  , c_modules          :: Maybe [String]
	  , c_libraries        :: Maybe [String]
	  , c_include_builtins :: Bool
	  , c_include_core     :: Bool
	  , c_page             :: Int
	  }

derive JSONEncode Kind, ClassOrGeneric, Type, RequestCacheKey
instance toString RequestCacheKey
where toString rck = toString $ toJSON rck

toRequestCacheKey :: Request -> RequestCacheKey
toRequestCacheKey r =
	{ c_unify            = r.unify >>= parseType o fromString
	, c_name             = r.name
	, c_className        = r.className
	, c_typeName         = r.typeName
	, c_modules          = sort <$> r.modules
	, c_libraries        = sort <$> r.libraries
	, c_include_builtins = fromJust (r.include_builtins <|> Just DEFAULT_INCLUDE_BUILTINS)
	, c_include_core     = fromJust (r.include_core <|> Just DEFAULT_INCLUDE_CORE)
	, c_page             = fromJust (r.page <|> Just 0)
	}

Start w
# (io, w) = stdio w
# (cmdline, w) = getCommandLine w
| length cmdline <> 2 = help io w
# [_,port:_] = cmdline
# port = toInt port
# (db, io) = openDb io
# (_, w) = fclose io w
| isNothing db = abort "stdin does not have a TypeDB\n"
#! db = fromJust db
= serve (handle db) (Just log) port w
where
	help :: *File *World -> *World
	help io w
	# io = io <<< "Usage: ./CloogleServer <port>\n"
	= snd $ fclose io w

	handle :: !TypeDB !(Maybe Request) !*World -> *(!Response, CacheKey, !*World)
	handle _ Nothing w = (err CLOOGLE_E_INVALIDINPUT "Couldn't parse input", "", w)
	handle db (Just request=:{unify,name,page}) w
		//Check cache
		# (mbResponse, w) = readCache key w
		| isJust mbResponse
			# r = fromJust mbResponse
			= ({r & return = if (r.return == 0) 1 r.return}, cacheKey key, w)
		| isJust name && size (fromJust name) > 40
			= respond (err CLOOGLE_E_INVALIDNAME "Function name too long") w
		| isJust name && any isSpace (fromString $ fromJust name)
			= respond (err CLOOGLE_E_INVALIDNAME "Name cannot contain spaces") w
		| isJust unify && isNothing (parseType $ fromString $ fromJust unify)
			= respond (err CLOOGLE_E_INVALIDTYPE "Couldn't parse type") w
		// Results
		# drop_n = fromJust (page <|> pure 0) * MAX_RESULTS
		# results = drop drop_n $ sort $ search request db
		# more = max 0 (length results - MAX_RESULTS)
		// Suggestions
		# mbType = unify >>= parseType o fromString
		# suggestions = mbType >>= flip (suggs name) db
		# w = seq [cachePages
				(toRequestCacheKey req) CACHE_PREFETCH 0 zero suggs
				\\ (req,suggs) <- mb2list suggestions] w
			with
				mb2list Nothing = []; mb2list (Just xs) = xs
		# suggestions
			= sortBy (\a b -> snd a > snd b) <$>
			  filter ((<) (length results) o snd) <$>
			  map (appSnd length) <$> suggestions
		# (results,nextpages) = splitAt MAX_RESULTS results
		// Response
		# response = if (isEmpty results)
			(err CLOOGLE_E_NORESULTS "No results")
			{ zero
		    & data           = results
		    , more_available = Just more
		    , suggestions    = suggestions
		    }
		// Save page prefetches
		# w = cachePages key CACHE_PREFETCH 1 response nextpages w
		// Save cache file
		= respond response w
	where
		key = toRequestCacheKey request

		respond :: Response *World -> *(Response, CacheKey, *World)
		respond r w = (r, cacheKey key, writeCache LongTerm key r w)

		cachePages :: RequestCacheKey Int Int Response [Result] *World -> *World
		cachePages key _ _  _ [] w = w
		cachePages key 0 _  _ _  w = w
		cachePages key npages i response results w
		# w = writeCache Brief req` resp` w
		= cachePages key (npages - 1) (i + 1) response keep w
		where
			req` = { key & c_page = key.c_page + i }
			resp` =
				{ response
				& more_available = Just $ max 0 (length results - MAX_RESULTS)
				, data = give
				}
			(give,keep) = splitAt MAX_RESULTS results

	suggs :: !(Maybe String) !Type !TypeDB -> Maybe [(Request, [Result])]
	suggs n (Func is r cc) db
		| length is < 3
			= Just [let t` = concat $ print False $ Func is` r cc in
			        let request = {zero & name=n, unify=Just t`} in
			        (request, search request db)
			        \\ is` <- permutations is | is` <> is]
	suggs _ _ _ = Nothing

	search :: !Request !TypeDB -> [Result]
	search {unify,name,className,typeName,modules,libraries,page,include_builtins,include_core} db
		# include_builtins = fromJust (include_builtins <|> Just DEFAULT_INCLUDE_BUILTINS)
		# include_core = fromJust (include_core <|> Just DEFAULT_INCLUDE_CORE)
		# db = case libraries of
			(Just ls) = filterLocations (isLibMatch ls) db
			Nothing   = db
		# db = case modules of
			(Just ms) = filterLocations (isModMatch ms) db
			Nothing   = db
		# db = if include_builtins id (filterLocations (not o isBuiltin)) db
		# db = if include_core id (filterLocations (not o isCore)) db
			with
				isCore :: Location -> Bool
				isCore (Builtin _) = False
				isCore (Location lib mod _ _ _) = case getModule lib mod db of
					Nothing  = False
					(Just b) = b.is_core
		| isJust className
			# className = fromJust className
			# classes = findClass className db
			= map (flip makeClassResult db) classes
		| isJust typeName
			# typeName = fromJust typeName
			# types = findType typeName db
			= [makeTypeResult (Just typeName) l td db \\ (l,td) <- types]
		# mbType = prepare_unification True <$> (unify >>= parseType o fromString)
		// Search normal functions
		# filts = catMaybes [ (\t _ -> isUnifiable t) <$> mbType
		                    , (\n loc _ -> isNameMatch (size n*2/3) n $ getName loc) <$> name
		                    ]
		# funs = map (\f -> makeFunctionResult name mbType Nothing f db) $ findFunction`` filts db
		// Search macros
		# macros = case (isNothing mbType,name) of
			(True,Just n) = findMacro` (\loc _ -> isNameMatch (size n*2/3) n $ getName loc) db
			_             = []
		# macros = map (\(lhs,rhs) -> makeMacroResult name lhs rhs) macros
		// Search class members
		# filts = catMaybes [ (\t _ _ _ _->isUnifiable t) <$> mbType
		                    , (\n (Location lib mod _ _ _) _ _ f _ -> isNameMatch
		                      (size n*2/3) n f) <$> name
		                    ]
		# members = findClassMembers`` filts db
		# members = map (\(Location lib mod line iclline cls,vs,_,f,et) -> makeFunctionResult name mbType
			(Just {cls_name=cls,cls_vars=vs}) (Location lib mod line iclline f,et) db) members
		// Search types
		# lcName = if (isJust mbType && isType (fromJust mbType))
			(let (Type name _) = fromJust mbType in Just $ toLowerCase name)
			(toLowerCase <$> name)
		# types = case (isNothing mbType,lcName) of
			(True,Just n) = findType` (\loc _ -> toLowerCase (getName loc) == n) db
			_             = []
		# types = map (\(tl,td) -> makeTypeResult name tl td db) types
		// Search classes
		# classes = case (isNothing mbType, toLowerCase <$> name) of
			(True, Just c) = findClass` (\loc _ _ _ -> toLowerCase (getName loc) == c) db
			_              = []
		# classes = map (flip makeClassResult db) classes
		// Search modules
		# modules = case (mbType, name) of
			(Nothing, Just n) = findModule` (\_ m _ -> isModNameMatch (size n*2/3) n m) db
			_                 = []
		# modules = map (makeModuleResult name) modules
		// Merge results
		= sort $ funs ++ members ++ types ++ classes ++ macros ++ modules

	makeModuleResult :: (Maybe String) (Library, Module, ModuleInfo) -> Result
	makeModuleResult mbName (lib, mod, info)
		= ModuleResult
		  ( { library  = lib
		    , modul    = mod
		    , filename = modToFilename mod
		    , dcl_line = Nothing
		    , icl_line = Nothing
		    , distance = modLevenshtein (fromJust mbName) mod
		    , builtin  = Nothing
		    }
		  , { module_is_core = info.is_core
		    }
		  )

	makeClassResult :: (Location, [TypeVar], ClassContext, [(Name,ExtendedType)])
		TypeDB -> Result
	makeClassResult rec=:(Builtin _, _, _, _) db
		= ClassResult
		  ( { library  = ""
		    , filename = ""
		    , dcl_line = Nothing
		    , icl_line = Nothing
		    , modul    = ""
		    , distance = -100
		    , builtin  = Just True
		    }
		  , makeClassResultExtras rec db
		  )
	makeClassResult rec=:(Location lib mod line iclline cls, vars, cc, funs) db
		= ClassResult
		  ( { library  = lib
		    , filename = modToFilename mod
		    , dcl_line = line
		    , icl_line = iclline
		    , modul    = mod
		    , distance = -100
		    , builtin  = Nothing
		    }
		  , makeClassResultExtras rec db
		  )
	makeClassResultExtras :: (Location, [TypeVar], ClassContext, [(Name,ExtendedType)])
		TypeDB -> ClassResultExtras
	makeClassResultExtras (l, vars, cc, funs) db
		= { class_name = cls
		  , class_heading = foldl ((+) o (flip (+) " ")) cls vars +
		      if (isEmpty cc) "" " | " + concat (print False cc)
		  , class_funs = [print_fun fun \\ fun <- funs]
		  , class_instances
		      = sortBy (\(a,_) (b,_) -> a < b)
		          [([concat (print False t) \\ t <- ts], map loc ls)
		          \\ (ts,ls) <- getInstances cls db]
		  }
	where
		cls = case l of
			Builtin c = c
			Location _ _ _ _ c = c

		print_fun :: (Name,ExtendedType) -> String
		print_fun f=:(_,ET _ et) = fromJust $
			et.te_representation <|> (pure $ concat $ print False f)

	makeTypeResult :: (Maybe String) Location TypeDef TypeDB -> Result
	makeTypeResult mbName (Location lib mod line iclline t) td db
		= TypeResult
		  ( { library  = lib
		    , filename = modToFilename mod
		    , dcl_line = line
		    , icl_line = iclline
		    , modul    = mod
		    , distance
		        = if (isNothing mbName) -100 (levenshtein` t (fromJust mbName))
		    , builtin  = Nothing
		    }
		  , { type             = concat $ print False td
		    , type_instances   = map (appSnd3 (map (concat o (print False)))) $
		        map (appThd3 (map loc)) $ getTypeInstances t db
		    , type_derivations = map (appSnd (map loc)) $ getTypeDerivations t db
		    }
		  )
	makeTypeResult mbName (Builtin t) td db
		= TypeResult
		  ( { library  = ""
		    , filename = ""
		    , dcl_line = Nothing
		    , icl_line = Nothing
		    , modul    = ""
		    , distance
		        = if (isNothing mbName) -100 (levenshtein` t (fromJust mbName))
		    , builtin  = Just True
		    }
		  , { type             = concat $ print False td
		    , type_instances   = map (appSnd3 (map (concat o (print False)))) $
		        map (appThd3 (map loc)) $ getTypeInstances t db
		    , type_derivations = map (appSnd (map loc)) $ getTypeDerivations t db
		    }
		  )

	makeMacroResult :: (Maybe String) Location Macro -> Result
	makeMacroResult mbName (Location lib mod line iclline m) mac
		= MacroResult
		  ( { library  = lib
		    , filename = modToFilename mod
		    , dcl_line = line
		    , icl_line = iclline
		    , modul    = mod
		    , distance
		        = if (isNothing mbName) -100 (levenshtein` (fromJust mbName) m)
		    , builtin  = Nothing
		    }
		  , { macro_name = m
		    , macro_representation = mac.macro_as_string
		    }
		  )

	makeFunctionResult :: (Maybe String) (Maybe Type) (Maybe ShortClassResult)
		(Location, ExtendedType) TypeDB -> Result
	makeFunctionResult
		orgsearch orgsearchtype mbCls (fl, et=:(ET type tes)) db
		= FunctionResult
		  ( { library  = lib
		    , filename = modToFilename mod
		    , dcl_line = line
		    , icl_line = iclline
		    , modul    = mod
		    , distance = distance
		    , builtin  = builtin
		    }
		  , { func     = fromJust (tes.te_representation <|>
		                           (pure $ concat $ print False (fname,et)))
		    , unifier  = toStrUnifier <$> finish_unification <$>
		        (orgsearchtype >>= unify [] (prepare_unification False type))
		    , cls      = mbCls
		    , constructor_of = if tes.te_isconstructor
		        (let (Func _ r _) = type in Just $ concat $ print False r)
		        Nothing
		    , recordfield_of = if tes.te_isrecordfield
		        (let (Func [t:_] _ _) = type in Just $ concat $ print False t)
		        Nothing
		    , generic_derivations
		        = let derivs = getDerivations fname db in
		          const (sortBy (\(a,_) (b,_) -> a < b)
				    [(concat $ print False d, map loc ls) \\ (d,ls) <- derivs]) <$>
		          tes.te_generic_vars
		    }
		  )
	where
		(lib,mod,fname,line,iclline,builtin) = case fl of
			(Location l m ln iln f) = (l,  m,  f, ln,      iln,     Nothing)
			(Builtin f)             = ("", "", f, Nothing, Nothing, Just True)

		toStrUnifier :: Unifier -> StrUnifier
		toStrUnifier (tvas1, tvas2) = (map toStr tvas1, map toStr tvas2)
		where toStr (var, type) = (var, concat $ print False type)

		toStrPriority :: (Maybe Priority) -> String
		toStrPriority p = case print False p of [] = ""; ss = concat [" ":ss]

		distance
			| isNothing orgsearch || fromJust orgsearch == ""
				| isNothing orgsearchtype = 0
				# orgsearchtype = fromJust orgsearchtype
				# (Just (ass1, ass2)) = finish_unification <$>
					unify [] orgsearchtype (prepare_unification False type)
				= penalty + toInt (sum [typeComplexity t \\ (_,t)<-ass1 ++ ass2 | not (isVar t)])
			# orgsearch = fromJust orgsearch
			= penalty + levenshtein` orgsearch fname
		where
			penalty
			| tes.te_isrecordfield = 2
			| tes.te_isconstructor = 1
			| otherwise            = 0

			typeComplexity :: Type -> Real
			typeComplexity (Type _ ts) = 1.2 * foldr ((+) o typeComplexity) 1.0 ts
			typeComplexity (Func is r _) = 2.0 * foldr ((+) o typeComplexity) 1.0 [r:is]
			typeComplexity (Var _) = 1.0
			typeComplexity (Cons _ ts) = 1.2 * foldr ((+) o typeComplexity) 1.0 ts
			typeComplexity (Uniq t) = 3.0 + typeComplexity t

	levenshtein` :: String String -> Int
	levenshtein` a b = if (indexOf a b == -1) 0 -100 +
		levenshtein [c \\ c <-: a] [c \\ c <-: b]

	modLevenshtein :: String Module -> Int
	modLevenshtein s mod = minList $ map (levenshtein` s) [mod:split "." mod]

	modToFilename :: String -> String
	modToFilename mod = (toString $ reverse $ takeWhile ((<>)'.')
	                              $ reverse $ fromString mod) + ".dcl"

	isUnifiable :: Type ExtendedType -> Bool
	isUnifiable t1 (ET t2 _) = isJust (unify [] t1 (prepare_unification False t2))

	isNameMatch :: !Int !String !String -> Bool
	isNameMatch maxdist n1 name
		# (n1, n2) = ({toLower c \\ c <-: n1}, {toLower c \\ c <-: name})
		= n1 == "" || indexOf n1 n2 <> -1 || levenshtein [c \\ c <-: n1] [c \\ c <-: n2] <= maxdist

	isModNameMatch :: !Int !String !Module -> Bool
	isModNameMatch maxdist name mod
		= any (isNameMatch maxdist name) [mod:split "." mod]

	isModMatch :: ![String] Location -> Bool
	isModMatch mods (Location _ mod _ _ _) = isMember mod mods
	isModMatch _    (Builtin _)            = False

	isLibMatch :: ![String] Location -> Bool
	isLibMatch libs (Location lib _ _ _ _) = any (\l -> indexOf l lib == 0) libs
	isLibMatch _    (Builtin _)            = True

	loc :: Location -> LocationResult
	loc (Location lib mod ln iln _) = (lib, mod, ln, iln)

	log :: (LogMessage (Maybe Request) Response CacheKey) IPAddress *World
		-> *(IPAddress, *World)
	log msg s w
	| not needslog = (newS msg s, w)
	# (tm,w) = localTime w
	# (io,w) = stdio w
	# io = io <<< trim (toString tm) <<< " " <<< msgToString msg s
	= (newS msg s, snd (fclose io w))
	where
		needslog = case msg of (Received _) = True; (Sent _ _) = True; _ = False

	newS :: (LogMessage (Maybe Request) Response CacheKey) IPAddress -> IPAddress
	newS m s = case m of (Connected ip) = ip; _ = s

	msgToString :: (LogMessage (Maybe Request) Response CacheKey) IPAddress -> String
	msgToString (Received Nothing) ip
		= toString ip + " <-- Nothing\n"
	msgToString (Received (Just a)) ip
		= toString ip + " <-- " + toString a + "\n"
	msgToString (Sent {return,data,msg,more_available} ck) ip
		= toString ip + " --> " + toString (length data)
			+ " results (" + toString return + "; " + msg
			+ if (isJust more_available) ("; " + toString (fromJust more_available) + " more") ""
			+ "; cache: " + ck + ")\n"
	msgToString _ _ = ""
