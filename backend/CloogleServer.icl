module CloogleServer

import StdArray
import StdBool
import StdFile
from StdFunc import id, o
import StdList
import StdMisc
import StdOrdList
import StdOverloaded
import StdString
import StdTuple

from TCPIP import :: IPAddress, :: Port, instance toString IPAddress

import Control.Applicative
import Control.Monad
import Data.Error
import qualified Data.Foldable as Foldable
from Data.Foldable import class Foldable
from Data.Func import $, hyperstrict, instance Functor ((->) r), mapSt, seqSt
import Data.Functor
from Data.List import permutations
import Data.Maybe
import Data.Tuple
import System._Posix
import System.CommandLine
import System.Time
from Text import class Text(concat,toLowerCase), instance Text String, <+
import Text.JSON

import Cloogle
import Type
import CloogleDB
import Search

import SimpleTCPServer
import Cache
import Memory

MAX_RESULTS    :== 15
CACHE_PREFETCH :== 5

:: RequestCacheKey
	= { c_unify            :: Maybe Type
	  , c_name             :: Maybe String
	  , c_className        :: Maybe String
	  , c_typeName         :: Maybe String
	  , c_using            :: Maybe [String]
	  , c_modules          :: Maybe [String]
	  , c_libraries        :: Maybe [String]
	  , c_include_builtins :: Bool
	  , c_include_core     :: Bool
	  , c_include_apps     :: Bool
	  , c_page             :: Int
	  }

derive JSONEncode Kind, Type, RequestCacheKey, TypeRestriction
derive JSONDecode Kind, Type, RequestCacheKey, TypeRestriction
instance toString RequestCacheKey
where toString rck = toString $ toJSON rck

toRequestCacheKey :: !*CloogleDB !Request -> *(!RequestCacheKey, !*CloogleDB)
toRequestCacheKey db r
# (allsyns,db) = allTypeSynonyms db
= (
	{ c_unify            = snd <$>
		prepare_unification True allsyns <$>
		(parseType o fromString =<< r.unify)
	, c_name             = toLowerCase <$> r.name
	, c_className        = r.className
	, c_typeName         = r.typeName
	, c_using            = r.using
	, c_modules          = sort <$> r.modules
	, c_libraries        = sort <$> r.libraries
	, c_include_builtins = fromMaybe DEFAULT_INCLUDE_BUILTINS r.include_builtins
	, c_include_core     = fromMaybe DEFAULT_INCLUDE_CORE r.include_core
	, c_include_apps     = fromMaybe DEFAULT_INCLUDE_APPS r.include_apps
	, c_page             = fromMaybe 0 r.page
	}, db)
fromRequestCacheKey :: RequestCacheKey -> Request
fromRequestCacheKey k =
	{ unify            = concat <$> print False <$> k.c_unify
	, name             = k.c_name
	, className        = k.c_className
	, typeName         = k.c_typeName
	, using            = k.c_using
	, modules          = k.c_modules
	, libraries        = k.c_libraries
	, include_builtins = Just k.c_include_builtins
	, include_core     = Just k.c_include_core
	, include_apps     = Just k.c_include_apps
	, page             = Just k.c_page
	}

:: Options =
	{ port         :: Int
	, help         :: Bool
	, reload_cache :: Bool
	}

instance zero Options where zero = {port=31215, help=False, reload_cache=False}

parseOptions :: Options [String] -> MaybeErrorString Options
parseOptions opt [] = Ok opt
parseOptions opt ["-p":p:rest] = case (toInt p, p) of
	(0, "0") -> Error "Cannot use port 0"
	(0, p)   -> Error $ "'" <+ p <+ "' is not an integer"
	(p, _)   -> parseOptions {Options | opt & port=p} rest
parseOptions opt ["-h":rest] = parseOptions {opt & help=True} rest
parseOptions opt ["--help":rest] = parseOptions {opt & help=True} rest
parseOptions opt ["--reload-cache":rest] = parseOptions {opt & reload_cache=True} rest
parseOptions opt [arg:_] = Error $ "Unknown option '" <+ arg <+ "'"

Start w
# (cmdline, w) = getCommandLine w
# opts = parseOptions zero (tl cmdline)
| isError opts
	# (io,w) = stdio w
	# io = io <<< fromError opts <<< "\n"
	# (_,w) = fclose io w
	= w
# opts = fromOk opts
| opts.help = help (hd cmdline) w
# w = disableSwap w
#! (_,f,w) = fopen "types.json" FReadText w
#! (db,f) = openDB f
#! (ok,db) = isJustU db
| not ok
	# (io,w) = stdio w
	# io = io <<< "Could not open database\n"
	# (_,w) = fclose io w
	= w
#! db = hyperstrict (fromJust db)
#! (db,w) = if opts.reload_cache (doInBackground reloadCache) id (db,w)
#! (_,w) = fclose f w
= serve
	{ handler           = handle
	, logger            = Just log
	, port              = opts.Options.port
	, connect_timeout   = Just 3600000 // 1h
	, keepalive_timeout = Just 5000    // 5s
	} db w
where
	help :: String *World -> *World
	help pgm w
	# (io, w) = stdio w
	# io = io <<< "Usage: " <<< pgm <<< " [--reload-cache] [-p <port>] [-h] [--help]\n"
	= snd $ fclose io w

	disableSwap :: *World -> *World
	disableSwap w
	# (ok,w) = mlockall (MCL_CURRENT bitor MCL_FUTURE) w
	| ok = w
	# (err,w) = errno w
	# (io,w) = stdio w
	# io = io <<< "Could not lock memory (" <<< err <<< "); process may get swapped out\n"
	= snd $ fclose io w

	handle :: !(Maybe Request) !*CloogleDB !*World -> *(!Response, CacheKey, !*CloogleDB, !*World)
	handle Nothing db w = (err InvalidInput "Couldn't parse input", "", db, w)
	handle (Just request=:{unify,name,page}) db w
		//Check cache
		#! (key,db) = toRequestCacheKey db request
		#! (mbResponse, w) = readCache key w
		| isJust mbResponse
			# r = fromJust mbResponse
			= respond key {r & return = if (r.return == 0) 1 r.return} db w
		| isJust name && size (fromJust name) > 40
			= respond key (err InvalidName "Function name too long") db w
		| isJust name && any isSpace (fromString $ fromJust name)
			= respond key (err InvalidName "Name cannot contain spaces") db w
		| isJust unify && isNothing (parseType $ fromString $ fromJust unify)
			= respond key (err InvalidType "Couldn't parse type") db w
		// Results
		#! drop_n = fromJust (page <|> pure 0) * MAX_RESULTS
		#! (res,db) = search request db
		#! results = drop drop_n res
		#! more = max 0 (length results - MAX_RESULTS)
		// Suggestions
		#! (suggestions,db) = case unify >>= parseType o fromString of
			Just t -> suggs name t db
			Nothing -> (Nothing, db)
		#! (db,w) = seqSt
			(\(req,res) (db,w) -> let (k,db`) = toRequestCacheKey db req in (db`,cachePages k CACHE_PREFETCH 0 zero res w))
			(fromMaybe [] suggestions)
			(db,w)
		#! suggestions
			= sortBy (\a b -> snd a > snd b) <$>
			  filter ((<) (length results) o snd) <$>
			  map (appSnd length) <$> suggestions
		#! (results,nextpages) = splitAt MAX_RESULTS results
		// Response
		#! response = if (isEmpty results)
			(err NoResults "No results")
			{ zero
		    & data           = results
		    , more_available = Just more
		    , suggestions    = suggestions
		    }
		// Save page prefetches
		#! w = cachePages key CACHE_PREFETCH 1 response nextpages w
		// Save cache file
		= respond key response db w
	where
		respond :: !RequestCacheKey !Response !*CloogleDB !*World -> *(!Response, !CacheKey, !*CloogleDB, !*World)
		respond key r db w = (r, cacheKey key, db, writeCache LongTerm key r w)

		cachePages :: !RequestCacheKey !Int !Int !Response ![Result] !*World -> *World
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

	suggs :: !(Maybe String) !Type !*CloogleDB -> *(Maybe [(Request, [Result])], *CloogleDB)
	suggs n (Func is r cc) db | length is < 3
	= appFst Just $ mapSt (\r -> appFst (tuple r) o search r o resetDB) reqs db
	where
		reqs = [{zero & name=n, unify=Just $ concat $ print False $ Func is` r cc}
			\\ is` <- permutations is | is` <> is]
	suggs _ _ db = (Nothing, db)

	reloadCache :: !*(!*CloogleDB, !*World) -> *(!*CloogleDB, !*World)
	reloadCache (db,w)
	# (ks,w) = allCacheKeys LongTerm w
	= loop ks db w
	where
		loop :: ![RequestCacheKey] !*CloogleDB !*World -> *(!*CloogleDB, !*World)
		loop [] db w = (db,w)
		loop [k:ks] db w
		# w = removeFromCache LongTerm k w
		# (_,_,db,w) = handle (Just $ fromRequestCacheKey k) db w
		# db = resetDB db
		= loop ks db w

	doInBackground :: (*a -> *a) *a -> *a
	doInBackground f w
	#! (pid,w) = fork w
	| pid  < 0 = abort "fork failed\n"
	| pid  > 0 = w // Parent: return directly
	| pid == 0 = snd $ exit 0 $ f w // Child: do function

:: LogMemory =
	{ mem_ip         :: IPAddress
	, mem_time_start :: Tm
	, mem_time_end   :: Tm
	, mem_request    :: Maybe Request
	}

instance zero LogMemory
where
	zero =
		{ mem_ip         = undef
		, mem_time_start = undef
		, mem_time_end   = undef
		, mem_request    = undef
		}

:: LogMessage` :== LogMessage (Maybe Request) Response CacheKey

:: LogEntry =
	{ ip            :: String
	, time_start    :: (String, Int)
	, time_end      :: (String, Int)
	, request       :: Maybe Request
	, cachekey      :: String
	, response_code :: Int
	, results       :: Int
	}

derive JSONEncode LogEntry

log :: LogMessage` (Maybe LogMemory) *World -> *(Maybe LogMemory, *World)
log msg mem w
# mem     = fromJust (mem <|> pure zero)
# (mem,w) = updateMemory msg mem w
| not needslog = (Just mem, w)
# (io,w)  = stdio w
# io      = io <<< toString (toJSON $ makeLogEntry msg mem) <<< "\n"
= (Just mem, snd (fclose io w))
where
	needslog = case msg of (Sent _ _) = True; _ = False

	updateMemory :: LogMessage` LogMemory *World -> *(LogMemory, *World)
	updateMemory (Connected ip) s w = ({s & mem_ip=ip}, w)
	updateMemory (Received r)   s w
	# (t,w) = localTime w
	= ({s & mem_time_start=t, mem_request=r}, w)
	updateMemory (Sent _ _)     s w
	# (t,w) = localTime w
	= ({s & mem_time_end=t}, w)
	updateMemory _                   s w = (s,w)

	makeLogEntry :: LogMessage` LogMemory -> LogEntry
	makeLogEntry (Sent response ck) mem =
		{ ip            = toString mem.mem_ip
		, time_start    = (toString mem.mem_time_start, toInt $ timeGm mem.mem_time_start)
		, time_end      = (toString mem.mem_time_end, toInt $ timeGm mem.mem_time_end)
		, request       = mem.mem_request
		, cachekey      = ck
		, response_code = response.return
		, results       = length response.data
		}

err :: CloogleError String -> Response
err c m = { return         = toInt c
          , data           = []
          , msg            = m
          , more_available = Nothing
          , suggestions    = Nothing
          }
