module CloogleServer

import StdArray
import StdBool
import StdFile
from StdFunc import o, seq
from StdMisc import abort, undef
import StdOrdList
import StdOverloaded
import StdTuple

from TCPIP import :: IPAddress, :: Port, instance toString IPAddress

import Control.Applicative
import Control.Monad
import qualified Data.Foldable as Foldable
from Data.Foldable import class Foldable, instance Foldable Maybe
from Data.Func import $
import Data.Functor
import Data.List
import Data.Tuple
import System._Posix
import System.CommandLine
import System.Time
from Text import class Text(concat), instance Text String
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
# (cmdline, w) = getCommandLine w
| length cmdline <> 2
	= help w
# [_,port:_] = cmdline
# w = disableSwap w
#! (_,f,w) = fopen "types.json" FReadText w
#! (db,f) = openDb f
#! db = eval_all_nodes db
#! (_,w) = fclose f w
= serve
	{ handler           = handle db
	, logger            = Just log
	, port              = toInt port
	, connect_timeout   = Just 3600000 // 1h
	, keepalive_timeout = Just 5000    // 5s
	} w
where
	help :: *World -> *World
	help w
	# (io, w) = stdio w
	# io = io <<< "Usage: ./CloogleServer <port>\n"
	= snd $ fclose io w

	disableSwap :: *World -> *World
	disableSwap w
	# (ok,w) = mlockall (MCL_CURRENT bitor MCL_FUTURE) w
	| ok = w
	# (err,w) = errno w
	# (io,w) = stdio w
	# io = io <<< "Could not lock memory (" <<< err <<< "); process may get swapped out\n"
	= snd $ fclose io w

	eval_all_nodes :: !.a -> .a // From GraphCopy
	eval_all_nodes g = code {
		push_a 0
		.d 1 0
		jsr	_eval_to_nf
		.o 0 0
	}

	handle :: !CloogleDB !(Maybe Request) !*World -> *(!Response, CacheKey, !*World)
	handle db Nothing w = (err InvalidInput "Couldn't parse input", "", w)
	handle db (Just request=:{unify,name,page}) w
		//Check cache
		#! (mbResponse, w) = readCache key w
		| isJust mbResponse
			# r = fromJust mbResponse
			= ({r & return = if (r.return == 0) 1 r.return}, cacheKey key, w)
		| isJust name && size (fromJust name) > 40
			= respond (err InvalidName "Function name too long") w
		| isJust name && any isSpace (fromString $ fromJust name)
			= respond (err InvalidName "Name cannot contain spaces") w
		| isJust unify && isNothing (parseType $ fromString $ fromJust unify)
			= respond (err InvalidType "Couldn't parse type") w
		// Results
		#! drop_n = fromJust (page <|> pure 0) * MAX_RESULTS
		#! results = drop drop_n $ sort $ search request db
		#! more = max 0 (length results - MAX_RESULTS)
		// Suggestions
		#! suggestions = unify >>= parseType o fromString >>= flip (suggs name) db
		#! w = seq [cachePages
				(toRequestCacheKey req) CACHE_PREFETCH 0 zero suggs
				\\ (req,suggs) <- 'Foldable'.concat suggestions] w
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
		= respond response w
	where
		key = toRequestCacheKey request

		respond :: !Response !*World -> *(!Response, !CacheKey, !*World)
		respond r w = (r, cacheKey key, writeCache LongTerm key r w)

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

	suggs :: !(Maybe String) !Type !CloogleDB -> Maybe [(Request, [Result])]
	suggs n (Func is r cc) db
		| length is < 3
			= Just [let t` = concat $ print False $ Func is` r cc in
			        let request = {zero & name=n, unify=Just t`} in
			        (request, search request db)
			        \\ is` <- permutations is | is` <> is]
	suggs _ _ _ = Nothing

:: LogMemory =
	{ mem_ip         :: IPAddress
	, mem_time_start :: Tm
	, mem_time_end   :: Tm
	, mem_request    :: Request
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
	, request       :: Request
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
	updateMemory (Connected ip)      s w = ({s & mem_ip=ip}, w)
	updateMemory (Received (Just r)) s w
	# (t,w) = localTime w
	= ({s & mem_time_start=t, mem_request=r}, w)
	updateMemory (Sent _ _)          s w
	# (t,w) = localTime w
	= ({s & mem_time_end=t}, w)
	updateMemory _                   s w = (s,w)

	makeLogEntry :: LogMessage` LogMemory -> LogEntry
	makeLogEntry (Sent response ck) mem =
		{ ip            = toString mem.mem_ip
		, time_start    = (toString mem.mem_time_start, toInt $ mkTime mem.mem_time_start)
		, time_end      = (toString mem.mem_time_end, toInt $ mkTime mem.mem_time_end)
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
