definition module Cache

from StdOverloaded import class toString
from Data.Maybe import :: Maybe
from Text.JSON import generic JSONEncode, generic JSONDecode, :: JSONNode

:: CacheType = Brief | LongTerm
:: CacheKey :== String

cacheKey :: (a -> CacheKey) | toString a

/**
 * Check if for the hash of the argument a JSON file exists of type `b`
 */
readCache :: !a !*World -> (!Maybe b, !*World) | toString a & JSONDecode{|*|} b

/**
 * All keys of a certain type currently in the cache
 */
allCacheKeys :: !CacheType !*World -> (![a], !*World) | JSONDecode{|*|} a

/**
 * Write for the hash of `a` a JSON file of type `b`
 */
writeCache :: CacheType !a !b !*World -> *World | toString, JSONEncode{|*|} a & JSONEncode{|*|} b
