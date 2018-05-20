definition module Util.Cache

from StdOverloaded import class toString
from Data.Maybe import :: Maybe
from Text.GenJSON import generic JSONEncode, generic JSONDecode, :: JSONNode

:: CacheType = Brief | LongTerm
:: CacheKey :== String

cacheKey :: (a -> CacheKey) | toString a

/**
 * Check if for the hash of the argument a JSON file exists of type `b`.
 */
readCache :: !a !*World -> (!Maybe b, !*World) | toString a & JSONDecode{|*|} b

/**
 * All keys of a certain type currently in the cache. The list is sorted in
 * ascending order of acess time.
 */
allCacheKeys :: !CacheType !*World -> (![a], !*World) | JSONDecode{|*|} a

/**
 * Write for the hash of `a` a JSON file of type `b`.
 */
writeCache :: !CacheType !a !b !*World -> *World | toString, JSONEncode{|*|} a & JSONEncode{|*|} b

/**
 * Remove an entry from the cache.
 */
removeFromCache :: !CacheType !a -> *World -> *World | toString a
