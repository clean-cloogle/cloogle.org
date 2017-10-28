implementation module Cache

import StdFile
import StdFunc
import StdTuple

import Control.Applicative
import Control.Monad
import Crypto.Hash.MD5
import Data.Error
from Data.Func import $
import Data.Functor
import Data.Tuple
import System.Directory
import System.File
import System.FilePath
from Text import class Text(endsWith), instance Text String
import Text.JSON

cache_types :== [Brief, LongTerm]

typeToDir :: CacheType -> FilePath
typeToDir LongTerm = "lt"
typeToDir Brief = "brief"

cache_dir :: CacheType -> FilePath
cache_dir t = "." </> "cache" </> typeToDir t

cacheKey :: (a -> CacheKey) | toString a
cacheKey = md5 o toString

toCacheFile :: CacheType -> a -> FilePath | toString a
toCacheFile t = (</>) (cache_dir t) o cacheKey

readCache :: !a !*World -> (!Maybe b, !*World) | toString a & JSONDecode{|*|} b
readCache k w
# (files,w) = seqList [appFst error2mb o readFile (toCacheFile t k) \\ t <- cache_types] w
= (join $ fromJSON <$> fromString <$> foldl (<|>) empty files, w)

allCacheKeys :: !CacheType !*World -> (![a], !*World) | JSONDecode{|*|} a
allCacheKeys t w
# (fps,w) = appFst (fmap (map ((</>) (cache_dir t)) o filter (endsWith ".key")))
	$ readDirectory (cache_dir t) w
| isError fps = ([], w)
# (files,w) = seqList [appFst error2mb o readFile f \\ f <- fromOk fps] w
= (catMaybes $ catMaybes $ map (fmap (fromJSON o fromString)) files, w)

writeCache :: CacheType !a !b !*World -> *World | toString, JSONEncode{|*|} a & JSONEncode{|*|} b
writeCache t k v w
# (_,w) = writeFile file (toString $ toJSON v) w
# (_,w) = writeFile (file +++ ".key") (toString $ toJSON k) w
= w
where
	file = toCacheFile t k
