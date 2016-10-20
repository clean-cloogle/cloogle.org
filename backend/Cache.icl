implementation module Cache

import StdFunc
import StdTuple
from Data.Func import $
import Control.Monad
import Control.Applicative
import Data.Functor
import Crypto.Hash.MD5
import Text.JSON
import Data.Error
import StdFile
import System.FilePath
import System.File
import Data.Tuple

cache_types :== [Brief, LongTerm]

cache_dir :: CacheType -> FilePath
cache_dir LongTerm = "./cache/lt"
cache_dir Brief = "./cache/brief"

toCacheFile :: CacheType -> a -> FilePath | toString a
toCacheFile t = (</>) (cache_dir t) o md5 o toString

readCache :: !a *World -> (Maybe b, !*World) | toString a & JSONDecode{|*|} b
readCache k w
# (files,w) = seqList [appFst error2mb o readFile (toCacheFile t k) \\ t <- cache_types] w
= (join o fmap (fromJSON o fromString) $ foldl (<|>) empty files, w)

writeCache :: CacheType !a !b -> *World -> *World | toString a & JSONEncode{|*|} b
writeCache t k v = snd o writeFile (toCacheFile t k) (toString $ toJSON v)
