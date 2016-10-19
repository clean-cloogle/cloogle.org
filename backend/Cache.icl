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

CACHE_DIR :== "./cache"

toCacheFile :: (a -> FilePath) | toString a
toCacheFile = ((</>) CACHE_DIR) o md5 o toString

readCache :: !a -> *World -> (Maybe b, !*World) | toString a & JSONDecode{|*|} b
readCache k = appFst (join o fmap (fromJSON o fromString) o error2mb) o readFile (toCacheFile k)

writeCache :: !a !b -> *World -> *World | toString a & JSONEncode{|*|} b
writeCache k v = snd o writeFile (toCacheFile k) (toString $ toJSON v)
