implementation module Cache

import StdFunc
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

toCacheFile :: (a -> FilePath) | toString a
toCacheFile = ((</>) "./cache") o md5 o toString

readCache :: !a !*World -> (Maybe b, !*World) | toString a & JSONDecode{|*|} b
readCache k w = appFst (join o fmap (fromJSON o fromString) o error2mb) $ readFile (toCacheFile k) w

writeCache :: !a !b !*World -> (!b, !*World) | toString a & JSONEncode{|*|} b
writeCache k v w = appFst (const v) $ writeFile (toCacheFile k) (toString $ toJSON v) w
