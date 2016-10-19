implementation module Cache

import StdFunc
import StdTuple
import StdOrdList
from Data.Func import $
import Control.Monad
import Control.Applicative
import Data.Functor
import Crypto.Hash.MD5
import Text.JSON
import Data.Error
import StdFile
import System.Directory
import System.FilePath
import System.File
import System.Time
import Data.Tuple
import GenLexOrd

CACHE_DIR :== "./cache"
CACHE_SIZE :== 100

toCacheFile :: (a -> FilePath) | toString a
toCacheFile = ((</>) CACHE_DIR) o md5 o toString

readCache :: !a -> *World -> (Maybe b, !*World) | toString a & JSONDecode{|*|} b
readCache k = appFst (join o fmap (fromJSON o fromString) o error2mb) o readFile (toCacheFile k)

writeCache :: !a !b -> *World -> *World | toString a & JSONEncode{|*|} b
writeCache k v = purgeCache o snd o writeFile (toCacheFile k) (toString $ toJSON v)

purgeCache :: *World -> *World
purgeCache w
# (fs,w)    = readDirectory CACHE_DIR w
| isError fs
	= w
# fs        = map ((</>) CACHE_DIR) $ fromOk fs
| length fs < CACHE_SIZE
	= w
# (infos,w) = seqList (map (\f st -> let (i,w) = getFileInfo f st in ((f,i),w)) fs) w
| any (isError o snd) infos
	= w
# infos     = map (appSnd fromOk) infos
# infos     = sortBy (\(_,a) (_,b) -> usedLess a b) infos
= snd $ deleteFile (fst $ hd infos) w
where
	usedLess :: FileInfo FileInfo -> Bool
	usedLess a b = a.lastAccessedTime < b.lastAccessedTime

instance < Tm
where
	(<) a b = LT === ([a.year,a.yday,a.hour,a.min,a.sec] =?= [b.year,b.yday,b.hour,b.min,b.sec])
