module CloogleServer

import StdString, StdArray, StdList, StdFile, StdTuple, StdMisc, StdOrdList, StdBool
from StdFunc import o

import TCPIP

from Data.Func import $
import Data.Maybe
import System.CommandLine
import Text
import Text.JSON
import Data.Functor
import Control.Applicative
from Control.Monad import class Monad(..)

import SimpleTCPServer
import TypeDB
import Type
import Levenshtein

:: Command = { unify :: String
             , name :: String
             }

:: Response = { return :: Int
              , data :: [Result]
              , msg :: String
              }

:: Result = { library :: String
            , filename :: String
            , func :: String
            , modul :: String
            , distance :: Int
            }

derive JSONEncode Command, Response, Result
derive JSONDecode Command, Response, Result

instance toString Response where toString r = toString $ toJSON r
instance toString Command where toString r = toString $ toJSON r

instance fromString (Maybe Command) where fromString s = fromJSON $ fromString s

instance < Result where (<) r1 r2 = r1.distance < r2.distance

err :: Int String -> Response
err c m = {return=c, data=[], msg=m}

Start w
# (io, w) = stdio w
# (cmdline, w) = getCommandLine w
| length cmdline <> 2 = help io w
# [_,port:_] = cmdline
# port = toInt port
# (db, io) = openDb io
# (_, w) = fclose io w
| isNothing db = abort "stdin does not have a TypeDB\n"
# db = fromJust db
= serve (handle db) (Just log) port w
where
    help :: *File *World -> *World
    help io w
    # io = io <<< "Usage: ./CloogleServer <port>\n"
    = snd $ fclose io w

    handle :: TypeDB (Maybe Command) *World -> *(Response, *World)
    handle _ Nothing w = (err 4 "Couldn't parse input", w)
    handle db (Just {unify,name}) w
        # mbType = parseType (fromString unify)
        # filters = catMaybes $ [ isUnifiable <$> mbType
                                , pure $ isNameMatch (size name - 2) name
                                ]
        # results = map (makeResult name mbType) $ findType`` filters db
        = ({return=0,msg="Success",data=sort results}, w)
    
    makeResult :: String (Maybe Type) (FunctionLocation, Type) -> Result
    makeResult orgsearch orgsearchtype (FL lib mod fname, type)
        = { library  = lib
          , filename = (toString $ reverse $ takeWhile ((<>)'.') $ reverse $ fromString mod) +++ ".dcl"
          , modul    = mod
          , func     = fname +++ " :: " +++ concat (print type)
          , distance = distance
          }
    where
        distance
            | orgsearch == ""
                | isNothing orgsearchtype = 0
                # orgsearchtype = fromJust orgsearchtype
                # (Just (ass1, ass2)) = unify [] orgsearchtype type
                = length $ filter (not o isVar o snd) $ ass1 ++ ass2
            # levdist = levenshtein fname orgsearch
            = if (indexOf orgsearch fname == -1) 0 -100 + levdist

    isUnifiable :: Type FunctionLocation Type -> Bool
    isUnifiable t1 _ t2 = isJust (unify [] t1 t2)

    isNameMatch :: Int String FunctionLocation Type -> Bool
    isNameMatch maxdist n1 (FL _ _ n2) _
        # (n1, n2) = ({toLower c \\ c <-: n1}, {toLower c \\ c <-: n2})
        = n1 == "" || indexOf n1 n2 <> -1 || levenshtein n1 n2 <= maxdist
    
    log :: (LogMessage (Maybe Command) Response) IPAddress *World -> *(IPAddress, *World)
    log msg s w
    # (io,w) = stdio w
    # io = fwrites (msgToString msg s) io
    = (newS msg s, snd (fclose io w))

    newS :: (LogMessage (Maybe Command) Response) IPAddress -> IPAddress
    newS m s = case m of (Connected ip) = ip; _ = s

    msgToString :: (LogMessage (Maybe Command) Response) IPAddress -> String
    msgToString (Received Nothing) ip
        = toString ip +++ " <-- Nothing\n"
    msgToString (Received (Just a)) ip
        = toString ip +++ " <-- " +++ toString a +++ "\n"
    msgToString (Sent b) ip
        = toString ip +++ " --> " +++ toString b +++ "\n"
    msgToString _ _ = ""

