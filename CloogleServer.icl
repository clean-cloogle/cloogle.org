module CloogleServer

import StdArray, StdBool, StdFile, StdList, StdOrdList, StdOverloaded, StdTuple
from StdFunc import o
from StdMisc import abort

from TCPIP import :: IPAddress, :: Port, instance toString IPAddress

from Data.Func import $
import Data.Maybe
import System.CommandLine
import Text.JSON
import Data.Functor
import Control.Applicative
import Control.Monad
from Text import class Text(concat,trim,indexOf), instance Text String

import System.Time

import qualified StdMaybe as OldMaybe
from SimpleTCPServer import :: LogMessage{..}, serve, :: Logger
import qualified SimpleTCPServer
import TypeDB
import Type
import Levenshtein

:: OldMaybe a :== 'SimpleTCPServer'.Maybe a

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
			, unifier :: Maybe StrUnifier
            , cls :: Maybe ClassResult
            , modul :: String
            , distance :: Int
            }

:: StrUnifier :== ([(String,String)], [(String,String)])

:: ErrorResult = Error Int String

:: ClassResult = { cls_name :: String, cls_vars :: [String] }

derive JSONEncode Command, Response, Result, ClassResult
derive JSONDecode Command, Response, Result, ClassResult

instance toString Response where toString r = toString $ toJSON r
instance toString Command where toString r = toString $ toJSON r

instance fromString (Maybe Command) where fromString s = fromJSON $ fromString s

instance < Result where (<) r1 r2 = r1.distance < r2.distance

err :: Int String -> Response
err c m = {return=c, data=[], msg=m}

MAX_RESULTS :== 100

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
= serve (handle db) ('OldMaybe'.Just log) port w
where
	help :: *File *World -> *World
	help io w
	# io = io <<< "Usage: ./CloogleServer <port>\n"
	= snd $ fclose io w

	handle :: TypeDB (Maybe Command) *World -> *(Response, *World)
	handle _ Nothing w = (err 4 "Couldn't parse input", w)
	handle db (Just {unify,name}) w
		# mbType = parseType (fromString unify)
		// Search normal functions
		# filts = catMaybes $ [ (\t->(\_ u->isUnifiable t u)) <$> mbType
		                      , pure (\loc _ ->
		                        isNameMatch (size name-2) name loc)
		                      ]
		# funcs = map (makeResult name mbType Nothing) $ findType`` filts db
		// Search class members
		# filts = catMaybes $ [ (\t->(\_ _ _ u->isUnifiable t u)) <$> mbType
		                      , pure (\(CL lib mod _) _ f _ ->
		                        isNameMatch (size name-2) name (FL lib mod f))
		                      ]
		# members = findClassMembers`` filts db
		# members = map (\(CL lib mod cls,vs,f,t) -> makeResult name mbType
			(Just {cls_name=cls,cls_vars=vs}) (FL lib mod f,t)) members
		# results = take MAX_RESULTS $ sort $ funcs ++ members
		= ({return=0,msg="Success",data=results}, w)

	makeResult :: String (Maybe Type) (Maybe ClassResult)
	              (FunctionLocation, Type) -> Result
	makeResult orgsearch orgsearchtype mbCls (FL lib mod fname, type)
		= { library  = lib
		  , filename = (toString $ reverse $ takeWhile ((<>)'.')
		                         $ reverse $ fromString mod) +++ ".dcl"
		  , modul    = mod
		  , func     = fname +++ " :: " +++ concat (stripParens $ print type)
		  , unifier  = toStrUnifier <$> (orgsearchtype >>= unify [] type)
		  , cls      = mbCls
		  , distance = distance
		  }
	where
		toStrUnifier :: Unifier -> StrUnifier
		toStrUnifier (tvas1, tvas2) = (map toStr tvas1, map toStr tvas2)
		where toStr (var, type) = (var, concat $ print type)

		stripParens :: [String] -> [String]
		stripParens ["(":ss]
			| last ss == ")" && parensMatch 0 (init ss) = stripParens $ init ss
			| otherwise = ["(":ss]
		stripParens ss = ss

		parensMatch :: Int [String] -> Bool
		parensMatch 0 []       = True
		parensMatch _ []       = False
		parensMatch i ["(":ss] = i >= 0 && parensMatch (i+1) ss
		parensMatch i [")":ss] = i >= 0 && parensMatch (i-1) ss
		parensMatch i [_:ss]   = i >= 0 && parensMatch i     ss

		distance
			| orgsearch == ""
				| isNothing orgsearchtype = 0
				# orgsearchtype = fromJust orgsearchtype
				# (Just (ass1, ass2)) = unify [] orgsearchtype type
				= length $ filter (not o isVar o snd) $ ass1 ++ ass2
			# levdist = levenshtein fname orgsearch
			= if (indexOf orgsearch fname == -1) 0 -100 + levdist

	isUnifiable :: Type Type -> Bool
	isUnifiable t1 t2 = isJust (unify [] t1 t2)

	isNameMatch :: Int String FunctionLocation -> Bool
	isNameMatch maxdist n1 (FL _ _ n2)
		# (n1, n2) = ({toLower c \\ c <-: n1}, {toLower c \\ c <-: n2})
		= n1 == "" || indexOf n1 n2 <> -1 || levenshtein n1 n2 <= maxdist

	log :: (LogMessage (Maybe Command) Response) IPAddress *World
	       -> *(IPAddress, *World)
	log msg s w
	| not needslog = (newS msg s, w)
	# (tm,w) = localTime w
	# (io,w) = stdio w
	# io = io <<< trim (toString tm) <<< " " <<< msgToString msg s
	= (newS msg s, snd (fclose io w))
	where
		needslog = case msg of (Received _) = True; (Sent _) = True; _ = False

	newS :: (LogMessage (Maybe Command) Response) IPAddress -> IPAddress
	newS m s = case m of (Connected ip) = ip; _ = s

	msgToString :: (LogMessage (Maybe Command) Response) IPAddress -> String
	msgToString (Received Nothing) ip
		= toString ip +++ " <-- Nothing\n"
	msgToString (Received (Just a)) ip
		= toString ip +++ " <-- " +++ toString a +++ "\n"
	msgToString (Sent {return,data,msg}) ip
		= toString ip +++ " --> " +++ toString (length data) +++ " results ("
			+++ toString return +++ "; " +++ msg +++ ")\n"
	msgToString _ _ = ""

