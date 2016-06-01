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

:: Request = { unify   :: String
             , name    :: String
             , modules :: Maybe [String]
             , page    :: Maybe Int
             }

:: Response = { return         :: Int
              , data           :: [Result]
              , msg            :: String
              , more_available :: Maybe Int
              }

:: Result = FunctionResult FunctionResult

:: BasicResult = { library  :: String
                 , filename :: String
                 , modul    :: String
                 , distance :: Int
                 }

:: FunctionResult :== (BasicResult, FunctionExtras)
:: FunctionExtras = { func     :: String
                    , unifier  :: Maybe StrUnifier
                    , cls      :: Maybe ClassResult
                    }

:: StrUnifier :== ([(String,String)], [(String,String)])

:: ErrorResult = Error Int String

:: ClassResult = { cls_name :: String, cls_vars :: [String] }

derive JSONEncode Request, Response, Result, ClassResult, BasicResult,
	FunctionExtras
derive JSONDecode Request, Response, Result, ClassResult, BasicResult,
	FunctionExtras

instance toString Response where toString r = toString (toJSON r) +++ "\n"
instance toString Request where toString r = toString $ toJSON r

instance fromString (Maybe Request) where fromString s = fromJSON $ fromString s

instance < BasicResult where (<) r1 r2 = r1.distance < r2.distance
instance < Result
where
	(<) r1 r2 = basic r1 < basic r2
	where
		basic :: Result -> BasicResult
		basic (FunctionResult (br,_)) = br

err :: Int String -> Response
err c m = {return=c, data=[], msg=m, more_available=Nothing}

E_NORESULTS :== 127
E_INVALIDINPUT :== 128
E_NAMETOOLONG :== 129

MAX_RESULTS :== 15

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

	handle :: TypeDB (Maybe Request) *World -> *(Response, *World)
	handle _ Nothing w = (err E_INVALIDINPUT "Couldn't parse input", w)
	handle db (Just {unify,name,modules,page}) w
		| size name > 40 = (err E_NAMETOOLONG "function name too long", w)
		# mbType = parseType (fromString unify)
		// Search normal functions
		# filts = catMaybes $ [ (\t->(\_ u->isUnifiable t u)) <$> mbType
		                      , pure (\loc _ ->
		                        isNameMatch (size name-2) name loc)
		                      , isModMatchF <$> modules
		                      ]
		# funs = map (makeFunctionResult name mbType Nothing) $ findFunction`` filts db
		// Search class members
		# filts = catMaybes $ [ (\t->(\_ _ _ u->isUnifiable t u)) <$> mbType
		                      , pure (\(CL lib mod _) _ f _ ->
		                        isNameMatch (size name-2) name (FL lib mod f))
		                      , isModMatchC <$> modules
		                      ]
		# members = findClassMembers`` filts db
		# members = map (\(CL lib mod cls,vs,f,et) -> makeFunctionResult name mbType
			(Just {cls_name=cls,cls_vars=vs}) (FL lib mod f,et)) members
		# drop_n = fromJust (page <|> pure 0) * MAX_RESULTS
		# results = drop drop_n $ sort $ funs ++ members
		# more = max 0 (length results - MAX_RESULTS)
		# results = take MAX_RESULTS results
		| isEmpty results = (err E_NORESULTS "No results", w)
		= ( { return = 0
		    , msg = "Success"
		    , data           = results
		    , more_available = Just more
		    }
		  , w)

	makeFunctionResult :: String (Maybe Type) (Maybe ClassResult)
	              (FunctionLocation, ExtendedType) -> Result
	makeFunctionResult
		orgsearch orgsearchtype mbCls (FL lib mod fname, ET type tes)
		= FunctionResult
		  ( { library  = lib
		    , filename = (toString $ reverse $ takeWhile ((<>)'.')
		                           $ reverse $ fromString mod) +++ ".dcl"
		    , modul    = mod
		    , distance = distance
		    }
		  , { func     = fname +++ toStrPriority tes.te_priority +++
		                 " :: " +++ concat (stripParens $ print type)
		    , unifier  = toStrUnifier <$> (orgsearchtype >>= unify [] type)
		    , cls      = mbCls
		    }
		  )
	where
		toStrUnifier :: Unifier -> StrUnifier
		toStrUnifier (tvas1, tvas2) = (map toStr tvas1, map toStr tvas2)
		where toStr (var, type) = (var, concat $ print type)

		toStrPriority :: (Maybe TE_Priority) -> String
		toStrPriority p = case print p of [] = ""; ss = concat [" ":ss]

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
				= sum [typeComplexity t \\ (_,t)<-ass1 ++ ass2 | not (isVar t)]
			# levdist = levenshtein fname orgsearch
			= if (indexOf orgsearch fname == -1) 0 -100 + levdist
		where
			typeComplexity :: Type -> Int
			typeComplexity (Type _ ts) = foldr ((+) o typeComplexity) 1 ts
			typeComplexity (Func is _ _) = foldr ((+) o typeComplexity) 5 is
			typeComplexity (Var _) = 1
			typeComplexity (Cons _ ts) = foldr ((+) o typeComplexity) 1 ts
			typeComplexity (Uniq t) = 3 + typeComplexity t

	isUnifiable :: Type ExtendedType -> Bool
	isUnifiable t1 (ET t2 _) = isJust (unify [] t1 t2)

	isNameMatch :: !Int !String FunctionLocation -> Bool
	isNameMatch maxdist n1 (FL _ _ n2)
		# (n1, n2) = ({toLower c \\ c <-: n1}, {toLower c \\ c <-: n2})
		= n1 == "" || indexOf n1 n2 <> -1 || levenshtein n1 n2 <= maxdist

	isModMatchF :: ![String] FunctionLocation ExtendedType -> Bool
	isModMatchF mods (FL _ mod _) _ = isMember mod mods

	isModMatchC :: ![String] ClassLocation [TypeVar] FunctionName ExtendedType -> Bool
	isModMatchC mods (CL _ mod _) _ _ _ = isMember mod mods

	log :: (LogMessage (Maybe Request) Response) IPAddress *World
	       -> *(IPAddress, *World)
	log msg s w
	| not needslog = (newS msg s, w)
	# (tm,w) = localTime w
	# (io,w) = stdio w
	# io = io <<< trim (toString tm) <<< " " <<< msgToString msg s
	= (newS msg s, snd (fclose io w))
	where
		needslog = case msg of (Received _) = True; (Sent _) = True; _ = False

	newS :: (LogMessage (Maybe Request) Response) IPAddress -> IPAddress
	newS m s = case m of (Connected ip) = ip; _ = s

	msgToString :: (LogMessage (Maybe Request) Response) IPAddress -> String
	msgToString (Received Nothing) ip
		= toString ip +++ " <-- Nothing\n"
	msgToString (Received (Just a)) ip
		= toString ip +++ " <-- " +++ toString a +++ "\n"
	msgToString (Sent {return,data,msg,more_available}) ip
		= toString ip +++ " --> " +++ toString (length data)
			+++ " results (" +++ toString return +++ "; " +++ msg +++
			if (isJust more_available) ("; " +++ toString (fromJust more_available) +++ " more") "" +++ ")\n"
	msgToString _ _ = ""

