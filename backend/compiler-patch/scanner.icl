implementation module scanner

import	StdEnv, compare_constructor, general

from utilities import revCharListToString, isSpecialChar
from CoclSystemDependent import DirectorySeparator

// RWS Proof ... ::	SearchPaths	:== [String]
:: SearchPaths = 
	{ sp_locations   :: [(String, String)]       // (module, path)
	, sp_paths       :: [String]
	}

:: ModTimeFunction f
	:== ({#Char} f -> *(!{#Char}, !f))

// ... RWS

::	*ScanState = ScanState !RScanState

instance getFilename ScanState
where
	getFilename (ScanState scan_state)
		# (file_name,scan_state) = getFilename scan_state
		= (file_name,ScanState scan_state)

instance tokenBack ScanState
where
	tokenBack (ScanState scan_state) = ScanState (tokenBack scan_state)

instance nextToken ScanState
where
	nextToken context (ScanState scan_state=:{ss_scanOptions})
		# (token,scan_state) = nextToken context scan_state
		= (replaceUnderscoreToken token ((ss_scanOptions bitand ScanOptionUnderscoreIdentsBit) <> 0),
				ScanState scan_state)
		where
			replaceUnderscoreToken :: Token !Bool -> Token
			replaceUnderscoreToken (UnderscoreIdentToken name) underscoreModule
				| underscoreModule
					=	IdentToken name
			replaceUnderscoreToken token _
				=	token

instance currentToken ScanState
where
	currentToken (ScanState scan_state)
		# (token,scan_state) = currentToken scan_state
		= (token,ScanState scan_state) 
/*
instance insertToken ScanState
where
	insertToken token context (ScanState scan_state) = ScanState (insertToken token context scan_state)

instance replaceToken ScanState
where
	replaceToken token (ScanState scan_state) = ScanState (replaceToken token scan_state)
*/
instance getPosition ScanState
where
	getPosition (ScanState scan_state)
		# (position,scan_state) = getPosition scan_state
		= (position,ScanState scan_state)

::	* RScanState =
	{	ss_input		::	ScanInput
	,	ss_offsides		::	! [(Int, Bool) ]	// (column, defines newDefinition)
	,	ss_scanOptions	::	! Int
	,	ss_tokenBuffer	::	! Buffer LongToken
	}

ScanOptionUseLayoutBit :== 1
ScanOptionUnderscoreIdentsBit :== 2

ScanOptionNoNewOffsideForSeqLetBit:==4;

::	* ScanInput
	=	Input			Input
	|	PushedToken		LongToken ScanInput

::	* Input =
	{	inp_stream		::	! * InputStream
	,	inp_filename	::	!String
	,	inp_pos			::	! FilePosition
	,	inp_tabsize		::	! Int
	}

::	* InputStream
	=	InFile			* File
	|	OldLine 		!Int !{#Char} !InputStream

::	FilePosition =
	{	fp_line			::	! Int
	,	fp_col			::	! Int
	}

::	LongToken =
	{	lt_position		::	! FilePosition	// Start position of this token
	,	lt_index		::	! Int			// The index in the current line
	,	lt_token		::	! Token			// The token itself
	,	lt_context		::	! ScanContext	// The context of the token
	}

::	*Buffer x:==SBuffer x
::	SBuffer x
	=	Buffer0
	|	Buffer1 x
	|	Buffer2 x x
	|	Buffer3 x x x // buffer size is 3.

::	Token
	= 	IdentToken ! .String	//		an identifier
	| 	UnderscoreIdentToken !.String//	an identifier that starts with a '_'
	| 	QualifiedIdentToken !String !.String	//	a qualified identifier
	|	IntToken !.String		//		an integer
	|	RealToken !.String		//		a real
	|	StringToken !.String	//		a string
	|	CharToken !.String		//		a character
	|	CharListToken !.String	//		a character list '{char}*'
	|	BoolToken !Bool			//		a boolean
	|	OpenToken				//		(
	|	CloseToken				//		)
	|	CurlyOpenToken			//		{
	|	CurlyCloseToken			//		}
	|	SquareOpenToken			//		[
	|	SquareCloseToken		//		]

	|	DotToken				//		.
	|	SemicolonToken			//		;
	|	ColonToken				//		:
	|	DoubleColonToken		//		::
	|	CommaToken				//		,
	|	ExclamationToken		//		!
	|	BarToken				//		|
	|	ArrowToken				//		->
	|	DoubleArrowToken		//		=>
	|	EqualToken				//		=
	|	DefinesColonToken		//		=:
	|	ColonDefinesToken		//		:==
	|	WildCardToken			//		_
	|	BackSlashToken			//		\
	|	DoubleBackSlashToken	//		\\
	|	LeftArrowToken			//		<-
	|	LeftArrowColonToken		//		<-:
	|	LeftArrowWithBarToken	//		<|-
	|	DotDotToken				//		..
	|	AndToken				//		&
	|	HashToken				//		#
	|	AsteriskToken			//		*
	|	LessThanOrEqualToken	//		<=

	|	ModuleToken				//		module
	|	ImpModuleToken			//		implementation
	|	DefModuleToken			//		definition
	|	SysModuleToken			//		system

	|	ImportToken				//		import
	|	FromToken				//		from
	|	SpecialToken			//		special
	|	ForeignToken			//		foreign

	|	IntTypeToken			//		Int
	|	CharTypeToken			//		Char
	|	RealTypeToken			//		Real
	|	BoolTypeToken			//		Bool
	|	StringTypeToken			//		String
	|	FileTypeToken			//		File
	|	WorldTypeToken			//		World
	|	ClassToken				//		class
	|	InstanceToken			//		instance
	|	OtherwiseToken			//		otherwise

	|	IfToken					//		if
	|	WhereToken				//		where
	|	WithToken				//		with
	|	CaseToken				//		case
	|	OfToken					//		of
	|	LetToken Bool			//		let!, let
	|	SeqLetToken Bool		//		#!, #
	|	InToken					//		in

	|	DynamicToken			//		dynamic
	|	DynamicTypeToken		//		Dynamic

	|	PriorityToken Priority	//		infixX N

	|	CodeToken				//		code
	|	InlineToken				//		inline
	|	CodeBlockToken [String]	//		{...}

	|	NewDefinitionToken		//		generated automatically, OffsideToken.
	|	EndGroupToken			//		generated automatically
	|	EndOfFileToken			//		end of file
	|	ErrorToken String		//		an error has occured

	| 	GenericToken			//		generic
	| 	DeriveToken				//		derive
	|	GenericOpenToken		//		{|
	|	GenericCloseToken		//		|}
	|	GenericOfToken			//		of
	|	GenericWithToken		//		with

	|	ExistsToken				//		E.
	|	ForAllToken				//		A.

::	ScanContext
	=	GeneralContext
	|	TypeContext
	|	FunctionContext
	|	CodeContext
	| 	GenericContext
	|	ModuleNameContext

instance == ScanContext
where
	(==) co1 co2 = equal_constructor co1 co2

::	Assoc
	=	LeftAssoc
	|	RightAssoc
	|	NoAssoc

::	Priority
	=	Prio Assoc Int
	|	NoPrio

//
//	Macros for error messages
//
ScanErrIllegal	:== "illegal char in input"
ScanErrCharErr	:== "wrong character denotation"
ScanErrNLString	:== "new line in string denotation"

class getFilename state :: !*state -> (!String,!*state)

instance getFilename ScanInput
where
	getFilename (Input input)
		# (filename,input) = input!inp_filename
		= (filename,Input input)
	getFilename (PushedToken tok input)
		# (filename,input) = getFilename input
		= (filename,PushedToken tok input)

instance getFilename RScanState
where
	getFilename scanState=:{ss_input}
		# (filename,ss_input) = getFilename ss_input
		= (filename,{scanState & ss_input = ss_input })

class getPosition state :: !*state -> (!FilePosition,!*state)  // Position of current Token (or Char)

instance getPosition RScanState
where
	getPosition scanState=:{ss_tokenBuffer}
		| isEmptyBuffer ss_tokenBuffer
			= getCharPosition scanState
		# (ltok,ss_tokenBuffer) = head ss_tokenBuffer
		= (ltok.lt_position, {scanState & ss_tokenBuffer=ss_tokenBuffer})

instance getPosition Input
where
	getPosition input=:{inp_pos} = (inp_pos, input)

class getCharPosition state :: !*state -> (FilePosition,!*state)

instance getCharPosition RScanState
where
	getCharPosition scanState=:{ss_input=Input input}
		# (pos,input) = getPosition input
		= (pos,{ scanState & ss_input = Input input })
	getCharPosition scanState=:{ss_input=PushedToken longToken _}
		= (longToken.lt_position,scanState)

instance getCharPosition Input
where getCharPosition input=:{inp_pos} = (inp_pos, input)

class getIndex input :: !*input -> (!Int, !*input)

instance getIndex InputStream
where
	getIndex input=:(OldLine index _ _) = (index-1,input)
	getIndex input = (0,input)

instance getIndex Input
where
	getIndex input=:{inp_stream=stream}
		# (index,stream) = getIndex stream
		= (index,{input & inp_stream=stream})

class nextToken state :: !ScanContext !*state -> (!Token, !*state)

instance nextToken RScanState
where
	nextToken newContext (scanState=:{ss_input=inp=:PushedToken token=:{lt_position,lt_token,lt_context,lt_index} rest_inp,ss_tokenBuffer,ss_offsides,ss_scanOptions})
		| lt_context == newContext || notContextDependent lt_token
		=	(	lt_token
			,	{ scanState & ss_input = rest_inp , ss_tokenBuffer	= store token ss_tokenBuffer }
			)  -->> ("nextToken: pushed token", lt_token)
		= token_back rest_inp
		where
			token_back input=:(Input {inp_pos,inp_stream=OldLine currentIndex string stream,inp_filename,inp_tabsize}) // one old token in wrong context.
				|	inp_pos.fp_line == lt_position.fp_line
				#	old_input
					 =	{ inp_stream	= OldLine (lt_index+1) string stream
						, inp_filename	= inp_filename
						, inp_pos		= lt_position
						, inp_tabsize	= inp_tabsize
						} -->> ("token_back in input", lt_token)
				#	c				= string.[lt_index]
				#	(token, inp)	= Scan c old_input newContext
				= ( token
				  , {	ss_input 		= Input inp
					,	ss_tokenBuffer	= store
											{	lt_position 	= lt_position
											,	lt_index		= lt_index
											,	lt_token		= token
											,	lt_context		= newContext
											}
											(pop ss_tokenBuffer)
					,	ss_offsides=ss_offsides
					,	ss_scanOptions=ss_scanOptions
					}
				  )	 -->> ("renewed token",token,lt_position)
				=	(	lt_token
					,	{ss_input = input , ss_tokenBuffer	= store token ss_tokenBuffer, ss_offsides=ss_offsides, ss_scanOptions=ss_scanOptions}
					) -->> ("unable to push token_back in input; line is lost",(inp_pos.fp_line,lt_position.fp_line), lt_token)
			token_back input
				=	(	lt_token
					,	{ss_input = input , ss_tokenBuffer	= store token ss_tokenBuffer, ss_offsides=ss_offsides, ss_scanOptions=ss_scanOptions}
					) -->> ("unable to push token_back in input; generated token", lt_token)

	nextToken context {ss_input=Input inp,ss_tokenBuffer,ss_offsides,ss_scanOptions}
		# (error, c, inp) 	= SkipWhites inp
		  (pos, inp)		= inp!inp_pos
		  (index,inp)		= getIndex inp
		= case error of
			Yes string
				->	( ErrorToken string
							,	{	ss_tokenBuffer	= store
												{	lt_position 	= pos
												,	lt_index		= index
												,	lt_token		= ErrorToken string
												,	lt_context		= context
												}
												ss_tokenBuffer,
									ss_input=Input inp,
									ss_offsides=ss_offsides,	ss_scanOptions=ss_scanOptions
								}
							) -->> ("Error token generated",string)
			no
				#	(eof, inp)	= EndOfInput inp
				|	eof && c == NewLineChar
					#	newToken	= EndOfFileToken
					->	checkOffside pos index newToken
							{ ss_tokenBuffer	= store
													{	lt_position 	= pos
													,	lt_index		= index
													,	lt_token		= newToken
													,	lt_context		= context
													}
													ss_tokenBuffer
							, ss_input = Input inp,
							ss_offsides=ss_offsides,	ss_scanOptions=ss_scanOptions
							} // -->> ("Token", EndOfFileToken,pos)
				// otherwise // ~ (eof && c == NewLineChar)
					#	(token, inp)	= Scan c inp context
					-> checkOffside pos index token
						{ ss_input 			= Input inp
						, ss_tokenBuffer	= store
												{	lt_position 	= pos
												,	lt_index		= index
												,	lt_token		= token
												,	lt_context		= context
												}
												ss_tokenBuffer,
							ss_offsides=ss_offsides,	ss_scanOptions=ss_scanOptions
						}	 -->> (token,pos)
	nextToken _ _ = abort "Scanner: Error in nextToken"

class tokenBack state :: !*state -> *state

instance tokenBack RScanState
where
	tokenBack scanState=:{ss_tokenBuffer, ss_input}
		| isEmptyBuffer ss_tokenBuffer = abort "tokenBack with empty token buffer"
		# (tok, buf) = get ss_tokenBuffer
		=	{ scanState
			& ss_tokenBuffer	= buf
			, ss_input			= PushedToken tok ss_input
		} // -->> ("tokenBack", tok, buf)

class currentToken state :: !*state -> (!Token, !*state)

instance currentToken RScanState
where currentToken scanState=:{ss_tokenBuffer}
		| isEmptyBuffer ss_tokenBuffer
			= (ErrorToken "dummy", scanState)
			# (ltok,ss_tokenBuffer) = head ss_tokenBuffer
			= (ltok.lt_token, {scanState & ss_tokenBuffer=ss_tokenBuffer})
/*
class insertToken state :: !Token !ScanContext !*state -> *state

instance insertToken RScanState
where
	insertToken t c scanState
		#	(pos, scanState=:{ss_input}) = getPosition scanState
		=	{ scanState
			& ss_input = PushedToken
							{ lt_position	= pos
							, lt_index		= pos.fp_col
							, lt_token		= t
							, lt_context	= c
							}
							ss_input
			}
*/
notContextDependent :: !Token -> Bool
notContextDependent token
 = case token of
	NewDefinitionToken	-> True
	EndGroupToken		-> True
	EndOfFileToken		-> True
	InToken				-> True
	ErrorToken _		-> True
	CodeBlockToken _	-> True
	OpenToken			-> True
	CloseToken			-> True
	CurlyOpenToken		-> True
	CurlyCloseToken		-> True
	SquareOpenToken		-> True
	SquareCloseToken	-> True
	SemicolonToken		-> True
	CommaToken			-> True
	ClassToken			-> True
	InstanceToken		-> True
	OtherwiseToken		-> True
	IfToken				-> True
	WhereToken			-> True
	WithToken			-> True
	_					-> False
/*
class replaceToken state :: !Token !*state -> *state

instance replaceToken RScanState
where
	replaceToken tok scanState=:{ss_tokenBuffer}
		# (longToken,buffer) = get ss_tokenBuffer
		= { scanState
		  & ss_tokenBuffer = store { longToken & lt_token = tok } buffer
		  }
*/
SkipWhites :: !Input -> (!Optional String, !Char, !Input)
SkipWhites {inp_stream=OldLine i line stream,inp_pos={fp_line,fp_col},inp_tabsize,inp_filename}
	| i<size line
		= skip_whites_in_line i fp_col fp_line line inp_tabsize stream inp_filename
SkipWhites input
	# (eof, c, input)		= ReadChar input
	| eof					= (No, NewLineChar, input)
	| IsWhiteSpace c		= SkipWhites input
							= TryScanComment c input

skip_whites_in_line :: !Int !Int !Int !{#Char} !Int !*InputStream !String -> *(!Optional String,!Char,!*Input);
skip_whites_in_line i fp_col fp_line line tabsize stream inp_filename
	| i<size line
		# c=line.[i]
		| c==' ' || c == '\f' || c == '\v'
			= skip_whites_in_line (i+1) (fp_col+1) fp_line line tabsize stream inp_filename
		| c=='\t'
			= skip_whites_in_line (i+1) (tabsize * (fp_col / tabsize + 1)) fp_line line tabsize stream inp_filename
		| c==LFChar || c==CRChar
			# pos = {fp_line = fp_line + 1, fp_col = 0}
//			#	(c,stream)	= correctNewline_OldLine c i tabsize line stream
			=	SkipWhites  {
						inp_filename=inp_filename,inp_tabsize=tabsize,
						inp_stream = stream
					,	inp_pos	= pos
					}
			#	pos = {fp_line=fp_line,fp_col = fp_col + 1}
			=	TryScanComment c {
						inp_filename=inp_filename,inp_tabsize=tabsize,
						inp_stream = OldLine (i+1) line stream
					,	inp_pos	= pos
					}
	#	pos = {fp_line=fp_line, fp_col = fp_col}
	= SkipWhites {
				inp_filename=inp_filename,inp_tabsize=tabsize,
				inp_stream = stream
			,	inp_pos	= pos
			}

TryScanComment :: !Char !Input -> (!Optional String, !Char, !Input)
TryScanComment c1=:'/' input
	# (eof,c2, input)		= ReadNormalChar input
	| eof					= (No, c1, input)
	= case c2 of
		'/' -> SkipWhites (SkipToEndOfLine input)
		'*' -> case ScanComment input of
				(No,input)	-> SkipWhites input
				(er,input)	-> (er, c1, input)
		_   -> (No, c1, charBack input)
TryScanComment c input
	= (No, c, input)

ScanComment	:: !Input -> (!Optional String, !Input)
ScanComment {inp_stream=OldLine i line stream,inp_pos={fp_line,fp_col},inp_tabsize,inp_filename}
	| i<size line
		= scan_comment_in_line i fp_col fp_line line inp_tabsize stream inp_filename
ScanComment input
	# (eof1, c1, input)	= ReadChar input
	| eof1				= (Yes "end of file encountered inside comment", input)
		= ScanComment2 c1 input;

ScanComment2	:: !Char !Input -> (!Optional String, !Input)
ScanComment2 c1 input
	| c1 == '/'
		# (eof2, c2, input)	= ReadChar input
		| eof2				= (Yes "end of file encountered inside comment", input)
							= case c2 of
								'/'	->	ScanComment (SkipToEndOfLine input)
								'*'	->	case ScanComment input of
											(No, input) -> ScanComment input
											error		-> error
								_	->	ScanComment input
	| c1 == '*'
		# (eol2, c2, input)	= ReadNormalChar input
		| eol2	
			# (eof2, c2, input)	= ReadChar input
			| eof2
			= (Yes "end of file encountered inside comment", input)
			= ScanComment input
		| c2 == '/'			= (No, input)
		| c2 == '*'
//					= ScanComment (charBack input)
					= ScanComment2 c2 input
							= ScanComment input
	| otherwise				= ScanComment input

scan_comment_in_line :: !Int !Int !Int !{#Char} !Int !*InputStream !String -> (!Optional String, !Input)
scan_comment_in_line i fp_col fp_line line tabsize stream inp_filename
	| i<size line
		# c=line.[i]
		| c=='\t'
			= scan_comment_in_line (i+1) (tabsize * (fp_col / tabsize + 1)) fp_line line tabsize stream inp_filename
		| c==LFChar || c==CRChar
			# pos = {fp_line = fp_line + 1, fp_col = 0}
//			#	(c,stream)	= correctNewline_OldLine c i tabsize line stream
			=	ScanComment {
						inp_filename=inp_filename,inp_tabsize=tabsize,
						inp_stream = stream
					,	inp_pos	= pos
					}
		| c=='/' || c=='*'
			= ScanComment2 c {
								inp_filename=inp_filename,inp_tabsize=tabsize,
								inp_stream = OldLine (i+1) line stream
							,	inp_pos	= {fp_line=fp_line, fp_col = fp_col+1}
							}
			= scan_comment_in_line (i+1) (fp_col+1) fp_line line tabsize stream inp_filename
	= ScanComment {
				inp_filename=inp_filename,inp_tabsize=tabsize,
				inp_stream = stream
			,	inp_pos	= {fp_line=fp_line, fp_col = fp_col}
			}

SkipToEndOfLine	:: !Input -> Input
SkipToEndOfLine input=:{inp_stream=OldLine i line stream,inp_pos={fp_line,fp_col}}
	| i<size line
		= {input & inp_stream=stream,inp_pos={fp_line=fp_line+1,fp_col=0}}
SkipToEndOfLine input
	# (eof, c, input)	= ReadChar input
	| eof				= input
	| c==NewLineChar	= input
			= SkipToEndOfLine input

Scan :: !Char !Input !ScanContext -> (!Token, !Input)
Scan '(' input co			= (OpenToken, input)
Scan ')' input co			= (CloseToken, input)
Scan '{' input CodeContext	= ScanCodeBlock input
//Scan '{' input co			= (CurlyOpenToken, input)
Scan c0=:'{' input co
	# (eof, c1, input)		= ReadNormalChar input
	| eof					= (CurlyOpenToken, input)
	| c1 == '|'				= (GenericOpenToken, input)
							= (CurlyOpenToken, charBack input)
Scan '}' input co			= (CurlyCloseToken, input)
Scan '[' input co			= (SquareOpenToken, input)
Scan ']' input co			= (SquareCloseToken, input)
Scan c0=:'|' input co
	# (eof, c1, input)		= ReadNormalChar input
	| eof					= (BarToken, input)
	| c1 == '}'				= (GenericCloseToken, input) // AA
	| isSpecialChar c1		= ScanOperator 1 input [c1, c0] co
							= (BarToken, charBack input)
Scan ',' input co			= (CommaToken, input)
Scan ';' input co			= (SemicolonToken, input)
Scan '#' input TypeContext	= (HashToken, input)
Scan c0=:'#' input co
	# (strict, input)		= determineStrictness input
	| strict
		= (SeqLetToken strict, input)
	# (eof,c1, input)		= ReadNormalChar input
	| eof
		= (SeqLetToken False, input)
	| isSpecialChar c1
		= ScanOperator 1 input [c1, c0] co
	// otherwise
		= (SeqLetToken strict, charBack input)
Scan '*' input TypeContext	= (AsteriskToken, input)
Scan c0=:'&' input co		= possibleKeyToken AndToken [c0] co input
/*	# (eof, c1, input)		= ReadNormalChar input
	| eof					= (AndToken, input)
	| isSpecialChar c1		= ScanOperator 1 input [c1, c0] co
							= (AndToken, charBack input) */
Scan c0=:'.' input co	// PK incorrect ?
	= case co of
		TypeContext
						-> (DotToken, input)
		_	# (eof, c1, input) = ReadNormalChar input
			| eof		-> (DotToken, input)
			| c1 == '.'
				# (eof, c2, input) = ReadNormalChar input
				| eof		-> (DotDotToken, input)
				| isSpecialChar c2
						-> ScanOperator 2 input [c2, c1, c0] co
						-> (DotDotToken, charBack input)
			| isSpecialChar c1
						-> ScanOperator 1 input [c1, c0] co
						-> (DotToken, charBack input)
Scan '!' input TypeContext	= (ExclamationToken, input)
Scan c0=:'\\' input co
	# (eof, c, input)		= ReadNormalChar input
	| eof					= (BackSlashToken, input)
	| c == '\\'				= possibleKeyToken DoubleBackSlashToken [c, c0] co input
	| isSpecialChar c		= ScanOperator 1 input [c, c0] co
							= (BackSlashToken, charBack input)
Scan c0=:'_' input=:{inp_stream=OldLine i line stream,inp_pos} co
	# size	= size line
	# end_i	= scan_underscores i size line
		with
			scan_underscores :: !Int !Int !{#Char} -> Int
			scan_underscores i size line
				| i<size && line.[i] == '_'
					= scan_underscores (i+1) size line
					= i
	| end_i<size && IsIdentChar line.[end_i] co
		= replaceIdentToken (ScanIdentFast (end_i-i+1) {input & inp_stream=OldLine end_i line stream} co)
		with
			replaceIdentToken :: (Token, *state) -> (Token, *state)
			replaceIdentToken (IdentToken name, s)
				=	(UnderscoreIdentToken name, s)
			replaceIdentToken tokenAndState
				=	tokenAndState
	| end_i==i
		= (WildCardToken, input)
		# pos = {inp_pos & fp_col = inp_pos.fp_col + (end_i-i)}
		# input =  {input & inp_stream=OldLine end_i line stream,inp_pos=pos}
		= (ErrorToken (line % (i-1,end_i-1)+++" is an illegal token"),input)
Scan c0=:'<' input TypeContext
	# (eof, c1, input)		= ReadNormalChar input
	| eof					= (ErrorToken "< just before end of file in TypeContext", input)
	| c1 == '='				= (LessThanOrEqualToken, input)
							=  ScanOperator 0 (charBack input) [c0] TypeContext
Scan c0=:'<' input co
	# (eof, c1, input)		= ReadNormalChar input
	| eof 					= (IdentToken "<", input)
	| c1 <> '-'
		| c1<>'|'
			= ScanOperator 0 (charBack input) [c0] co
		# (eof, c2, input)	= ReadNormalChar input
		| eof
			= (IdentToken "<|",input)
		| c2=='-'
			# (eof, c3, input)	= ReadNormalChar input
			| eof				= (LeftArrowWithBarToken, input)
			| isSpecialChar c3	= ScanOperator 3 input [c3, c2, c1, c0] co
								= (LeftArrowWithBarToken, charBack input)
		| isSpecialChar c2
			= ScanOperator 2 input [c2, c1, c0] co
			= (IdentToken "<|", charBack input)
	# (eof, c2, input)		= ReadNormalChar input
	| eof					= (LeftArrowToken, input)
	| c2 == ':'	
		# (eof, c3, input)		= ReadNormalChar input
		| eof					= (LeftArrowColonToken, input)
		| isSpecialChar c3		= ScanOperator 3 input [c3, c2, c1, c0] co
								= (LeftArrowColonToken, charBack input)
	| isSpecialChar c2		= ScanOperator 2 input [c2, c1, c0] co
							= (LeftArrowToken, charBack input)
Scan c0=:'-' input co
	# (previous_char,input) = GetPreviousChar input;

	# (eof, c1, input)		= ReadNormalChar input
	| eof					= (IdentToken "-", input)
	| IsDigit c1 && new_exp_char previous_char
		= ScanNumeral 1 input [c1,c0]

	| c1 <> '>'				= ScanOperator 0 (charBack input) [c0] co
	| co == TypeContext		= (ArrowToken, input)
	# (eof, c2, input)		= ReadNormalChar input		
	| eof					= (ArrowToken, input)
	| isSpecialChar c2		= ScanOperator 2 input [c2, c1, c0] co
							= (ArrowToken, charBack input)
Scan c0=:'+' input co
	# (previous_char,input) = GetPreviousChar input;

	# (eof, c1, input)		= ReadNormalChar input
	| eof					= (IdentToken "+", input)
	| IsDigit c1 && new_exp_char previous_char
		= ScanNumeral 1 input [c1,c0]
							= ScanOperator 0 (charBack input) [c0] co
Scan c0=:'=' input co
	# (eof, c, input)		= ReadNormalChar input
	| eof					= (EqualToken, input)
	| c == ':'				= possibleKeyToken DefinesColonToken [c, c0] co input
	| c == '>'				= possibleKeyToken DoubleArrowToken [c, c0] co input
	| isSpecialChar c		= ScanOperator 1 input [c, c0] co
							= (EqualToken, charBack input)
Scan c0=:':' input co
	# (eof,c1, input)		= ReadNormalChar input
	| eof					= (ColonToken, input)
	| c1 == ':'
		# (eof, c2, input)	= ReadNormalChar input
		| eof				= (DoubleColonToken, input)
		| isSpecialChar c2	&& ~(c2=='!' || c2=='*' || c2=='.') // for type rules and the like
							= ScanOperator 2 input [c2, c1, c0] co
							= (DoubleColonToken, charBack input)
	| c1 == '='
		# (eof, c2, input)	= ReadNormalChar input
		| eof				= ScanOperator 1 input [c1, c0] co
		| c2 == '='			= (ColonDefinesToken, input)
							= ScanOperator 1 (charBack input) [c1, c0] co
	// c1 <> '='
	| isSpecialChar c1		= ScanOperator 1 input [c1, c0] co
							= (ColonToken, charBack input)
Scan '\'' input co			= ScanChar input
Scan c0=:'\"' input co		= ScanString 0 [c0] input

Scan 'E' input TypeContext
	# (eof,c1,input)		= ReadNormalChar input
	| eof					= (IdentToken "E", input)
	| c1 == '.'				= (ExistsToken, input)
							= ScanIdentFast 1 (charBack input) TypeContext
Scan 'A' input TypeContext
	# (eof,c1,input)		= ReadNormalChar input
	| eof					= (IdentToken "A", input)
	| c1 == '.'				= (ForAllToken, input)
							= ScanIdentFast 1 (charBack input) TypeContext
Scan c    input co
	| IsDigit c				= ScanNumeral 0 input [c]
	| IsIdentChar c	co	
		= ScanIdentFast 1 input co
	| isSpecialChar c		= ScanOperator 0 input [c] co
							= (ErrorToken ScanErrIllegal, input)

possibleKeyToken :: !Token ![Char] !ScanContext !Input -> (!Token, !Input)
possibleKeyToken token reversedPrefix context input
	# (eof, c, input)		= ReadNormalChar input
	| eof					= (token, input)
	| isSpecialChar c		= ScanOperator (length reversedPrefix) input [c : reversedPrefix] context
							= (token, charBack input)

new_exp_char ',' = True
new_exp_char '[' = True
new_exp_char '(' = True
new_exp_char '{' = True
new_exp_char '/' = True // to handle end of comment symbol: */
new_exp_char c	 = isSpace c

ScanIdentFast :: !Int !Input !ScanContext -> (!Token, !Input)
ScanIdentFast n input=:{inp_stream=OldLine i line stream,inp_pos} ModuleNameContext
	# end_i = ScanModuleNameCharsInString i line
		with
			ScanModuleNameCharsInString :: !Int !{#Char} -> Int
			ScanModuleNameCharsInString i line
				| i<size line
					| IsModuleNameChar line.[i]
						= ScanModuleNameCharsInString (i+1) line
						= i
					= i
	# pos = {inp_pos & fp_col = inp_pos.fp_col + (end_i-i)}
	# input =  {input & inp_stream=OldLine end_i line stream,inp_pos=pos}
	= (IdentToken (line % (i-n,end_i-1)), input)
ScanIdentFast n input=:{inp_stream=OldLine i line stream,inp_pos} co
	# end_i = ScanIdentCharsInString i line co
		with
			ScanIdentCharsInString :: !Int !{#Char} !ScanContext -> Int
			ScanIdentCharsInString i line co
				| i<size line
					| IsIdentChar line.[i] co
						= ScanIdentCharsInString (i+1) line co
						= i
					= i
	# pos = {inp_pos & fp_col = inp_pos.fp_col + (end_i-i)}
	# input =  {input & inp_stream=OldLine end_i line stream,inp_pos=pos}
	= CheckReservedIdent co (line % (i-n,end_i-1)) input

ScanOperator :: !Int !Input ![Char] !ScanContext -> (!Token, !Input)
ScanOperator n input token co
	#  (eof, c, input)		= ReadNormalChar input
	| eof					= CheckReservedOperator (revCharListToString n token) input
	| isSpecialChar c		= ScanOperator (n + 1) input [c:token] co
							= CheckReservedOperator (revCharListToString n token) (charBack input)

CheckReservedIdent :: !ScanContext !String !Input -> (!Token, !Input)
CheckReservedIdent GeneralContext   s i = CheckGeneralContext s i
CheckReservedIdent TypeContext      s i = CheckTypeContext s i
CheckReservedIdent FunctionContext	s i = CheckFunctContext s i
CheckReservedIdent CodeContext		s i = CheckCodeContext s i
CheckReservedIdent GenericContext	s i = CheckGenericContext s i
// not called with ModuleNameContext

CheckReservedOperator :: !String !Input -> (!Token, !Input)
CheckReservedOperator "!"  input =	(ExclamationToken, input)
CheckReservedOperator "*/" input =	(ErrorToken "Unexpected end of comment, */", input)
CheckReservedOperator s    input =	(IdentToken s, input)

CheckGeneralContext :: !String !Input -> (!Token, !Input)
CheckGeneralContext s input
 = case s of
	"module"     		-> (ModuleToken		, input)
	"definition"  		-> (DefModuleToken	, input)
	"implementation"	-> (ImpModuleToken	, input)
	"system"			-> (SysModuleToken	, input)
	"from" 				-> (FromToken		, input)
	"in"  	    		-> (InToken			, input)
	s					-> CheckEveryContext s input

CheckEveryContext :: !String !Input -> (!Token, !Input)
CheckEveryContext s input
 = case s of
	"where"		->	(WhereToken			, input)
	"with"		->	(WithToken			, input)
	"class" 	->	(ClassToken			, input)
	"instance"	->	(InstanceToken		, input)
	"generic" 	->	(GenericToken		, input)
	"derive"	-> 	(DeriveToken		, input)
	"otherwise"	->	(OtherwiseToken		, input)
	"infixr"	#	(error, n, input) = GetPrio  input
				->	case error of
						Yes err -> (ErrorToken err						, input)  //-->> ("Error token generated: "+err)
						No		-> (PriorityToken (Prio RightAssoc n)	, input)
	"infixl"	#	(error, n, input) = GetPrio  input
				->	case error of
						Yes err -> (ErrorToken err						, input)  //-->> ("Error token generated: "+err)
						No		-> (PriorityToken (Prio LeftAssoc n)	, input)
	"infix"		#	(error, n, input) = GetPrio  input
				->	case error of
						Yes err -> (ErrorToken err						, input)  //-->> ("Error token generated: "+err)
						No		-> (PriorityToken (Prio NoAssoc n)		, input)
	"import" -> (ImportToken,input)
	"foreign" -> (ForeignToken,input)
   	s			->	(IdentToken s		, input)

CheckTypeContext :: !String !Input -> (!Token, !Input)
CheckTypeContext s input
 = case s of
 	"Int"		->	(IntTypeToken		, input)
	"Char"		->	(CharTypeToken		, input)
	"Real"		->	(RealTypeToken		, input)
	"Bool"		->	(BoolTypeToken		, input)
	"String"	->	(StringTypeToken	, input)
	"File"		->	(FileTypeToken		, input)
	"World"		->	(WorldTypeToken		, input)
	"Dynamic"	->	(DynamicTypeToken	, input)
	"special"	->	(SpecialToken		, input)
	"from" 		->	(FromToken			, input)
	"of"		->  (OfToken			, input) // AA
	s			->	CheckEveryContext s input

CheckFunctContext :: !String !Input -> (!Token, !Input)
CheckFunctContext s input
 = case s of
	"if"		->	(IfToken			, input)
	"True"		->	(BoolToken True		, input)
	"False"		->	(BoolToken False	, input)
	"case"		->	(CaseToken			, input)
	"of"		->	(OfToken			, input)
	"system"	->	(SysModuleToken		, input)
	"from"		->	(FromToken			, input)
	"let" 	    #	(strict, input) = determineStrictness input
				->	(LetToken strict, input)
//	"Let" 	    #	(strict, input) = determineStrictness input
//				->	(SeqLetToken strict	, input)
	"in"  	    ->	(InToken			, input)
	"dynamic"  	->	(DynamicToken		, input)
	"code"		->	(CodeToken			, input)
	s			->	CheckEveryContext s input

CheckCodeContext :: !String !Input -> (!Token, !Input)
CheckCodeContext s input
 = case s of
	"inline"	->	(InlineToken		, input)
	s			->	CheckEveryContext s input	

CheckGenericContext :: !String !Input -> (!Token, !Input)
CheckGenericContext s input
 = case s of
	"of"		->	(GenericOfToken,	input)
	"with"		->	(GenericWithToken,	input)
	s			->	CheckEveryContext s input	

GetPrio :: !Input -> (!Optional String, !Int, !Input)
GetPrio input
	# (error, c, input) = SkipWhites input
	| IsDigit c
		= (error, digitToInt c, input)
		= (error, defaultPrio , charBack input)
where defaultPrio = 9

determineStrictness :: !Input -> (!Bool, !Input)
determineStrictness input
	# (eof, c, input)		= ReadNormalChar input
	| eof					= (False, input)
	| c == '!'				= (True, input)
							= (False, charBack input)

ScanCodeBlock :: !Input -> (!Token, !Input)
ScanCodeBlock input
	= scan_code_block [] input
where
	scan_code_block :: ![String] !Input -> (!Token,!Input)
	scan_code_block acc input
		# (eof, c, input)	= ReadChar input
		| c == '}'
			= (CodeBlockToken (reverse acc), input)
		| isNewLine c
			| eof
				= (ErrorToken "eof in code block", input)
				= scan_code_block acc input
		| IsWhiteSpace c
				= scan_code_block acc input
		# (line, input)		= ReadLine input
		= scan_code_block [toString c+stripNewline line:acc] input

stripNewline :: !String -> String
stripNewline string
	# size = size string
	= case size of
		0 -> string
		1 | isNewLine string.[0]
			-> ""
			-> string
		_ | isNewLine string.[size-1]
			| isNewLine string.[size-2]
				-> string%(0,size-3)
				-> string%(0,size-2)
			-> string

ScanNumeral	:: !Int !Input [Char] -> (!Token, !Input)
ScanNumeral n input chars=:['0':r]
	| isEmpty r || r == ['+']
		# (eof, c, input)		= ReadNormalChar input
		| eof					= (IntToken (revCharListToString n chars), input)
		| c == 'x'
			# (eof, c1, input)	= ReadNormalChar input
			| eof				= (IntToken "0", charBack input)
			| isHexDigit c1		= ScanHexNumeral (hexDigitToInt c1) input
								= (IntToken "0", charBack (charBack input))
		| isOctDigit c			= ScanOctNumeral (digitToInt c) input
		| c == '.'				= TestFraction n input chars
								= (IntToken "0", charBack input)
	| r == ['-']
		# (eof, c, input)		= ReadNormalChar input
		| eof					= (IntToken (revCharListToString n chars), input)
		| c == 'x'
			# (eof, c1, input)	= ReadNormalChar input
			| eof				= (IntToken "0", charBack input)
			| isHexDigit c1		= ScanHexNumeral (~ (hexDigitToInt c1)) input
								= (IntToken "0", charBack (charBack input))
		| isOctDigit c			= ScanOctNumeral (~ (digitToInt c)) input
		| c == '.'				= TestFraction n input chars
								= (IntToken "0", charBack input)
ScanNumeral n input chars
	# (eof, c, input)		= ReadNormalChar input
	| eof					= (IntToken (revCharListToString n chars), input)
	| IsDigit c				= ScanNumeral (n + 1) input [c:chars]
	| c == 'E'				= ScanExponentSign (n + 1) input [c:chars]
	| c == '.'				= TestFraction n input chars
							#! s = revCharListToString n chars
							= (IntToken s, charBack input)

TestFraction :: !Int !Input ![Char] -> (!Token, !Input)
TestFraction n input chars
	# (eof, c, input)		= ReadNormalChar input
	| eof					= (ErrorToken ("Incorrect Real at end of file: "+(revCharListToString (n+1) ['.':chars])), input)
	| IsDigit c				= ScanFraction (n + 2) input [c,'.':chars]
			 				= (IntToken (revCharListToString n chars), charBack (charBack input))

ScanFraction :: !Int !Input ![Char] -> (!Token, !Input)
ScanFraction n input chars
	# (eof, c, input)		= ReadNormalChar input
	| eof					= (RealToken (revCharListToString n chars), input)
	| c == 'E'				= case chars of
								[c:_] | IsDigit c	-> ScanExponentSign (n + 1) input ['E':chars]
								_					-> ScanExponentSign (n + 2) input ['E','0':chars]
	| IsDigit c				= ScanFraction (n + 1) input [c:chars]
							= case chars of
								[c:_] | IsDigit c	-> (RealToken (revCharListToString n chars), charBack input)
								_					-> (RealToken (revCharListToString (n+1) ['0':chars]), charBack input)

ScanExponentSign :: !Int !Input ![Char] -> (!Token, !Input)
ScanExponentSign n input chars
	# (eof, c, input)		= ReadNormalChar input
	| eof					= (RealToken (revCharListToString n chars), input)
	| c == '+'				= ScanExponent n input chars
	| c == '-' || IsDigit c	= ScanExponent (n+1) input [c:chars]
	| otherwise				= (ErrorToken ("Digit or sign expected after "+revCharListToString n chars), charBack input)

ScanExponent :: !Int !Input ![Char] -> (!Token, !Input)
ScanExponent n input chars
	# (eof, c, input)		= ReadNormalChar input
	| eof					= (RealToken (revCharListToString n chars), input)
	| IsDigit c				= ScanExponent (n + 1) input [c:chars]
							= case chars of
								[c:_] | IsDigit c	-> (RealToken (revCharListToString n chars), charBack input)
								_					-> (ErrorToken ("Digit expected after "+revCharListToString n chars), charBack input)

ScanHexNumeral	:: !Int !Input -> (!Token, !Input)
ScanHexNumeral n input
	# (eof, c, input)		= ReadNormalChar input
	| eof					= (IntToken (toString n), input)
	| isHexDigit c			= ScanHexNumeral (n*16+hexDigitToInt c) input
							= (IntToken (toString n), charBack input)

ScanOctNumeral	:: !Int !Input -> (!Token, !Input)
ScanOctNumeral n input
	# (eof, c, input)		= ReadNormalChar input
	| eof					= (IntToken (toString n), input)
	| isOctDigit c			= ScanOctNumeral (n*8+digitToInt c) input
							= (IntToken (toString n), charBack input)

ScanChar :: !Input -> (!Token, !Input)
ScanChar input
	# (eof, c, input)		= ReadChar input // PK: was ReadNormalChar input
	| eof					= (ErrorToken "End of file inside Char denotation", input)
	| c=='\''				= (CharListToken "", input)
	| c=='\\'				= ScanBSChar 0 ['\''] input ScanEndOfChar
							= ScanEndOfNoBSChar c input

ScanBSChar :: !Int ![Char] !Input (Int [Char] Input -> (!Token, !Input)) -> (!Token, !Input)
ScanBSChar n chars input cont
	# (eof, c, input)		= ReadNormalChar input
	| eof					= cont n chars input
	= case c of
		'n'		->	cont (n+2) ['n','\\':chars] input		// (['n','\\':chars], n + 2, input)
		'r'		->	cont (n+2) ['r','\\':chars] input		// (['r','\\':chars], n + 2, input)
		'f'		->	cont (n+2) ['f','\\':chars] input		// (['f','\\':chars], n + 2, input)
		'b'		->	to_chars '\b' input
		't'		->	cont (n+2) ['t','\\':chars] input		// (['t','\\':chars], n + 2, input)
		'v'		->	to_chars '\v' input
		'\\'	->	cont (n+2) ['\\','\\':chars] input		// (['\\','\\':chars], n + 2, input)
		'"'		->	cont (n+2) ['"','\\':chars] input		// (['"' ,'\\':chars], n + 2, input)
		'\''	->	cont (n+2) ['\'','\\':chars] input		// (['\'','\\':chars], n + 2, input)
		'x'		->	ScanNumChar Hex isHexDigit 2 0 input	// max 2 characters
		'X'		->	ScanNumChar Hex isHexDigit 2 0 input	// max 2 characters
		'd'		->	ScanNumChar Dec isDigit 3 0 input		// max 3 characters
		'D'		->	ScanNumChar Dec isDigit 3 0 input		// max 3 characters
		'0'		->	ScanNumChar Oct IsOct 3 0 input			// max 3 characters
		c | IsOct c
				->	ScanNumChar Oct IsOct 2 (digitToInt c) input // max 2 more characters, 3 including current
				->	cont (n+1) [c:chars] input
where
	ScanNumChar base valid 0 acc input
		= to_chars acc input
	ScanNumChar base valid n acc input
		# (eof, c, input)		= ReadNormalChar input
		| eof					= to_chars acc input
		| valid c				= ScanNumChar base valid (n-1) (base*acc+hexDigitToInt c) input
								= to_chars acc (charBack input)
	Hex = 16
	Oct = 8
	Dec = 10
	
	to_chars cc input
	| toInt cc > 255
	 = (ErrorToken "invalid char, value > 255", input)
	 = case toChar cc of
		'\n'	->	cont (n+2) ['n','\\':chars] input	// (['n','\\':chars], n + 2, input)
		'\r'	->	cont (n+2) ['r','\\':chars] input	// (['r','\\':chars], n + 2, input)
		'\f'	->	cont (n+2) ['f','\\':chars] input	// (['f','\\':chars], n + 2, input)
		'\t'	->	cont (n+2) ['t','\\':chars] input	// (['t','\\':chars], n + 2, input)
		'\\'	->	cont (n+2) ['\\','\\':chars] input	// (['\\','\\':chars], n + 2, input)
		'"'		->	cont (n+2) ['"','\\':chars] input	// (['"' ,'\\':chars], n + 2, input)
		'\''	->	cont (n+2) ['\'','\\':chars] input	// (['\'','\\':chars], n + 2, input)
		// '\b' and '\v' not accepted in abc
		// escape non-printable characters
		c		| not (IsPrint c)
					-> cont (n+4) more_chars input
					with
						more_chars =
							[	toChar (48 +  (toInt c       bitand 7))
							,	toChar (48 + ((toInt c >> 3) bitand 7))
							,	toChar (48 + ((toInt c >> 6) bitand 7))
							,	'\\'
							:	chars
							]
	 	c		->	cont (n+1) [c:chars] input

ScanEndOfChar :: !Int ![Char] !Input -> (!Token, !Input)
ScanEndOfChar n chars input
	# (eof, c, input)		= ReadChar input
	| eof					= (ErrorToken "End of file inside char denotation", input)
	| '\'' == c				= (CharToken (revCharListToString (n + 1) [c:chars]), input)
	| '\\' == c				= ScanBSChar n chars input ScanCharList
							= ScanCharList (n+1) [c:chars] input

ScanEndOfNoBSChar :: !Char !Input -> (!Token, !Input)
ScanEndOfNoBSChar c1 input
	# (eof, c, input)		= ReadChar input
	| eof					= (ErrorToken "End of file inside char denotation", input)
	| c=='\''
							| is_ident_char c1
								= qualified_ident_or_CharToken c1 input							
								= (CharToken {'\'',c1,'\''}, input)
	| c=='\\'				= ScanBSChar 1 [c1,'\''] input ScanCharList
							= scan_CharList_or_qualified_ident c1 c input
where
	qualified_ident_or_CharToken :: !Char !Input -> (!Token,!Input)
	qualified_ident_or_CharToken c1 input=:{inp_stream=OldLine i line stream,inp_pos}
		| i+1<size line && line.[i]=='.'
			# c=line.[i+1]
			| is_ident_char c
				# end_i = scan_ident_chars_in_string (i+2) line
				  ident_name = line % (i+1,end_i-1)
				  pos = {inp_pos & fp_col = inp_pos.fp_col + (end_i-i)}
				  input =  {input & inp_stream=OldLine end_i line stream,inp_pos=pos}
				= (QualifiedIdentToken {c1} ident_name,input)
			| c==' ' && i+2<size line && isSpecialChar line.[i+2]
				# end_i = scan_special_chars_in_string (i+3) line
				  ident_name = line % (i+1,end_i-1)
				  pos = {inp_pos & fp_col = inp_pos.fp_col + (end_i-i)}
				  input =  {input & inp_stream=OldLine end_i line stream,inp_pos=pos}
				= (QualifiedIdentToken {c1} ident_name,input)
				= scan_char c1 input
			= scan_char c1 input
	qualified_ident_or_CharToken c1 input
		= scan_char c1 input

	scan_CharList_or_qualified_ident :: !Char !Char !Input -> (!Token, !Input)
	scan_CharList_or_qualified_ident c1 c2 input=:{inp_stream=OldLine i line _}
		| i>=3 && is_ident_char c1
			| is_ident_char c2
				# after_module_name_i = scan_rest_of_module_name_in_string i line
				| after_module_name_i+2<size line && line.[after_module_name_i]=='\'' && line.[after_module_name_i+1]=='.'
					# c=line.[after_module_name_i+2]
					| is_ident_char c
						= qualified_ident after_module_name_i input
					| c==' ' && after_module_name_i+3<size line && isSpecialChar line.[after_module_name_i+3]
						= qualified_special_ident after_module_name_i input
						= scan_char_list c2 c1 input
					= scan_char_list c2 c1 input
			| c2=='.' && i<size line && is_ident_char line.[i]
				# after_module_name_i = scan_rest_of_module_name_in_string (i+1) line
				| after_module_name_i+2<size line && line.[after_module_name_i]=='\'' && line.[after_module_name_i+1]=='.'
					# c=line.[after_module_name_i+2]
					| is_ident_char c
						= qualified_ident after_module_name_i input
					| c==' ' && after_module_name_i+3<size line && isSpecialChar line.[after_module_name_i+3]
						= qualified_special_ident after_module_name_i input
						= scan_char_list c2 c1 input
					= scan_char_list c2 c1 input
				= scan_char_list c2 c1 input
			= scan_char_list c2 c1 input
	where
		scan_rest_of_module_name_in_string :: !Int !{#Char} -> Int
		scan_rest_of_module_name_in_string i s
			| i<size s
				# c=s.[i]
				| is_ident_char c
					= scan_rest_of_module_name_in_string (i+1) s
				| c=='.' && i+1<size s && is_ident_char s.[i+1]
					= scan_rest_of_module_name_in_string (i+2) s
					= i
				= i

		qualified_ident :: !Int !Input -> (!Token,!Input)
		qualified_ident after_module_name_i input=:{inp_stream=OldLine i line stream,inp_pos}
			# module_name = line % (i-2,after_module_name_i-1)
			  after_ident_i = scan_ident_chars_in_string (after_module_name_i+3) line
			  ident_name = line % (after_module_name_i+2,after_ident_i-1)
			  pos = {inp_pos & fp_col = inp_pos.fp_col + (after_ident_i-i)}
			  input =  {input & inp_stream=OldLine after_ident_i line stream,inp_pos=pos}
			= (QualifiedIdentToken module_name ident_name,input)

		qualified_special_ident :: !Int !Input -> (!Token,!Input)
		qualified_special_ident after_module_name_i input=:{inp_stream=OldLine i line stream,inp_pos}
			# module_name = line % (i-2,after_module_name_i-1)
			  after_ident_i = scan_special_chars_in_string (after_module_name_i+4) line
			  ident_name = line % (after_module_name_i+3,after_ident_i-1)
			  pos = {inp_pos & fp_col = inp_pos.fp_col + (after_ident_i-i)}
			  input =  {input & inp_stream=OldLine after_ident_i line stream,inp_pos=pos}
			= (QualifiedIdentToken module_name ident_name,input)

		scan_char_list :: !Char !Char !Input -> (!Token, !Input)
		scan_char_list c2 c1 input
			= ScanCharList 2 [c2,c1,'\''] input
	scan_CharList_or_qualified_ident c1 c2 input
		= ScanCharList 2 [c2,c1,'\''] input

	scan_ident_chars_in_string :: !Int !{#Char} -> Int
	scan_ident_chars_in_string i line
		| i<size line && is_ident_char line.[i]
			= scan_ident_chars_in_string (i+1) line
			= i

	scan_special_chars_in_string :: !Int !{#Char} -> Int
	scan_special_chars_in_string i line
		| i<size line && isSpecialChar line.[i]
			= scan_special_chars_in_string (i+1) line
			= i

	scan_char :: !Char !Input -> (!Token, !Input)
	scan_char c input
		= (CharToken {'\'',c,'\''}, input)

	is_ident_char c
		= isAlphanum c || c=='_' || c=='`'

ScanCharList :: !Int ![Char] !Input -> (!Token, !Input)
ScanCharList n chars input
	# (eof, c, input)		= ReadChar input
	| eof					= (ErrorToken "End of file inside char list denotation", input)
	= case c of
		'\''		#	charList	= revCharListToString n chars % (1,n) // without '\''
					->	(CharListToken charList, input)
		'\\'		->	ScanBSChar n chars input ScanCharList
		NewLineChar	->	(ErrorToken "newline in char list", input)
		_			->	ScanCharList (n+1) [c:chars] input

ScanString :: !Int ![Char] !Input -> (!Token, !Input)
ScanString n chars input
 # (eof, c, input)		= ReadChar input
 | eof					= (ErrorToken "End of file inside String denotation", input)
 = case c of 
	'\\' 		->	ScanBSChar n chars input ScanString
	'\"' 		-> (StringToken (revCharListToString (n + 1) [c:chars]), input)
	NewLineChar -> (ErrorToken ScanErrNLString, input)
	_	 		-> ScanString (n + 1) [c:chars] input

/*
	some predicates on tokens
*/

isLhsStartToken :: ! Token -> Bool
isLhsStartToken OpenToken       = True
isLhsStartToken SquareOpenToken	= True
isLhsStartToken CurlyOpenToken	= True
isLhsStartToken (IdentToken id) = True
isLhsStartToken token           = False

isOffsideToken :: ! Token -> Bool
isOffsideToken NewDefinitionToken	= True
isOffsideToken EndGroupToken		= True
isOffsideToken EndOfFileToken		= True
isOffsideToken token				= False

isEndGroupToken :: ! Token -> Bool
isEndGroupToken EndGroupToken = True
isEndGroupToken CurlyCloseToken = True
isEndGroupToken token = False
/*
	character functions
*/

//IsWhiteSpace	:: Char	-> Bool
IsWhiteSpace c :== isSpace c

//IsDigit	:: Char	-> Bool
IsDigit c :== isDigit c

IsOct c :== '0' <= c && c <= '7'

// IsPrint assumes all 8 bit characters (>127) are not printable
IsPrint c
	:== c >= ' ' && c <= '~'

hexDigitToInt :: !Char -> Int
hexDigitToInt 'a' = 10
hexDigitToInt 'A' = 10
hexDigitToInt 'b' = 11
hexDigitToInt 'B' = 11
hexDigitToInt 'c' = 12
hexDigitToInt 'C' = 12
hexDigitToInt 'd' = 13
hexDigitToInt 'D' = 13
hexDigitToInt 'e' = 14
hexDigitToInt 'E' = 14
hexDigitToInt 'f' = 15
hexDigitToInt 'F' = 15
hexDigitToInt c   = digitToInt c

IsIdentChar :: !Char !ScanContext -> Bool
IsIdentChar  c	_ | isAlphanum c	= True
IsIdentChar '_'	_ 					= True
IsIdentChar '`'	_					= True
IsIdentChar '^'	TypeContext			= True
IsIdentChar  _	_					= False

IsModuleNameChar :: !Char -> Bool
IsModuleNameChar  c	| isAlphanum c	= True
IsModuleNameChar '_'				= True
IsModuleNameChar '`'				= True
IsModuleNameChar '.'				= True
IsModuleNameChar _					= False

string_to_list ::!{#Char} -> .[Char]
string_to_list s = stolacc s (size s - 1) []
where
	stolacc :: !String !Int u:[Char] -> u:[Char]
	stolacc s i acc 
		| i >= 0
			#! si=s.[i];
			= stolacc s (dec i) [si : acc] 
			= acc

/*
	Input functions
*/

EndOfInput :: !Input -> (!Bool, !Input)
EndOfInput input=:{inp_stream = InFile file}
	# (endoffile, file) = fend file
	= (endoffile, { input & inp_stream = InFile file })
EndOfInput input						= (False, input)

ReadNormalChar :: !*Input -> (!Bool, !Char, !Input)
ReadNormalChar {inp_stream = OldLine i line stream,inp_pos,inp_tabsize,inp_filename}
	| i<size line
		# c=line.[i]
		| c==LFChar || c==CRChar || c=='\t'
			=	(	True,	NewLineChar
				,	{
						inp_filename=inp_filename,inp_tabsize=inp_tabsize,
						inp_stream = OldLine i line stream,
						inp_pos	= inp_pos
					}
				)
			#	pos = {inp_pos & fp_col = inp_pos.fp_col + 1}
			=	(	False, c
				,	{
						inp_filename=inp_filename,inp_tabsize=inp_tabsize,
						inp_stream = OldLine (i+1) line stream,
						inp_pos	= pos
					}
				)
		= ReadNormalChar {inp_filename=inp_filename,inp_tabsize=inp_tabsize,inp_pos=inp_pos,inp_stream = stream}
ReadNormalChar {inp_stream = InFile file, inp_pos, inp_tabsize, inp_filename}
	#!	(s, file) = freadline file
	| size s==0
		#	c	= NewLineChar
//			pos	= NextPos c inp_pos inp_tabsize
		=	(	True
			,	c
			,	{
//					input &
					inp_tabsize=inp_tabsize,inp_filename=inp_filename,
					inp_stream = InFile file
				,	inp_pos	= inp_pos
				}
			) 
		= ReadNormalChar {
				inp_tabsize=inp_tabsize,inp_filename=inp_filename,inp_pos=inp_pos,
				inp_stream = OldLine 0 s (InFile file)
				}

ReadChar :: !*Input -> (!Bool, !Char, !Input) // Bool indicates end of file, we read always newlines in an empty file
ReadChar {inp_stream = OldLine i line stream,inp_pos,inp_tabsize,inp_filename}
	| i<size line
		# c=line.[i]
		| c==LFChar || c==CRChar || c=='\t'
			#	pos = NextPos c inp_pos inp_tabsize
				(c,stream)	= correctNewline_OldLine c i inp_tabsize line stream
			=	(	False,	c
				,	{	inp_filename = inp_filename, inp_tabsize = inp_tabsize
					,	inp_stream = stream
					,	inp_pos	= pos
					}
				)
			#	pos = {inp_pos & fp_col = inp_pos.fp_col + 1}
			=	(	False, c
				,	{	inp_filename = inp_filename, inp_tabsize = inp_tabsize
					,	inp_stream = OldLine (i+1) line stream
					,	inp_pos	= pos
					}
				)
		= ReadChar {inp_filename=inp_filename,inp_tabsize=inp_tabsize,inp_pos=inp_pos,
								inp_stream = stream}
//ReadChar input=:{inp_stream = InFile file, inp_pos, inp_tabsize}
ReadChar {inp_stream = InFile file, inp_pos, inp_tabsize, inp_filename}
	#!	(s, file) = freadline file
	| size s==0
		#	c	= NewLineChar
			pos	= NextPos c inp_pos inp_tabsize
		=	(	True
			,	c
			,	{
//					input &
					inp_tabsize=inp_tabsize,inp_filename=inp_filename,
					inp_stream = InFile file
				,	inp_pos	= pos
				}
			) 
		= ReadChar {
				inp_tabsize=inp_tabsize,inp_filename=inp_filename,inp_pos=inp_pos,
				inp_stream = OldLine 0 s (InFile file)
				}

ReadLine :: !Input -> (!String, !Input)
ReadLine input=:{inp_stream = OldLine i line oldfile, inp_pos}
	# input			= {input & inp_stream = oldfile, inp_pos = NextPos CRChar inp_pos 0}
	| i<size line
		| i==0
			= (line, input)
			= (line % (i,size line-1),input)
		= ReadLine input
ReadLine input=:{inp_stream = InFile infile,inp_pos}
	# (eof, file) 	= fend infile
	| eof			= ("", {input & inp_stream = InFile file})
	# (l, file )	= freadline file
	= (l,  {input & inp_stream = InFile file, inp_pos = NextPos CRChar inp_pos 0})
ReadLine input		= ("", input)

NextPos :: !Char !FilePosition !Int -> FilePosition
NextPos c pos=:{fp_line, fp_col} t
	= case c of
		LFChar	-> NextPos CRChar pos t		//	-->> "LF in Nextpos"
		CRChar	-> {fp_line = fp_line + 1, fp_col = 0}	// -->> ("line " +toString (fp_line + 1))
		'\t'	-> {pos & fp_col = t * (fp_col / t + 1)}
		_		-> {pos & fp_col = fp_col + 1}

correctNewline_OldLine :: !Char !Int !Int !{#Char} ! InputStream -> (!Char, !InputStream)
correctNewline_OldLine c i tab_size line input
	=	case c of
			LFChar
				-> (NewLineChar,OldLine (i+1) line input)	//-->> "UNIX newline"
			CRChar
				| i+1<size line && line.[i+1]==LFChar
					-> (NewLineChar,OldLine (i+2) line input) // -->> "DOS newline corrected"
				    -> (NewLineChar,OldLine (i+1) line input)
			_		-> (c,OldLine (i+1) line input)

charBack :: !Input -> Input
charBack {inp_stream=OldLine i line stream,inp_pos,inp_tabsize,inp_filename}
	=	{	
			inp_stream		= OldLine (i-1) line stream,
			inp_pos = {inp_pos & fp_col = inp_pos.fp_col - 1},
			inp_tabsize=inp_tabsize,inp_filename=inp_filename
		}

GetPreviousChar :: !Input -> (!Char,!Input)
GetPreviousChar input=:{inp_stream=OldLine i line stream}
	| i<=1
		= (NewLineChar,input)
		= (line.[i-2],input)
GetPreviousChar input
	= (NewLineChar,input)

qw s :== "\"" + s + "\""

instance <<< Token
where
	(<<<) f t = f <<< (toString t)

instance <<< LongToken
where
	(<<<) f lt = f <<< lt.lt_token <<< " from " <<< lt.lt_position

instance <<< FilePosition
where
	(<<<) f {fp_line,fp_col} = f <<< fp_line <<< ";" <<< fp_col

instance toString Token
where
	toString (IdentToken id)			= id // qw id
	toString (UnderscoreIdentToken id)	= id // qw id
	toString (IntToken id)				= id
	toString (RealToken id)				= id
	toString (StringToken id)			= id
	toString (CharToken id)				= id
	toString (CharListToken id)			= "['"+id+"']"
	toString (BoolToken id)				= toString id
	toString OpenToken					= "("
	toString CloseToken					= ")"
	toString CurlyOpenToken				= "{"
	toString CurlyCloseToken			= "}"
	toString SquareOpenToken			= "["
	toString SquareCloseToken			= "]"

	toString DotToken					= "."
	toString SemicolonToken				= ";"
	toString ColonToken					= ": (ColonToken)"
	toString DoubleColonToken			= "::"
	toString CommaToken					= ","
	toString ExclamationToken			= "!"
	toString BarToken					= "|"
	toString ArrowToken					= "->"
	toString DoubleArrowToken			= "=>"
	toString EqualToken					= "="
	toString DefinesColonToken			= "=:"
	toString ColonDefinesToken			= ":=="
	toString WildCardToken				= "_"
	toString BackSlashToken				= "\\"
	toString DoubleBackSlashToken		= "\\\\"
	toString LeftArrowToken				= "<-"
	toString LeftArrowColonToken		= "<-:"
	toString LeftArrowWithBarToken		= "<|-"
	toString DotDotToken				= ".."
	toString AndToken					= "&"
	toString HashToken					= "#"
	toString AsteriskToken				= "*"
	toString LessThanOrEqualToken		= "<="

	toString ModuleToken				= "module"
	toString ImpModuleToken				= "implementation"
	toString DefModuleToken				= "definition"
	toString SysModuleToken				= "system"	

	toString ImportToken				= "import"
	toString FromToken					= "from"
	toString SpecialToken				= "special"
	toString ForeignToken				= "foreign"

	toString IntTypeToken				= "Int"
	toString CharTypeToken				= "Char"
	toString RealTypeToken				= "Real"
	toString BoolTypeToken				= "Bool"
	toString StringTypeToken			= "String"
	toString FileTypeToken				= "File"
	toString WorldTypeToken				= "World"
	toString ClassToken					= "class"
	toString InstanceToken				= "instance"
	toString OtherwiseToken				= "otherwise"

	toString IfToken					= "if"
	toString WhereToken					= "where"
	toString WithToken					= "with"
	toString CaseToken					= "case"
	toString OfToken					= "of"
	toString (LetToken strict)			= if strict "let!" "let"
	toString (SeqLetToken strict)		= if strict "#!" "#"
	toString InToken					= "in"

	toString DynamicToken				= "dynamic"
	toString DynamicTypeToken			= "Dynamic"

	toString (PriorityToken priority)	= toString priority

	toString CodeToken					= "code"
	toString InlineToken				= "inline"
	toString (CodeBlockToken the_code)	= "<code block>"

	toString NewDefinitionToken			= "offside token (new def)"
	toString EndGroupToken				= "offside token (end group)"
	toString EndOfFileToken				= "end of file"
	toString (ErrorToken id)			= "Scanner error: " + id

	toString (QualifiedIdentToken module_name ident_name) = module_name+++"@"+++ident_name

	toString GenericToken				= "generic"	
	toString DeriveToken				= "derive"	
	toString GenericOpenToken			= "{|"
	toString GenericCloseToken			= "|}"	
	toString GenericOfToken				= "of"	

	toString ExistsToken				= "E."
	toString ForAllToken				= "A."

	toString token						= "toString (Token) does not know this token"

instance == Token
where
	(==) token1 token2
		= equal_constructor token1 token2 && equal_args_of_tokens token1 token2
	where
		equal_args_of_tokens :: !Token !Token -> Bool
		equal_args_of_tokens (IdentToken id1)		(IdentToken id2)		= id1 == id2
		equal_args_of_tokens (RealToken real1) 		(RealToken real2)		= real1 == real2
		equal_args_of_tokens (StringToken string1)	(StringToken string2)	= string1 == string2
		equal_args_of_tokens (CharToken char1)		(CharToken char2)		= char1 == char2
		equal_args_of_tokens (CharListToken chars1)	(CharListToken chars2)	= chars1 == chars2
		equal_args_of_tokens (BoolToken bool1)		(BoolToken bool2)		= bool1 == bool2
		equal_args_of_tokens (IntToken int1)		(IntToken int2)			= int1 == int2
		equal_args_of_tokens (LetToken l1)			(LetToken l2)			= l1 == l2
		equal_args_of_tokens (SeqLetToken l1)		(SeqLetToken l2)		= l1 == l2
		equal_args_of_tokens (ErrorToken id1)		(ErrorToken id2)		= id1 == id2
		equal_args_of_tokens (QualifiedIdentToken module_name1 ident_name1) (QualifiedIdentToken module_name2 ident_name2)
			= ident_name1==ident_name2 && module_name1==module_name2
		equal_args_of_tokens _						_						= True

/* Sjaak ... */

/*
instance < Priority
where
	(<) (Prio assoc1 prio1) (Prio assoc2 prio2)
		= prio1 < prio2 || prio1 == prio2 && assoc1 < assoc2
	(<) _ _ = abort "< of these Priorities (NoPrio) is undefined"
			
instance < Assoc
where
	(<) _ LeftAssoc = True
	(<) LeftAssoc _ = False
	(<) _ _ = True

*/
DefaultPriority :: Priority
DefaultPriority
	=	Prio LeftAssoc 9

determinePriority :: !Priority !Priority -> Optional Bool
determinePriority (Prio assoc_left prio_left) (Prio assoc_right prio_right)
	| prio_left == prio_right
		= has_priority_over assoc_left assoc_right
		= Yes (prio_left > prio_right)
where
	has_priority_over LeftAssoc 	LeftAssoc 	= Yes True
	has_priority_over RightAssoc	RightAssoc 	= Yes False
	has_priority_over _				_			= No

/* Sjaak ... */

			
instance toString Priority
where
	toString (Prio assoc prio) = toString assoc + toString prio
	toString NoPrio = "infix"
	
instance toString Assoc
where
	toString LeftAssoc 	= "infixl "
	toString RightAssoc = "infixr "
	toString NoAssoc	= "infix "

openScanner :: !*File !String !String -> ScanState
openScanner file file_name file_name_extension
	= ScanState {	ss_input 		= Input
										{ inp_stream		= InFile file
							 			, inp_filename		= file_name +++ file_name_extension
							 			, inp_pos			= {fp_line = 1, fp_col = 0}
										, inp_tabsize		= 4
										}
				,	ss_offsides		=	[(1,False)] // to generate offsides between global definitions
				,	ss_scanOptions	=	0
				,	ss_tokenBuffer	=	Buffer0
				}

fopenInSearchPaths :: !{#Char} !{#Char} !SearchPaths !Int (ModTimeFunction *Files) !*Files -> (Optional (*File, {#Char}, {#Char}),!*Files) 
fopenInSearchPaths moduleName fileNameExtension searchPaths mode modtimefunction f
	# fileName = replace_dots_by_directory_separators moduleName +++ fileNameExtension
	= case [path \\ (moduleName,path)<-searchPaths.sp_locations | moduleName == fileName] of
		[path:_]
			# fullFileName = path +++ fileName
			# (opened, file, f) = fopen fullFileName mode f
			| opened
				-> getModificationTime file path fullFileName modtimefunction f
				-> (No, f)
		[]
			-> fopenAnywhereInSearchPaths fileName searchPaths.sp_paths mode modtimefunction f
	where
		fopenAnywhereInSearchPaths :: !{#Char} ![{#Char}] !Int (ModTimeFunction *f) *f -> (Optional (*File, {#Char}, {#Char}),!*f) | FileSystem f
		fopenAnywhereInSearchPaths fileName [path : paths] mode modtimefunction f
			# fullFileName = path +++ fileName
			# (opened, file, f) = fopen fullFileName mode f
			| opened
				=	getModificationTime file path fullFileName modtimefunction f
				=	fopenAnywhereInSearchPaths fileName paths mode modtimefunction f
		fopenAnywhereInSearchPaths fileName [] _ _ f
			=	(No, f)

		getModificationTime :: *File {#Char} {#Char} (ModTimeFunction *f) *f -> (Optional (*File, {#Char}, {#Char}),!*f) | FileSystem f
		getModificationTime file path fullFileName modtimefunction f
			# (time, f) = modtimefunction fullFileName f
			=	(Yes (file, path, time), f)

		replace_dots_by_directory_separators :: !{#Char} -> *{#Char}
		replace_dots_by_directory_separators file_name
			= {if (c=='.') DirectorySeparator c \\ c<-:file_name}

closeScanner :: !ScanState !*Files -> *Files
closeScanner (ScanState scan_state) files = closeScanner_ scan_state files

closeScanner_ :: !RScanState !*Files -> *Files
closeScanner_ scanState=:{ss_input=PushedToken _ input} files
	= closeScanner_ {scanState & ss_input = input} files 
closeScanner_ {ss_input=Input {inp_stream}} files
	= case get_file inp_stream of
		Yes file	#	(_,files) = fclose file files
					->	files
		No			->	files
where
	get_file (InFile file)			= Yes file
	get_file (OldLine _ _ stream)	= get_file stream

NewLineChar	:== '\n'
LFChar		:== '\xA'
CRChar		:== '\xD'

isNewLine :: !Char -> Bool
isNewLine LFChar = True
isNewLine CRChar = True
isNewLine _      = False

  //------------------------//
 //--- Offside handling ---//
//------------------------//

UseLayout_ :: !RScanState -> (!Bool, !RScanState)
UseLayout_ scanState=:{ss_scanOptions}
	= ((ss_scanOptions bitand ScanOptionUseLayoutBit) <> 0, scanState)

UseLayout :: !ScanState -> (!Bool, !ScanState)
UseLayout (ScanState scanState)
	#! (useLayout, scanState) = UseLayout_ scanState
	= (useLayout,ScanState scanState)

setUseLayout :: !Bool !ScanState -> ScanState
setUseLayout b (ScanState ss) = ScanState  (setUseLayout_ b ss)

setUseLayout_ :: !Bool !RScanState -> RScanState
setUseLayout_ b ss=:{ss_scanOptions} = { ss & ss_scanOptions = if b (ss_scanOptions bitor ScanOptionUseLayoutBit) (ss_scanOptions bitand (bitnot ScanOptionUseLayoutBit)) } // -->> ("uselayout set to ",b)

setUseUnderscoreIdents :: !Bool !ScanState -> ScanState
setUseUnderscoreIdents b (ScanState ss) = ScanState  (setUseUnderscoreIdents_ b ss)

setUseUnderscoreIdents_ :: !Bool !RScanState -> RScanState
setUseUnderscoreIdents_ b ss=:{ss_scanOptions} = { ss & ss_scanOptions = if b (ss_scanOptions bitor ScanOptionUnderscoreIdentsBit) (ss_scanOptions bitand (bitnot ScanOptionUnderscoreIdentsBit)) } // -->> ("uselayout set to ",b)

setNoNewOffsideForSeqLetBit :: !ScanState -> ScanState
setNoNewOffsideForSeqLetBit (ScanState ss=:{ss_scanOptions}) = ScanState { ss & ss_scanOptions = ss_scanOptions bitor ScanOptionNoNewOffsideForSeqLetBit};

clearNoNewOffsideForSeqLetBit :: !ScanState -> ScanState
clearNoNewOffsideForSeqLetBit (ScanState ss=:{ss_scanOptions}) = ScanState { ss & ss_scanOptions = ss_scanOptions bitand (bitnot ScanOptionNoNewOffsideForSeqLetBit)};

checkOffside :: !FilePosition !Int !Token !RScanState -> (Token,RScanState)
checkOffside pos index token scanState=:{ss_offsides,ss_scanOptions,ss_input}
	| (ss_scanOptions bitand ScanOptionUseLayoutBit) == 0
		=	(token, scanState)  //-->> (token,pos,"No layout rule applied")
	| isEmpty ss_offsides
		=	newOffside token scanState  //-->> "Empty offside stack"
	#	(os_col, new_def)	= hd ss_offsides
		col					= pos.fp_col
	| col == os_col && canBeOffside token
		# scanState	= tokenBack scanState
		  newToken	= NewDefinitionToken
		= (	newToken
		  ,	{ scanState
			& ss_tokenBuffer
				= store 
					{	lt_position		= pos
					,	lt_index		= index
					,	lt_token		= newToken
					,	lt_context		= FunctionContext
					}
					scanState.ss_tokenBuffer
			}
		  )	-->> (token,"NewDefinitionToken generated col==os && canBeOffside",pos,ss_offsides)
	| col < os_col && token <> InToken
		# (n,os_col,new_def,offsides) = scan_offsides 0 col os_col new_def ss_offsides
		  scanState	= { scanState & ss_offsides = offsides } //-->> (n,"end groups",offsides,new_def)
		  scanState	= snd (newOffside token scanState)
		  scanState	= case new_def && col == os_col && canBeOffside token of
  						True
							#	scanState	= tokenBack scanState
								newToken	= NewDefinitionToken
							->	{ scanState
								& ss_tokenBuffer
									= store 
										{	lt_position		= pos
										,	lt_index		= index
										,	lt_token		= newToken
										,	lt_context		= FunctionContext
										}
										scanState.ss_tokenBuffer
								} -->> ("new definition generated",token)
						False
							->	scanState
		= gen_end_groups n scanState
		with
			newToken		= EndGroupToken
			scan_offsides n col os_col new_def []
				= (n, os_col, new_def, [])
			scan_offsides n col _ new_def offsides=:[(os_col,b):r]
				| col < os_col
					= scan_offsides (inc n) col os_col b r
					= (n, os_col, new_def, offsides)
			gen_end_groups n scanState
		  	  #	scanState	= tokenBack scanState	// push current token back
		  		scanState	=	{ scanState
								& ss_tokenBuffer
									= store 
										{	lt_position		= pos
										,	lt_index		= index
										,	lt_token		= newToken
										,	lt_context		= FunctionContext
										}
										scanState.ss_tokenBuffer
								} -->> ("end group generated",pos) // insert EndGroupToken 
			  | n == 1
			 	// # (new_offsides, scanState) = scanState!ss_offsides // for tracing XXX
			  	= (newToken, scanState) // -->> ("new offsides",new_offsides)
			  	= gen_end_groups (dec n) scanState
	| token == InToken
//		= (token, { scanState & ss_offsides = tl ss_offsides })
		= (token, scanState) // PK: parser removes offsides
		= newOffside token scanState
where
	newOffside token scanState=:{ss_offsides,ss_scanOptions}
	| definesOffside token
		&& ((ss_scanOptions bitand ScanOptionNoNewOffsideForSeqLetBit)==0
			|| (case token of
				SeqLetToken _ -> False
				_ -> True
			))
		#	( _, scanState )		= nextToken FunctionContext scanState
			( os_pos, scanState )	= getPosition scanState // next token defines offside position
			scanState				= tokenBack scanState
			os						= os_pos.fp_col
		|	os == 1
			#	scanState			= tokenBack scanState
				newToken			= ErrorToken "groups should not start in column 1"
			=	( newToken
				, 	{ scanState
					& ss_tokenBuffer
						= store 
							{	lt_position		= pos
							,	lt_index		= index
							,	lt_token		= newToken
							,	lt_context		= FunctionContext
							}
							scanState.ss_tokenBuffer
					}
				)
		// otherwise // os <> 1
			=	( token
				, { scanState
				  & ss_offsides = [ (os, needsNewDefinitionToken token) : ss_offsides ]
				  }
				) // -->> (token,pos,"New offside defined at ",os_pos,[ (os, token == CaseToken) : ss_offsides ])
	// otherwise // ~ (definesOffside token)
	= (token, scanState) -->> (token,pos," not offside")

definesOffside :: !Token -> Bool
definesOffside (LetToken _) 	= True
definesOffside (SeqLetToken _)	= True
definesOffside WhereToken		= True
definesOffside WithToken		= True
definesOffside SpecialToken		= True
definesOffside OfToken			= True
definesOffside _				= False

needsNewDefinitionToken :: !Token -> Bool
needsNewDefinitionToken OfToken			= True
needsNewDefinitionToken SpecialToken	= True
needsNewDefinitionToken _				= False

canBeOffside :: !Token -> Bool
canBeOffside EqualToken			= False
canBeOffside ColonDefinesToken	= False
canBeOffside DefinesColonToken	= False
canBeOffside (SeqLetToken _)	= False
canBeOffside WhereToken			= False
canBeOffside SpecialToken		= False
canBeOffside WithToken			= False
canBeOffside BarToken			= False
//canBeOffside CurlyOpenToken		= False // not allowed for record patterns
canBeOffside (CodeBlockToken _) = False
canBeOffside _					= True

dropOffsidePosition :: !ScanState -> ScanState
dropOffsidePosition (ScanState s) = ScanState (dropOffsidePosition_ s)

dropOffsidePosition_ :: !RScanState -> RScanState
dropOffsidePosition_ scanState=:{ss_offsides} = { scanState & ss_offsides = drop 1 ss_offsides }

  //-----------------------//
 //--- Buffer handling ---//
//-----------------------//

store :: !x !(Buffer x) -> Buffer x
store x  Buffer0        = Buffer1 x
store x (Buffer1 y)     = Buffer2 x y
store x (Buffer2 y z)   = Buffer3 x y z
store x (Buffer3 y z _) = Buffer3 x y z

isEmptyBuffer :: !(SBuffer x) -> Bool
isEmptyBuffer Buffer0 = True
isEmptyBuffer _       = False

get :: !(Buffer x) -> (x,!Buffer x)
get  Buffer0        = abort "get from empty buffer"
get (Buffer1 x)     = (x, Buffer0)
get (Buffer2 x y)   = (x, Buffer1 y)
get (Buffer3 x y z) = (x, Buffer2 y z)

pop :: !(Buffer x) -> Buffer x
pop  Buffer0        = Buffer0 //abort "pop from empty buffer"
pop (Buffer1 x)     = Buffer0
pop (Buffer2 x y)   = Buffer1 y
pop (Buffer3 x y z) = Buffer2 y z

head :: !(Buffer x) -> (x,!Buffer x);
head  Buffer0			= abort "head of empty buffer"
head b=:(Buffer1 x)		= (x,b)
head b=:(Buffer2 x _)	= (x,b)
head b=:(Buffer3 x _ _)	= (x,b)

instance <<< (SBuffer a) | <<< a
where
	(<<<) file  Buffer0			= file <<< "Empty buffer"
	(<<<) file (Buffer1 x)		= file <<< "Buffer1 (" <<< x <<< ")"
	(<<<) file (Buffer2 x y)	= file <<< "Buffer2 (" <<< x <<< ") (" <<< y <<< ")"
	(<<<) file (Buffer3 x y z)	= file <<< "Buffer3 (" <<< x <<< ") (" <<< y <<< ") (" <<< z <<< ")"

  //---------------//
 //--- Tracing ---//
//---------------//

(-->>) val _ :== val
//(-->>) val message :== val ---> ("Scanner",message)
