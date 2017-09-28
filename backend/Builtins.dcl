definition module Builtins

from CloogleDB import :: Location, :: FunctionEntry, :: ClassEntry,
	:: TypeDefEntry, :: SyntaxEntry, :: SyntaxResultExtras

builtin_functions :: [(Location, FunctionEntry)]
builtin_classes :: [(Location, ClassEntry)]
builtin_types :: [(Location, TypeDefEntry)]
builtin_syntax :: [([String], SyntaxEntry)]
