definition module Builtins

from CloogleDB import :: FunctionEntry, :: ClassEntry, :: TypeDefEntry,
	:: SyntaxEntry

builtin_functions :: [(FunctionEntry)]
builtin_classes :: [(ClassEntry)]
builtin_types :: [(TypeDefEntry)]
builtin_syntax :: [([String], SyntaxEntry)]
