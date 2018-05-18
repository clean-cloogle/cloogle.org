definition module Builtin.Predef

from Cloogle.DB import :: FunctionEntry, :: ClassEntry, :: TypeDefEntry,
	:: CleanLangReportLocation

CLR :: Int String String -> CleanLangReportLocation

builtin_functions :: [FunctionEntry]
builtin_classes :: [ClassEntry]
builtin_types :: [TypeDefEntry]
