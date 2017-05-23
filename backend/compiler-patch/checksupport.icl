implementation module checksupport

import StdEnv, compare_constructor
import syntax, predef, containers
import utilities

cUndef			:== -1

instance toInt STE_Kind
where
	toInt STE_Type					= cTypeDefs
	toInt STE_Constructor			= cConstructorDefs
	toInt (STE_Field _)				= cSelectorDefs
	toInt STE_Class					= cClassDefs
	toInt (STE_Generic _)			= cGenericDefs
	toInt STE_GenericCase			= cGenericCaseDefs
	toInt STE_Member				= cMemberDefs
	toInt STE_Instance				= cInstanceDefs
	toInt STE_DclFunction			= cFunctionDefs
	toInt (STE_FunctionOrMacro _)	= cMacroDefs
	toInt (STE_DclMacroOrLocalMacroFunction _)= cMacroDefs
	toInt STE_GenericDeriveClass	= cGenericCaseDefs
	toInt STE_TypeExtension			= cTypeDefs
	toInt _							= NoIndex

instance Erroradmin ErrorAdmin
where
	pushErrorAdmin pos error=:{ea_loc}
		= { error & ea_loc = [pos : ea_loc] } 
	
	setErrorAdmin pos error
		= { error & ea_loc = [pos] } 
	
	popErrorAdmin error=:{ea_loc = [_:ea_locs]}
		=  { error & ea_loc = ea_locs }

instance Erroradmin CheckState
where
	pushErrorAdmin pos cs=:{cs_error}
		= {cs & cs_error = pushErrorAdmin pos cs_error } 
	
	setErrorAdmin pos cs=:{cs_error}
		= {cs & cs_error = setErrorAdmin pos cs_error }
	
	popErrorAdmin cs=:{cs_error}
		= {cs & cs_error = popErrorAdmin cs_error } //...PK

newPosition :: !Ident !Position -> IdentPos
newPosition id (FunPos file_name line_nr _)
	= { ip_ident = id, ip_line = line_nr, ip_file = file_name }
newPosition id (LinePos file_name line_nr)
	= { ip_ident = id, ip_line = line_nr, ip_file = file_name }
newPosition id (PreDefPos file_name)
	= { ip_ident = id, ip_line = cNotALineNumber, ip_file = file_name.id_name }
newPosition id NoPos
	= { ip_ident = id, ip_line = cNotALineNumber, ip_file = "???" }

stringPosition :: !String !Position -> StringPos
stringPosition id (FunPos file_name line_nr _)
	= { sp_name = id, sp_line = line_nr, sp_file = file_name }
stringPosition id (LinePos file_name line_nr)
	= { sp_name = id, sp_line = line_nr, sp_file = file_name }
stringPosition id (PreDefPos file_name)
	= { sp_name = id, sp_line = cNotALineNumber, sp_file = file_name.id_name }
stringPosition id NoPos
	= { sp_name = id, sp_line = cNotALineNumber, sp_file = "???" }

checkError :: !a !b !*ErrorAdmin -> *ErrorAdmin | <<< a & <<< b // PK
checkError id mess error=:{ea_file,ea_loc=[]}
	= { error & ea_file = ea_file <<< "Error " <<< " " <<< id <<< " " <<< mess <<< '\n', ea_ok = False }
checkError id mess error=:{ea_file,ea_loc}
	= { error & ea_file = ea_file <<< "Error " <<< hd ea_loc <<< ": " <<< id  <<< " " <<< mess <<< '\n', ea_ok = False }

checkWarning :: !a !b !*ErrorAdmin -> *ErrorAdmin | <<< a & <<< b // PK
checkWarning id mess error=:{ea_file,ea_loc=[]}
	= { error & ea_file = ea_file <<< "Warning " <<< " " <<< id <<< " " <<< mess <<< '\n' }
checkWarning id mess error=:{ea_file,ea_loc}
	= { error & ea_file = ea_file <<< "Warning " <<< hd ea_loc <<< ": " <<< id  <<< " " <<< mess <<< '\n' }

checkErrorWithIdentPos :: !IdentPos !a !*ErrorAdmin -> .ErrorAdmin | <<< a;
checkErrorWithIdentPos ident_pos mess error=:{ea_file}
	= { error & ea_file = ea_file <<< "Error " <<< ident_pos <<< ": " <<< mess <<< '\n', ea_ok = False }

checkErrorWithPosition :: !Ident !Position !a !*ErrorAdmin -> .ErrorAdmin | <<< a;
checkErrorWithPosition ident pos mess error=:{ea_file}
	# ident_pos = newPosition ident pos
	= { error & ea_file = ea_file <<< "Error " <<< ident_pos <<< ": " <<< mess <<< '\n', ea_ok = False }

checkWarningWithPosition :: !Ident !Position !a !*ErrorAdmin -> .ErrorAdmin | <<< a;
checkWarningWithPosition ident pos mess error=:{ea_file}
	# ident_pos = newPosition ident pos
	= { error & ea_file = ea_file <<< "Warning " <<< ident_pos <<< ": " <<< mess <<< '\n' }

class envLookUp a :: !a !(Env Ident .b) -> (!Bool,.b)

instance envLookUp TypeVar
where
	envLookUp var [bind:binds]
		| var.tv_ident == bind.bind_src
			= (True, bind.bind_dst)
			= envLookUp var binds
	envLookUp var []
		= (False, abort "illegal value")
	
instance envLookUp AttributeVar
where
	envLookUp var [bind:binds]
		| var.av_ident == bind.bind_src
			= (True, bind.bind_dst)
			= envLookUp var binds
	envLookUp var []
		= (False, abort "illegal value")

instance envLookUp ATypeVar
where
	envLookUp var=:{atv_variable} [bind:binds]
		| atv_variable.tv_ident == bind.bind_src
			= (True, bind.bind_dst)
			= envLookUp var binds
	envLookUp var []
		= (False, abort "illegal value")

retrieveGlobalDefinition :: !SymbolTableEntry !STE_Kind !Index -> (!Index, !Index)
retrieveGlobalDefinition {ste_kind = STE_Imported kind decl_index, ste_def_level, ste_index} requ_kind mod_index
	| kind == requ_kind
		= (ste_index, decl_index)
		= (NotFound, mod_index)
retrieveGlobalDefinition {ste_kind,ste_def_level,ste_index} requ_kind mod_index
	| ste_kind == requ_kind && ste_def_level == cGlobalScope
		= (ste_index, mod_index)
		= (NotFound, mod_index)

retrieveGlobalGenericDefinition :: !SymbolTableEntry !Index -> (!Index, !Index, !Int)
retrieveGlobalGenericDefinition {ste_kind = STE_Imported (STE_Generic arity) decl_index, ste_def_level, ste_index} mod_index
	= (ste_index, decl_index, arity)
retrieveGlobalGenericDefinition {ste_kind = STE_Generic arity,ste_def_level,ste_index} mod_index
	| ste_def_level == cGlobalScope
		= (ste_index, mod_index, arity)
retrieveGlobalGenericDefinition _ mod_index
	= (NotFound, mod_index ,-1)

getBelongingSymbols :: !Declaration !v:{#DclModule} -> (!BelongingSymbols, !v:{#DclModule})
getBelongingSymbols (Declaration {decl_kind=STE_Imported STE_Type def_mod_index, decl_index}) dcl_modules
	# ({td_rhs}, dcl_modules)
			= dcl_modules![def_mod_index].dcl_common.com_type_defs.[decl_index]
	= case td_rhs of
		AlgType constructors
			-> (BS_Constructors constructors, dcl_modules)
		RecordType {rt_fields}
			-> (BS_Fields rt_fields, dcl_modules)
		_
			-> (BS_Nothing, dcl_modules)
getBelongingSymbols (Declaration {decl_kind=STE_Imported STE_Class def_mod_index, decl_index}) dcl_modules
	# ({class_members}, dcl_modules)
			= dcl_modules![def_mod_index].dcl_common.com_class_defs.[decl_index]
	= (BS_Members class_members, dcl_modules)
getBelongingSymbols _ dcl_modules
	= (BS_Nothing, dcl_modules)

nrOfBelongingSymbols :: !BelongingSymbols -> Int
nrOfBelongingSymbols (BS_Constructors constructors)
	= length constructors
nrOfBelongingSymbols (BS_Fields fields)
	= size fields
nrOfBelongingSymbols (BS_Members members)
	= size members
nrOfBelongingSymbols BS_Nothing
	= 0

removeImportsAndLocalsOfModuleFromSymbolTable :: !Declarations !*(Heap SymbolTableEntry) -> .Heap SymbolTableEntry
removeImportsAndLocalsOfModuleFromSymbolTable {dcls_import,dcls_local_for_import} symbol_table
	# symbol_table = remove_declared_symbols_in_array 0 dcls_import symbol_table
	= remove_declared_symbols_in_array 0 dcls_local_for_import symbol_table
where
	remove_declared_symbols_in_array :: !Int !{!Declaration} !*SymbolTable -> *SymbolTable
	remove_declared_symbols_in_array symbol_index symbols symbol_table
		| symbol_index<size symbols
			# symbol = symbols.[symbol_index]
			# (Declaration {decl_ident={id_info}})=symbol
			# (entry, symbol_table) = readPtr id_info symbol_table
			# {ste_kind,ste_def_level} = entry
			| ste_kind == STE_Empty || ste_def_level > cModuleScope
				= remove_declared_symbols_in_array (symbol_index+1) symbols symbol_table
				# symbol_table = symbol_table <:= (id_info, entry.ste_previous)
				= case ste_kind of
					STE_Field selector_id
						#! declaration = symbols.[symbol_index]
						# (Declaration {decl_index}) = declaration
						-> remove_declared_symbols_in_array (symbol_index+1) symbols (removeFieldFromSelectorDefinition selector_id NoIndex decl_index symbol_table)
					STE_Imported (STE_Field selector_id) def_mod
						#! declaration = symbols.[symbol_index]
						# (Declaration {decl_index}) = declaration
						-> remove_declared_symbols_in_array (symbol_index+1) symbols (removeFieldFromSelectorDefinition selector_id def_mod decl_index symbol_table)
					_
						-> remove_declared_symbols_in_array (symbol_index+1) symbols symbol_table
			= symbol_table

addLocalFunctionDefsToSymbolTable :: !Level !Index !Index !Bool !*{#FunDef} !*SymbolTable !*ErrorAdmin -> (!*{# FunDef}, !*SymbolTable, !*ErrorAdmin)
addLocalFunctionDefsToSymbolTable level from_index to_index is_macro_fun fun_defs symbol_table error
	| from_index == to_index
		= (fun_defs, symbol_table, error)	
		# (fun_def, fun_defs) = fun_defs![from_index]
		# (symbol_table, error) = addDefToSymbolTable level from_index fun_def.fun_ident (STE_FunctionOrMacro []) symbol_table error
		| is_macro_fun
			# fun_defs = {fun_defs & [from_index].fun_info.fi_properties = fun_def.fun_info.fi_properties bitor FI_IsMacroFun }
			= addLocalFunctionDefsToSymbolTable level (inc from_index) to_index is_macro_fun fun_defs symbol_table error
			= addLocalFunctionDefsToSymbolTable level (inc from_index) to_index is_macro_fun fun_defs symbol_table error

addLocalDclMacroDefsToSymbolTable :: !Level !Int !Index !Index !*{#*{#FunDef}} !*SymbolTable !*ErrorAdmin -> (!*{#*{#FunDef}}, !*SymbolTable, !*ErrorAdmin)
addLocalDclMacroDefsToSymbolTable level module_index from_index to_index macro_defs symbol_table error
	| from_index == to_index
		= (macro_defs, symbol_table, error)	
		# (macro_def, macro_defs) = macro_defs![module_index,from_index]
		# (symbol_table, error) = addDefToSymbolTable level from_index macro_def.fun_ident (STE_DclMacroOrLocalMacroFunction []) symbol_table error
		# macro_defs = {macro_defs & [module_index].[from_index].fun_info.fi_properties = macro_def.fun_info.fi_properties bitor FI_IsMacroFun }
		= addLocalDclMacroDefsToSymbolTable level module_index (inc from_index) to_index macro_defs symbol_table error

NewEntry symbol_table symb_ptr def_kind def_index level previous :==
	 symbol_table <:= (symb_ptr,{  ste_kind = def_kind, ste_index = def_index, ste_def_level = level, ste_previous = previous, ste_doc = No })

addDefToSymbolTable :: !Level !Index !Ident !STE_Kind !*SymbolTable !*ErrorAdmin -> (!* SymbolTable, !*ErrorAdmin)
addDefToSymbolTable level def_index def_ident=:{id_info} def_kind symbol_table error
	# (entry, symbol_table) = readPtr id_info symbol_table
	| entry.ste_kind == STE_Empty || entry.ste_def_level <> level
		# entry = {ste_index = def_index, ste_kind = def_kind, ste_def_level = level, ste_previous = entry, ste_doc = No }
		= (symbol_table <:= (id_info,entry), error)
		= (symbol_table, checkError def_ident "already defined" error)

addDeclarationsOfDclModToSymbolTable :: Int !{!Declaration} !{!Declaration} !*{#DclModule} !*CheckState -> (!*{#DclModule},!*CheckState)
addDeclarationsOfDclModToSymbolTable ste_index locals imported dcl_modules cs
	# (dcl_modules,cs) = add_imports_in_array_to_symbol_table 0 imported dcl_modules cs
	= addLocalSymbolsForImportToSymbolTable 0 locals ste_index dcl_modules cs
  where
	add_imports_in_array_to_symbol_table :: !Int !{!Declaration} !*{#DclModule} !*CheckState -> (!*{#DclModule},!*CheckState)
	add_imports_in_array_to_symbol_table symbol_index symbols dcl_modules cs=:{cs_x}
		| symbol_index<size symbols
			#! (Declaration {decl_ident,decl_pos,decl_kind},symbols) = symbols![symbol_index]
			= case decl_kind of
				STE_Imported def_kind def_mod
					#! declaration = symbols.[symbol_index]
					# (Declaration {decl_index}) = declaration
					# (_,dcl_modules,cs) = addSymbol No decl_ident decl_pos decl_kind def_kind decl_index def_mod cUndef dcl_modules cs
					-> add_imports_in_array_to_symbol_table (symbol_index+1) symbols dcl_modules cs
				STE_FunctionOrMacro _
					#! declaration = symbols.[symbol_index]
					# (Declaration {decl_index}) = declaration
					# (_,dcl_modules,cs) = addImportedFunctionOrMacro No decl_ident decl_index dcl_modules cs
					-> add_imports_in_array_to_symbol_table (symbol_index+1) symbols dcl_modules cs
			= (dcl_modules,cs)

	addLocalSymbolsForImportToSymbolTable :: !Int !{!Declaration} Int !*{#DclModule} !*CheckState -> (!*{#DclModule},!*CheckState)
	addLocalSymbolsForImportToSymbolTable symbol_index symbols mod_index dcl_modules cs
		| symbol_index<size symbols
			# (Declaration {decl_ident,decl_pos,decl_kind,decl_index},symbols) = symbols![symbol_index]
			= case decl_kind of
				STE_FunctionOrMacro _
					# (_,dcl_modules,cs) = addImportedFunctionOrMacro No decl_ident decl_index dcl_modules cs
					-> addLocalSymbolsForImportToSymbolTable (symbol_index+1) symbols mod_index dcl_modules cs
				STE_Imported def_kind def_mod
					# (_,dcl_modules,cs) = addSymbol No decl_ident decl_pos decl_kind def_kind decl_index mod_index cUndef dcl_modules cs
					-> addLocalSymbolsForImportToSymbolTable (symbol_index+1) symbols mod_index dcl_modules cs
			= (dcl_modules,cs)

addImportedFunctionOrMacro :: !(Optional IndexRange) !Ident !Int !*{#DclModule} !*CheckState -> (!Bool,!*{#DclModule},!.CheckState)
addImportedFunctionOrMacro opt_dcl_macro_range ident=:{id_info} def_index dcl_modules cs=:{cs_symbol_table}
	# (entry, cs_symbol_table) = readPtr id_info cs_symbol_table
	  cs = { cs & cs_symbol_table = cs_symbol_table }
	= case entry.ste_kind of
		STE_Empty
			-> (True, dcl_modules, { cs & cs_symbol_table = NewEntry cs.cs_symbol_table id_info (STE_FunctionOrMacro []) 
													def_index cModuleScope entry})
		STE_FunctionOrMacro _
			| entry.ste_index == def_index || within_opt_range opt_dcl_macro_range def_index
				-> (False, dcl_modules, cs)
		STE_Imported _ module_n
			| module_n>=0 && module_n<size dcl_modules
				# (dcl_name,dcl_modules) = dcl_modules![module_n].dcl_name
				  cs & cs_error = checkError ident ("multiply defined (also defined in module "+++toString dcl_name+++")") cs.cs_error
				-> (False, dcl_modules, cs)
		_
			-> (False, dcl_modules, { cs & cs_error = checkError ident "multiply defined" cs.cs_error})
  where
	within_opt_range (Yes {ir_from, ir_to}) i
		= ir_from<=i && i<ir_to
	within_opt_range No _
		= False

addFieldToSelectorDefinition :: !Ident (Global .Int) !*CheckState -> .CheckState;
addFieldToSelectorDefinition {id_info} glob_field_index cs=:{cs_symbol_table}
	# (entry, cs_symbol_table) = readPtr id_info cs_symbol_table 
	  cs = { cs & cs_symbol_table = cs_symbol_table }
	= case entry.ste_kind of
		STE_Selector selector_list
			-> { cs & cs_symbol_table = cs.cs_symbol_table <:= (id_info, { entry & ste_kind = STE_Selector [ glob_field_index : selector_list ] })}
		_
			-> { cs & cs_symbol_table = NewEntry cs.cs_symbol_table id_info (STE_Selector [glob_field_index]) NoIndex cModuleScope entry }

addSymbol :: !(Optional a) !Ident !Position !STE_Kind !STE_Kind !.Int !.Int !Int !*{#DclModule} !*CheckState -> (!Bool,!*{#DclModule},!*CheckState)
addSymbol yes_for_icl_module ident pos decl_kind def_kind def_index def_mod importing_mod dcl_modules cs=:{cs_symbol_table}
	# (entry, cs_symbol_table) = readPtr ident.id_info cs_symbol_table
	= add_indirectly_imported_symbol yes_for_icl_module entry ident pos def_kind def_index def_mod 
			importing_mod dcl_modules {cs & cs_symbol_table = cs_symbol_table}
	where
		add_indirectly_imported_symbol _ {ste_kind = STE_Empty} {id_info} _ def_kind def_index def_mod _ dcl_modules cs=:{cs_symbol_table}
			# (entry, cs_symbol_table) = readPtr id_info cs_symbol_table
			  cs = { cs & cs_symbol_table = NewEntry cs_symbol_table id_info decl_kind def_index cModuleScope entry}
			= case def_kind of
				STE_Field selector_id
					-> (True, dcl_modules, addFieldToSelectorDefinition selector_id	{ glob_module = def_mod, glob_object = def_index } cs)
				_
					-> (True, dcl_modules, cs)
		add_indirectly_imported_symbol _ {ste_kind = STE_Imported kind mod_index, ste_index} _ _ def_kind def_index def_mod _ dcl_modules cs
			| kind == def_kind && mod_index == def_mod && ste_index == def_index
				= (False, dcl_modules, cs)
		add_indirectly_imported_symbol (Yes _) _ _ _ def_kind def_index def_mod _ dcl_modules cs
			| def_mod == cs.cs_x.x_main_dcl_module_n
				// an icl module imports one of it's definitions from the dcl module
				= (False, dcl_modules, cs)
		add_indirectly_imported_symbol _ _ _ _ def_kind def_index def_mod importing_mod dcl_modules cs
			| importing_mod==def_mod // a dcl module imports a definition from itself (cycle)
				= (False, dcl_modules, cs)
		add_indirectly_imported_symbol _ {ste_kind = STE_Imported _ mod_index, ste_index} _ _ def_kind def_index def_mod _ dcl_modules cs=:{cs_error}
			| mod_index>=0 && mod_index<size dcl_modules && def_mod>=0 && def_mod<size dcl_modules
				# (dcl_name1,dcl_modules) = dcl_modules![def_mod].dcl_name
				  (dcl_name2,dcl_modules) = dcl_modules![mod_index].dcl_name
				  cs & cs_error = checkError ident ("multiply defined (in module "+++toString dcl_name1+++" and already in module "+++toString dcl_name2+++")") cs_error
				= (False, dcl_modules, cs)
		add_indirectly_imported_symbol _ entry ident pos def_kind def_index def_mod _ dcl_modules cs=:{cs_error}
			= (False, dcl_modules, { cs & cs_error = checkError ident "multiply defined" cs_error})

addGlobalDefinitionsToSymbolTable :: ![Declaration] !*CheckState -> .CheckState;
addGlobalDefinitionsToSymbolTable decls cs
	= foldSt add_global_definition decls cs
where
	add_global_definition (Declaration {decl_ident=ident=:{id_info},decl_pos,decl_kind,decl_index}) cs=:{cs_symbol_table}
		# (entry, cs_symbol_table) = readPtr id_info cs_symbol_table
		| entry.ste_def_level < cGlobalScope
			# cs = { cs & cs_symbol_table = NewEntry cs_symbol_table id_info decl_kind decl_index cGlobalScope entry }
			= case decl_kind of
				STE_Field selector_id
					-> addFieldToSelectorDefinition selector_id	{ glob_module = NoIndex, glob_object = decl_index } cs
				_
					-> cs
			= { cs & cs_symbol_table = cs_symbol_table, cs_error = checkErrorWithIdentPos (newPosition ident decl_pos) "multiply defined" cs.cs_error}

removeImportedSymbolsFromSymbolTable :: Declaration !*SymbolTable -> .SymbolTable
removeImportedSymbolsFromSymbolTable (Declaration {decl_ident=decl_ident=:{id_info}, decl_index}) symbol_table
	# ({ste_kind,ste_def_level,ste_previous}, symbol_table) = readPtr id_info symbol_table
	  symbol_table = symbol_table <:= (id_info, ste_previous)
	= case ste_kind of
		STE_Imported (STE_Field selector_id) def_mod
			-> removeFieldFromSelectorDefinition selector_id def_mod decl_index symbol_table
		_
			-> symbol_table

removeFieldFromSelectorDefinition :: !Ident .Int .Int !*(Heap SymbolTableEntry) -> .Heap SymbolTableEntry;
removeFieldFromSelectorDefinition {id_info} field_mod field_index symbol_table 
	# (entry, symbol_table) = readPtr id_info symbol_table
	= case entry.ste_kind of
	  	STE_Selector selector_list
			-> symbol_table <:= (id_info, { entry & ste_kind = STE_Selector (remove_field field_mod field_index selector_list) })
		_	-> symbol_table
where	
	remove_field field_mod field_index [field=:{glob_module, glob_object} : fields]
		| field_mod == glob_module && field_index == glob_object
			= fields
			= [field : remove_field field_mod field_index fields]
	remove_field field_mod field_index []
		= []

removeDeclarationsFromSymbolTable :: ![Declaration] !Int !*SymbolTable -> *SymbolTable
removeDeclarationsFromSymbolTable decls scope symbol_table
	= foldSt (remove_declaration scope) decls symbol_table
where
	remove_declaration scope decl=:(Declaration {decl_ident={id_info}, decl_index}) symbol_table
		# ({ste_kind,ste_previous,ste_def_level}, symbol_table) = readPtr id_info symbol_table
		= case ste_kind of
			STE_Field selector_id
				# symbol_table = removeFieldFromSelectorDefinition selector_id NoIndex decl_index symbol_table
				| ste_def_level<>scope && scope==cGlobalScope
					-> symbol_table
				| ste_previous.ste_def_level == scope
					-> symbol_table <:= (id_info, ste_previous.ste_previous)
					-> symbol_table <:= (id_info, ste_previous)
			STE_Empty
				-> symbol_table
			_
				| ste_def_level<>scope && scope==cGlobalScope
					-> symbol_table
				| ste_previous.ste_def_level == scope
					-> symbol_table <:= (id_info, ste_previous.ste_previous)
					-> symbol_table <:= (id_info, ste_previous)

removeLocalIdentsFromSymbolTable :: .Int !.[Ident] !*(Heap SymbolTableEntry) -> .Heap SymbolTableEntry;
removeLocalIdentsFromSymbolTable level idents symbol_table
	= foldSt (removeIdentFromSymbolTable level) idents symbol_table

removeIdentFromSymbolTable :: !.Int !Ident !*(Heap SymbolTableEntry) -> .Heap SymbolTableEntry;
removeIdentFromSymbolTable level {id_name,id_info} symbol_table
	# ({ste_previous,ste_def_level}, symbol_table) = readPtr id_info symbol_table
	| level <= ste_def_level 
		= symbol_table <:= (id_info,ste_previous) // ---> ("removeIdentFromSymbolTable", id_name)
		= symbol_table // ---> ("NO removeIdentFromSymbolTable", id_name)

removeLocalDclMacrosFromSymbolTable :: !Level !Index !IndexRange !*{#*{#FunDef}} !*(Heap SymbolTableEntry) -> (!.{#.{#FunDef}}, !.Heap SymbolTableEntry)
removeLocalDclMacrosFromSymbolTable level module_index {ir_from,ir_to} defs symbol_table
	= remove_macro_defs_from_symbol_table level ir_from ir_to defs symbol_table
where
	remove_macro_defs_from_symbol_table level from_index to_index defs symbol_table
		| from_index == to_index
			= (defs, symbol_table)	
			# (def,defs) = defs![module_index,from_index]
			  id_info = (toIdent def).id_info
			  (entry,symbol_table) = readPtr id_info symbol_table
			| level == entry.ste_def_level
				= remove_macro_defs_from_symbol_table level (inc from_index) to_index defs (symbol_table <:= (id_info, entry.ste_previous))
				= remove_macro_defs_from_symbol_table level (inc from_index) to_index defs symbol_table

removeLocalFunctionsFromSymbolTable :: !Level !IndexRange !*{# FunDef} !*(Heap SymbolTableEntry) -> (!.{# FunDef}, !.Heap SymbolTableEntry)
removeLocalFunctionsFromSymbolTable level {ir_from,ir_to} defs symbol_table
	= remove_fun_defs_from_symbol_table level ir_from ir_to defs symbol_table
where
	remove_fun_defs_from_symbol_table level from_index to_index defs symbol_table
		| from_index == to_index
			= (defs, symbol_table)	
			# (def,defs) = defs![from_index]
			  id_info = (toIdent def).id_info
			#  (entry, symbol_table) = readPtr id_info symbol_table
			| level == entry.ste_def_level
				= remove_fun_defs_from_symbol_table level (inc from_index) to_index defs (symbol_table <:= (id_info, entry.ste_previous))
				= remove_fun_defs_from_symbol_table level (inc from_index) to_index defs symbol_table

newFreeVariable :: !FreeVar ![FreeVar] ->(!Bool, ![FreeVar])
newFreeVariable new_var vars=:[free_var=:{fv_def_level,fv_info_ptr}: free_vars]
	| new_var.fv_def_level > fv_def_level
		= (True, [new_var : vars])
	| new_var.fv_def_level == fv_def_level
		| new_var.fv_info_ptr == fv_info_ptr
			= (False, vars)
			#! (free_var_added, free_vars) = newFreeVariable new_var free_vars
			= (free_var_added, [free_var : free_vars])
		#! (free_var_added, free_vars) = newFreeVariable new_var free_vars
		= (free_var_added, [free_var : free_vars])
newFreeVariable new_var []
	= (True, [new_var])


local_declaration_for_import :: !u:Declaration .Index -> v:Declaration, [u <= v]
local_declaration_for_import decl=:(Declaration {decl_kind=STE_FunctionOrMacro _}) module_n
	= decl
local_declaration_for_import decl=:(Declaration {decl_kind=STE_Imported _ _}) module_n
	= abort "local_declaration_for_import"
local_declaration_for_import decl=:(Declaration declaration_record=:{decl_kind}) module_n
	= Declaration {declaration_record & decl_kind = STE_Imported decl_kind module_n}

instance toIdent SymbIdent
where
	toIdent symb = symb.symb_ident

instance toIdent TypeSymbIdent
where
	toIdent type_symb = type_symb.type_ident

instance toIdent BoundVar
where
	toIdent var = var.var_ident

instance toIdent TypeVar
where
	toIdent tvar = tvar.tv_ident

instance toIdent ATypeVar
where
	toIdent {atv_variable} = atv_variable.tv_ident


instance toIdent Ident
where
	toIdent id = id

instance toIdent ConsDef
where
	toIdent cons = cons.cons_ident

instance toIdent (TypeDef a)
where
	toIdent td = td.td_ident

instance toIdent ClassDef
where
	toIdent cl = cl.class_ident

instance toIdent MemberDef
where
	toIdent me = me.me_ident

instance toIdent FunDef
where
	toIdent fun = fun.fun_ident

instance toIdent SelectorDef
where
	toIdent sd = sd.sd_ident

/*
instance toIdent DeltaRule
where
	toIdent delta = delta.delta_name
*/

instance toIdent (a,b) | toIdent a
where
	toIdent (x,y) = toIdent x

instance == STE_Kind
where
	(==) (STE_FunctionOrMacro _) STE_DclFunction	= True
	(==) (STE_FunctionOrMacro _) (STE_DclMacroOrLocalMacroFunction _) = True
	(==) STE_DclFunction (STE_FunctionOrMacro _)	= True
	(==) (STE_DclMacroOrLocalMacroFunction _) (STE_FunctionOrMacro _)	= True
	(==) sk1 sk2 									= equal_constructor sk1 sk2

instance <<< IdentPos
where
	(<<<) file {ip_file,ip_line,ip_ident}
	| ip_line == cNotALineNumber
		= file <<< '[' <<< ip_file <<< ',' <<< ip_ident <<< ']'
		= file <<< '[' <<< ip_file <<< ',' <<< ip_line <<< ',' <<< ip_ident <<< ']'

instance <<< StringPos where
	(<<<) file {sp_file,sp_line,sp_name}
	| sp_line == cNotALineNumber
		= file <<< '[' <<< sp_file <<< ',' <<< sp_name <<< ']'
		= file <<< '[' <<< sp_file <<< ',' <<< sp_line <<< ',' <<< sp_name <<< ']'

instance <<< ExplImpInfo
  where
	(<<<) file (ExplImpInfo eii_ident eii_declaring_modules)
		= file <<< eii_ident //<<< " is declared in " <<< eii_declaring_modules

instance <<< DeclarationInfo
  where
	(<<<) file {di_decl}
		= file <<< di_decl

import_ident :: Ident
import_ident =: { id_name = "import", id_info = nilPtr }

restoreHeap :: !Ident !*SymbolTable -> .SymbolTable
restoreHeap {id_info} cs_symbol_table
		# ({ste_previous}, cs_symbol_table)
			= readPtr id_info cs_symbol_table
		= writePtr id_info ste_previous cs_symbol_table
