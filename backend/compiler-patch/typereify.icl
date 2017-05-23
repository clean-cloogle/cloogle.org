implementation module typereify

import syntax
import typesupport

class makeTypeFun a :: Ident Position SymbolType *VarHeap *SymbolTable
	-> (a, *VarHeap, *SymbolTable)

instance makeTypeFun FunDef where
	makeTypeFun ident position symbol_type var_heap symbol_table	
		=	(function, var_heap, symbol_table)
		where
			function =
				{	fun_ident = ident
				,	fun_arity = 1
				,	fun_priority = NoPrio
				,	fun_body = GeneratedBody
				,	fun_type = Yes symbol_type
				,	fun_pos = position
				,	fun_kind = FK_Function False
				,	fun_lifted = 0
				,	fun_info = EmptyFunInfo
				}

instance makeTypeFun FunType where
	makeTypeFun ident position symbol_type var_heap symbol_table
		# (entry, symbol_table)
			=	readPtr ident.id_info symbol_table
		# entry
			=	{ entry & ste_kind = STE_DclFunction}
		# symbol_table
			=	writePtr ident.id_info entry symbol_table
		# (ft_type_ptr, var_heap)
			=	newPtr VI_Empty var_heap
		=	({	ft_ident = ident
			,	ft_arity = 1
			,	ft_priority = NoPrio
			,	ft_type = symbol_type
			,	ft_pos = position
			,	ft_specials = FSP_None
			,	ft_type_ptr	= ft_type_ptr
			}, var_heap, symbol_table)

add_dcl_type_fun_types :: TypeSymbIdent Int *{#DclModule} *VarHeap *SymbolTable
										-> (*{#DclModule},*VarHeap,*SymbolTable)
add_dcl_type_fun_types ctListDefSymb n_cached_dcls dcl_mods var_heap symbols
	# (n, dcl_mods) = usize dcl_mods
	= add_type_fun_types n_cached_dcls n ctListDefSymb dcl_mods var_heap symbols
	where
		add_type_fun_types :: Int Int TypeSymbIdent *{#DclModule} *VarHeap *SymbolTable
												-> (*{#DclModule},*VarHeap,*SymbolTable)
		add_type_fun_types module_n n ctListDefSymb dcl_mods var_heap symbols
			| module_n >= n
				=	(dcl_mods, var_heap, symbols)
			| module_n == cPredefinedModuleIndex
				=	add_type_fun_types (module_n+1) n ctListDefSymb dcl_mods var_heap symbols
				# (dcl_mod, dcl_mods) = dcl_mods![module_n]
				# (dcl_mod, var_heap, symbols)
					=	add_fun_types_of_dcl_module ctListDefSymb dcl_mod var_heap symbols
				# dcl_mods = {dcl_mods & [module_n] = dcl_mod}
				=	add_type_fun_types (module_n+1) n ctListDefSymb dcl_mods var_heap symbols

add_fun_types_of_dcl_module :: TypeSymbIdent DclModule *VarHeap *SymbolTable
										 -> (DclModule,*VarHeap,*SymbolTable)
add_fun_types_of_dcl_module ctListDefSymb dcl_mod=:{dcl_functions, dcl_common={com_type_defs}} var_heap symbols
	# n_functions = size dcl_functions
	  (type_funs, com_type_defs, var_heap, symbols)
		=	addTypeFunctionsA n_functions ctListDefSymb {def \\ def <-: com_type_defs} var_heap symbols
	  dcl_functions = {function \\ function <- [e \\ e <-: dcl_functions] ++ type_funs}
	  dcl_type_funs = {ir_from = n_functions, ir_to = size dcl_functions}
	  dcl_mod = { dcl_mod	&	dcl_functions = dcl_functions
							,	dcl_common.com_type_defs = com_type_defs
							,	dcl_type_funs = dcl_type_funs
							}
	= (dcl_mod, var_heap, symbols)

getListTypeSymb predefs
	# ({pds_module, pds_def}, predefs) = predefs![PD_ListType]
	  ident = predefined_idents.[PD_ListType]
	  type_symb = {MakeNewTypeSymbIdent ident 0 & type_index.glob_module = pds_module, type_index.glob_object = pds_def}
	= (type_symb, predefs)

getNilSymb :: *PredefinedSymbols -> (SymbIdent, !*PredefinedSymbols)
getNilSymb predefs
	# ({pds_module, pds_def}, predefs) = predefs![PD_NilSymbol]
	  pds_ident = predefined_idents.[PD_NilSymbol]
	  symbol = { symb_ident = pds_ident, symb_kind = SK_Constructor { glob_module = pds_module, glob_object = pds_def} }
	= (symbol, predefs)

addDclTypeFunctions :: !Int !*{#DclModule} !*PredefinedSymbols !*VarHeap !*SymbolTable
						-> (!*{#DclModule},!*PredefinedSymbols,!*VarHeap,!*SymbolTable)
addDclTypeFunctions nr_cached_dcls dcl_modules predefs var_heap symbols
	# (ctListDefSymb, predefs) = getListTypeSymb predefs
	# (dcl_modules, var_heap, symbols)
		=	add_dcl_type_fun_types ctListDefSymb nr_cached_dcls dcl_modules var_heap symbols
	= (dcl_modules, predefs, var_heap, symbols)

addIclTypeFunctions :: !Int !Int !*{#FunDef} !*{#CheckedTypeDef} !*{#ClassDef} !*PredefinedSymbols !*VarHeap !*SymbolTable
				 -> (!IndexRange,!*{#FunDef},!*{#CheckedTypeDef},!*{#ClassDef},!*PredefinedSymbols,!*VarHeap,!*SymbolTable)
addIclTypeFunctions n_dcl_type_defs n_dcl_class_defs icl_functions icl_type_defs icl_class_defs predefs var_heap symbol_table
	# (ctListDefSymb, predefs) = getListTypeSymb predefs
	  (n_functions_before, icl_functions) = usize icl_functions

	# (type_fun_index,rev_type_funs,icl_type_defs,var_heap,symbol_table)
		= add_td_funs_for_exported_types 0 n_functions_before ctListDefSymb n_dcl_type_defs [] icl_type_defs var_heap symbol_table
	  (type_fun_index,rev_type_funs,icl_class_defs,var_heap,symbol_table)
		= add_td_funs_for_exported_classes 0 type_fun_index ctListDefSymb n_dcl_class_defs rev_type_funs icl_class_defs var_heap symbol_table
	  (type_fun_index,rev_type_funs,icl_type_defs,var_heap,symbol_table)
		= add_td_funs_for_not_exported_types (n_dcl_type_defs+n_dcl_class_defs) type_fun_index ctListDefSymb rev_type_funs icl_type_defs var_heap symbol_table
	  (type_fun_index,rev_type_funs,icl_class_defs,var_heap,symbol_table)
		= add_td_funs_for_not_exported_classes n_dcl_class_defs type_fun_index ctListDefSymb rev_type_funs icl_class_defs var_heap symbol_table				

	  icl_functions = {function \\ function <- [e \\ e <-: icl_functions] ++ reverse rev_type_funs}
	  (n_functions_after, icl_functions) = usize icl_functions
	  type_fun_range = {ir_from=n_functions_before,ir_to=n_functions_after}
	= (type_fun_range,icl_functions,icl_type_defs,icl_class_defs,predefs,var_heap,symbol_table)
where
	add_td_funs_for_exported_types :: Int Int TypeSymbIdent Int [FunDef]  *{#CheckedTypeDef}  *VarHeap  *SymbolTable
													  -> (!Int,![FunDef],!*{#CheckedTypeDef},!*VarHeap,!*SymbolTable)
	add_td_funs_for_exported_types dcl_type_index type_fun_index ct_type_def n_dcl_type_defs rev_type_fun_defs icl_type_defs var_heap symbol_table
		| dcl_type_index<n_dcl_type_defs
			# icl_type_index = dcl_type_index
			  (type_def,icl_type_defs) = icl_type_defs![icl_type_index]
			  (type_fun_def, var_heap, symbol_table)
				= add_td_fun_def type_fun_index type_def.td_ident.id_name type_def.td_pos ct_type_def var_heap symbol_table
			  icl_type_defs = {icl_type_defs & [icl_type_index].td_fun_index = type_fun_index}
			  rev_type_fun_defs = [type_fun_def : rev_type_fun_defs]
			= add_td_funs_for_exported_types (dcl_type_index+1) (type_fun_index+1) ct_type_def n_dcl_type_defs rev_type_fun_defs icl_type_defs var_heap symbol_table
			= (type_fun_index,rev_type_fun_defs,icl_type_defs,var_heap,symbol_table)

	add_td_funs_for_exported_classes :: Int Int TypeSymbIdent Int [FunDef]  *{#ClassDef}  *VarHeap  *SymbolTable
														-> (!Int,![FunDef],!*{#ClassDef},!*VarHeap,!*SymbolTable)
	add_td_funs_for_exported_classes dcl_class_index type_fun_index ct_type_def n_dcl_class_defs rev_type_fun_defs icl_class_defs var_heap symbol_table
		| dcl_class_index<n_dcl_class_defs
			# icl_type_index = dcl_class_index
			  (class_def,icl_class_defs) = icl_class_defs![icl_type_index]
			  (type_fun_def, var_heap, symbol_table)
				= add_td_fun_def type_fun_index (class_def.class_ident.id_name+++";") class_def.class_pos ct_type_def var_heap symbol_table
			  rev_type_fun_defs = [type_fun_def : rev_type_fun_defs]
			= add_td_funs_for_exported_classes (dcl_class_index+1) (type_fun_index+1) ct_type_def n_dcl_class_defs rev_type_fun_defs icl_class_defs var_heap symbol_table
			= (type_fun_index,rev_type_fun_defs,icl_class_defs,var_heap,symbol_table)

	add_td_funs_for_not_exported_types :: Int Int TypeSymbIdent [FunDef] *{#CheckedTypeDef}  *VarHeap  *SymbolTable
													 -> (!Int,![FunDef],!*{#CheckedTypeDef},!*VarHeap,!*SymbolTable)
	add_td_funs_for_not_exported_types icl_type_index type_fun_index ct_type_def rev_type_fun_defs icl_type_defs var_heap symbol_table
		| icl_type_index<size icl_type_defs
			# (type_def,icl_type_defs) = icl_type_defs![icl_type_index]
			| type_def.td_fun_index==NoIndex
				# (type_fun_def, var_heap, symbol_table)
					= add_td_fun_def type_fun_index type_def.td_ident.id_name type_def.td_pos ct_type_def var_heap symbol_table
				  icl_type_defs = {icl_type_defs & [icl_type_index].td_fun_index = type_fun_index}
				  rev_type_fun_defs = [type_fun_def : rev_type_fun_defs]
				= add_td_funs_for_not_exported_types (icl_type_index+1) (type_fun_index+1) ct_type_def rev_type_fun_defs icl_type_defs var_heap symbol_table
				= add_td_funs_for_not_exported_types (icl_type_index+1) type_fun_index ct_type_def rev_type_fun_defs icl_type_defs var_heap symbol_table
			= (type_fun_index,rev_type_fun_defs,icl_type_defs,var_heap,symbol_table)

	add_td_funs_for_not_exported_classes :: Int Int TypeSymbIdent [FunDef]  *{#ClassDef}  *VarHeap  *SymbolTable
														-> (!Int,![FunDef],!*{#ClassDef},!*VarHeap,!*SymbolTable)
	add_td_funs_for_not_exported_classes icl_class_index type_fun_index ct_type_def rev_type_fun_defs icl_class_defs var_heap symbol_table
		| icl_class_index<size icl_class_defs
			# (class_def,icl_class_defs) = icl_class_defs![icl_class_index]
			# (type_fun_def, var_heap, symbol_table)
				= add_td_fun_def type_fun_index (class_def.class_ident.id_name+++";") class_def.class_pos ct_type_def var_heap symbol_table
			  rev_type_fun_defs = [type_fun_def : rev_type_fun_defs]
			= add_td_funs_for_not_exported_classes (icl_class_index+1) (type_fun_index+1) ct_type_def rev_type_fun_defs icl_class_defs var_heap symbol_table
			= (type_fun_index,rev_type_fun_defs,icl_class_defs,var_heap,symbol_table)

:: BuildTypeFunState =
	!{	bs_predefs :: !.PredefinedSymbols
	,	bs_type_heaps :: !.TypeHeaps
	,	bs_var_heap :: !.VarHeap
	}

:: Info = { ri_main :: !Int, ri_common_defs :: !{#CommonDefs} }

buildTypeFunctions :: !Int !*{#FunDef} !{#CommonDefs}
					 !*PredefinedSymbols !*VarHeap !*TypeHeaps
	-> (!*{#FunDef}, !*PredefinedSymbols,!*VarHeap,!*TypeHeaps)
buildTypeFunctions main icl_functions common_defs predefs var_heap type_heaps
	# bs_state =
		{	bs_predefs = predefs
		,	bs_var_heap = var_heap
		,	bs_type_heaps = type_heaps
		}
	# type_defs = common_defs.[main].com_type_defs
	# (type_funs, {bs_predefs,bs_var_heap,bs_type_heaps})
		=	build 0 (size type_defs) type_defs icl_functions bs_state 
	= (type_funs, bs_predefs, bs_var_heap, bs_type_heaps)
	where
		build i n type_defs functions bs_state
			| i < n
				# info = {ri_main = main, ri_common_defs = common_defs}
				# (functions, bs_state)
					=	buildTypeFunction type_defs.[i] functions info bs_state
				=	build (i+1) n type_defs functions bs_state
				=	(functions, bs_state)

buildTypeFunction :: CheckedTypeDef *{#FunDef} Info *BuildTypeFunState
	-> (*{#FunDef}, *BuildTypeFunState)
buildTypeFunction type_def=:{td_fun_index, td_args} functions info bs_state
	| td_fun_index == NoIndex
		=	(functions, bs_state)
	// otherwise
		# (new_info_ptr, bs_var_heap) = newPtr VI_Empty bs_state.bs_var_heap
		  bs_state & bs_var_heap=bs_var_heap
		  var_id = {id_name = "_x", id_info = nilPtr}
		# (symb_Nil, bs_predefs) = getNilSymb bs_state.bs_predefs
		  bs_state & bs_predefs = bs_predefs
		  rhs = App {app_symb = symb_Nil, app_args = [], app_info_ptr = nilPtr}
	 	# lhs_free_var = {fv_def_level = NotALevel, fv_ident = var_id, fv_info_ptr = new_info_ptr, fv_count = 0}
		# body = {tb_args = [lhs_free_var], tb_rhs = rhs}
		# functions = {functions & [td_fun_index].fun_body=TransformedBody body}
		= (functions, bs_state)

addTypeFunctionsA :: Int TypeSymbIdent *{#CheckedTypeDef} *VarHeap *SymbolTable
							  -> ([FunType], *{#CheckedTypeDef},*VarHeap,*SymbolTable)
addTypeFunctionsA type_fun_index ct_type_def type_defs var_heap symbol_table
	# (n, type_defs) = usize type_defs
	= add_td_funs_acc 0 n type_fun_index ct_type_def type_defs [] var_heap symbol_table
where
	add_td_funs_acc :: Int Int Int TypeSymbIdent *{#CheckedTypeDef} [FunType] *VarHeap *SymbolTable
										   -> ([FunType], *{#CheckedTypeDef}, *VarHeap,*SymbolTable)
	add_td_funs_acc i n type_fun_index ct_type_def type_defs rev_type_fun_defs var_heap symbol_table
		| i >= n
			= (reverse rev_type_fun_defs, type_defs, var_heap, symbol_table)
			# (type_def, type_defs) = type_defs![i]
			  (type_fun_def, var_heap, symbol_table)
				=	add_td_fun_def type_fun_index type_def.td_ident.id_name type_def.td_pos ct_type_def var_heap symbol_table
			  type_defs = {type_defs & [i].td_fun_index = type_fun_index}
			  rev_type_fun_defs = [type_fun_def : rev_type_fun_defs]
			= add_td_funs_acc (i+1) n (type_fun_index+1) ct_type_def type_defs rev_type_fun_defs var_heap symbol_table

add_td_fun_def :: Int {#Char} Position TypeSymbIdent *VarHeap  *SymbolTable
											 -> (!a,!*VarHeap,!*SymbolTable) | makeTypeFun a
add_td_fun_def type_fun_index type_name pos ct_type_def var_heap symbol_table
	#	entry =	{	ste_kind		= STE_Empty
				,	ste_index		= type_fun_index
				,	ste_def_level	= -1
				,	ste_previous	= EmptySymbolTableEntry
				,	ste_doc			= No
				}
	# (fun_ident, symbol_table)
		=	newPtr entry symbol_table
	# type_fun_ident = {id_name="TD;"+++type_name, id_info=fun_ident}

	# result_type = TA ct_type_def [{at_attribute = TA_None, at_type = TB BT_Bool}]

	# symbol_type =
			{	st_vars = []
			,	st_args = [{at_attribute= TA_None, at_type = TB BT_Bool}]
			,	st_args_strictness = NotStrict
			,	st_arity = 1
			,	st_result = {at_attribute = TA_None, at_type = result_type}
			,	st_context = []
			,	st_attr_vars = []
			,	st_attr_env = []
			}

	=	makeTypeFun type_fun_ident pos symbol_type var_heap symbol_table
