implementation module checkFunctionBodies

import StdEnv, compare_types
import syntax, typesupport, parse, checksupport, utilities, checktypes, transform, predef
import explicitimports, comparedefimp
from check import checkFunctions,checkDclMacros

cIsInExpressionList		:== True
cIsNotInExpressionList	:== False

cEndWithUpdate			:== True
cEndWithSelection		:== False

cCaseExplicit		:== True
cCaseNotExplicit	:== False

::	Dynamics		:== [ExprInfoPtr]

::	ExpressionState =
	{	es_expr_heap	:: !.ExpressionHeap
	,	es_var_heap			:: !.VarHeap
	,	es_type_heaps		:: !.TypeHeaps
	,	es_generic_heap		:: !.GenericHeap
	,	es_calls			:: ![FunCall]
	,	es_dynamics			:: ![ExprInfoPtr]
	,	es_fun_defs			:: !.{# FunDef}
	}

::	ExpressionInput =
	{	ei_expr_level	:: !Level
	,	ei_fun_index	:: !FunctionOrMacroIndex
	,	ei_fun_level	:: !Level
	,	ei_mod_index	:: !Index
	,	ei_local_functions_index_offset :: !Int
	}

::	PatternState =
	{	ps_var_heap :: !.VarHeap
	,	ps_fun_defs :: !.{#FunDef}
	}

::	PatternInput =
	{	pi_def_level		:: !Int
	,	pi_mod_index		:: !Index
	,	pi_is_node_pattern	:: !Bool
	}
	
::	ArrayPattern =
	{	ap_opt_var		:: !Optional (Bind Ident VarInfoPtr)
	,	ap_array_var	:: !FreeVar
	,	ap_selections	:: ![Bind FreeVar [ParsedExpr]]
	}

::	UnfoldMacroState =
	{	ums_var_heap	:: !.VarHeap
	,	ums_modules		:: !.{# DclModule}
	,	ums_cons_defs	:: !.{# ConsDef}
	,	ums_error		:: !.ErrorAdmin
	}

::	RecordKind = RK_Constructor | RK_Update

get_unboxed_list_indices_and_decons_u_ident :: *CheckState -> (!Index,!Index,!Index,!Index,!Ident,!*CheckState);
get_unboxed_list_indices_and_decons_u_ident cs=:{cs_predef_symbols,cs_x}
	# (stdStrictLists_index,cs_predef_symbols)=cs_predef_symbols![PD_StdStrictLists].pds_def
	# (cons_u_index,cs_predef_symbols)=cs_predef_symbols![PD_cons_u].pds_def
	# (nil_u_index,cs_predef_symbols)=cs_predef_symbols![PD_nil_u].pds_def
	# (decons_u_symbol,cs_predef_symbols)=cs_predef_symbols![PD_decons_u]
	# decons_u_index=decons_u_symbol.pds_def
	# cs={cs & cs_predef_symbols=cs_predef_symbols,cs_x.x_needed_modules=cs_x.x_needed_modules bitor cNeedStdStrictLists}
	= (stdStrictLists_index,cons_u_index,decons_u_index,nil_u_index,predefined_idents.[PD_decons_u],cs)

make_unboxed_list type_symbol expr_heap cs
	# (stdStrictLists_index,cons_u_index,decons_u_index,nil_u_index,decons_u_ident,cs) = get_unboxed_list_indices_and_decons_u_ident cs
	# unboxed_list=UnboxedList type_symbol stdStrictLists_index decons_u_index nil_u_index
	# (new_info_ptr,expr_heap) = newPtr EI_Empty expr_heap
	  app_symb = {symb_ident=decons_u_ident,symb_kind=SK_OverloadedFunction {glob_object=decons_u_index,glob_module=stdStrictLists_index}}
	# decons_expr = App {app_symb=app_symb,app_args=[],app_info_ptr=new_info_ptr}
	= (unboxed_list,decons_expr,expr_heap,cs)

get_unboxed_tail_strict_list_indices_and_decons_uts_ident :: *CheckState -> (!Index,!Index,!Index,!Index,!Ident,!*CheckState);
get_unboxed_tail_strict_list_indices_and_decons_uts_ident cs=:{cs_predef_symbols,cs_x}
	# (stdStrictLists_index,cs_predef_symbols)=cs_predef_symbols![PD_StdStrictLists].pds_def
	# (cons_uts_index,cs_predef_symbols)=cs_predef_symbols![PD_cons_uts].pds_def
	# (nil_uts_index,cs_predef_symbols)=cs_predef_symbols![PD_nil_uts].pds_def
	# (decons_uts_symbol,cs_predef_symbols)=cs_predef_symbols![PD_decons_uts]
	# decons_uts_index=decons_uts_symbol.pds_def
	# cs={cs & cs_predef_symbols=cs_predef_symbols,cs_x.x_needed_modules=cs_x.x_needed_modules bitor cNeedStdStrictLists}
	= (stdStrictLists_index,cons_uts_index,decons_uts_index,nil_uts_index,predefined_idents.[PD_decons_uts],cs)

make_unboxed_tail_strict_list type_symbol expr_heap cs
	# (stdStrictLists_index,cons_uts_index,decons_uts_index,nil_uts_index,decons_uts_ident,cs) = get_unboxed_tail_strict_list_indices_and_decons_uts_ident cs
	# unboxed_list=UnboxedTailStrictList type_symbol stdStrictLists_index decons_uts_index nil_uts_index
	# (new_info_ptr,expr_heap) = newPtr EI_Empty expr_heap
	  app_symb = {symb_ident=decons_uts_ident,symb_kind=SK_OverloadedFunction {glob_object=decons_uts_index,glob_module=stdStrictLists_index}}
	# decons_expr = App {app_symb=app_symb,app_args=[],app_info_ptr=new_info_ptr}
	= (unboxed_list,decons_expr,expr_heap,cs)

get_overloaded_list_indices_and_decons_ident :: *CheckState -> (!Index,!Index,!Index,!Index,!Ident,!*CheckState);
get_overloaded_list_indices_and_decons_ident cs=:{cs_predef_symbols,cs_x}
	# (stdStrictLists_index,cs_predef_symbols)=cs_predef_symbols![PD_StdStrictLists].pds_def
	# (cons_index,cs_predef_symbols)=cs_predef_symbols![PD_cons].pds_def
	# (nil_index,cs_predef_symbols)=cs_predef_symbols![PD_nil].pds_def
	# (decons_symbol,cs_predef_symbols)=cs_predef_symbols![PD_decons]
	# decons_index=decons_symbol.pds_def
	# cs={cs & cs_predef_symbols=cs_predef_symbols,cs_x.x_needed_modules=cs_x.x_needed_modules bitor cNeedStdStrictLists}
	= (stdStrictLists_index,cons_index,decons_index,nil_index,predefined_idents.[PD_decons],cs)

make_overloaded_list type_symbol expr_heap cs
	# (stdStrictLists_index,cons_index,decons_index,nil_index,decons_ident,cs) = get_overloaded_list_indices_and_decons_ident cs
	# overloaded_list=OverloadedList type_symbol stdStrictLists_index decons_index nil_index
	# (new_info_ptr,expr_heap) = newPtr EI_Empty expr_heap
	  app_symb = {symb_ident=decons_ident,symb_kind=SK_OverloadedFunction {glob_object=decons_index,glob_module=stdStrictLists_index}}
	# decons_expr = App {app_symb=app_symb,app_args=[],app_info_ptr=new_info_ptr}
	= (overloaded_list,decons_expr,expr_heap,cs)

make_case_guards cons_symbol global_type_index alg_patterns expr_heap cs
	| cons_symbol.glob_module==cPredefinedModuleIndex
		# pd_cons_index=cons_symbol.glob_object.ds_index+FirstConstructorPredefinedSymbolIndex
		| pd_cons_index==PD_UnboxedConsSymbol || pd_cons_index==PD_UnboxedNilSymbol
			# (unboxed_list,decons_expr,expr_heap,cs) = make_unboxed_list global_type_index expr_heap cs
			= (OverloadedListPatterns unboxed_list decons_expr alg_patterns,expr_heap,cs)
		| pd_cons_index==PD_UnboxedTailStrictConsSymbol || pd_cons_index==PD_UnboxedTailStrictNilSymbol
			# (unboxed_tail_strict_list,decons_expr,expr_heap,cs) = make_unboxed_tail_strict_list global_type_index expr_heap cs
			= (OverloadedListPatterns unboxed_tail_strict_list decons_expr alg_patterns,expr_heap,cs)
		| pd_cons_index==PD_OverloadedConsSymbol || pd_cons_index==PD_OverloadedNilSymbol
			# (overloaded_list,decons_expr,expr_heap,cs) = make_overloaded_list global_type_index expr_heap cs
			= (OverloadedListPatterns overloaded_list decons_expr alg_patterns,expr_heap,cs)
			= (AlgebraicPatterns global_type_index alg_patterns,expr_heap,cs)
		= (AlgebraicPatterns global_type_index alg_patterns,expr_heap,cs)

checkFunctionBodies :: !FunctionBody !Ident !.ExpressionInput !*ExpressionState !*ExpressionInfo !*CheckState
							   -> (!FunctionBody, ![FreeVar], !*ExpressionState,!*ExpressionInfo,!*CheckState)
checkFunctionBodies (ParsedBody [{pb_args,pb_rhs={rhs_alts,rhs_locals}, pb_position} : bodies]) function_ident_for_errors e_input=:{ei_expr_level,ei_mod_index}
		e_state=:{es_var_heap, es_fun_defs} e_info cs

	# (aux_patterns, (var_env, array_patterns), {ps_var_heap,ps_fun_defs}, e_info, cs)
			= check_patterns pb_args {pi_def_level = ei_expr_level, pi_mod_index = ei_mod_index, pi_is_node_pattern = False} ([], [])
							{ps_var_heap = es_var_heap, ps_fun_defs = es_fun_defs} e_info cs
	  (rhs_expr, free_vars, e_state, e_info, cs)
	  		= checkRhs [] rhs_alts rhs_locals e_input { e_state & es_var_heap = ps_var_heap, es_fun_defs = ps_fun_defs } e_info cs
	  (expr_with_array_selections, free_vars, e_state=:{es_var_heap,es_dynamics=dynamics_in_rhs}, e_info, cs)
			= addArraySelections array_patterns rhs_expr free_vars e_input e_state e_info cs
	  cs_symbol_table = removeLocalIdentsFromSymbolTable ei_expr_level var_env cs.cs_symbol_table
	  cs = { cs & cs_symbol_table = cs_symbol_table }
	  (cb_args, es_var_heap) = mapSt determine_function_arg aux_patterns es_var_heap
	  (rhss, free_vars, e_state=:{es_dynamics,es_expr_heap,es_var_heap}, e_info, cs)
	  		= check_function_bodies free_vars cb_args bodies e_input { e_state & es_dynamics = [], es_var_heap = es_var_heap } e_info cs
	  (rhs, position, es_var_heap, es_expr_heap, dynamics_in_patterns, cs)
	  		= transform_patterns_into_cases aux_patterns cb_args expr_with_array_selections pb_position es_var_heap es_expr_heap
	  										dynamics_in_rhs cs
	= (CheckedBody { cb_args = cb_args, cb_rhs = [{ ca_rhs = rhs, ca_position = position } : rhss] }, free_vars,
		{ e_state & es_var_heap = es_var_heap, es_expr_heap = es_expr_heap, es_dynamics = dynamics_in_patterns ++ es_dynamics }, e_info, cs)
where
	check_patterns [pattern : patterns] p_input accus var_store e_info cs
		# (aux_pat, accus, var_store, e_info, cs) = checkPattern pattern No p_input accus var_store e_info cs
		  (aux_pats, accus, var_store, e_info, cs) = check_patterns patterns p_input accus var_store e_info cs
		= ([aux_pat : aux_pats], accus, var_store, e_info, cs)
	check_patterns [] p_input accus var_store e_info cs
		= ([], accus, var_store, e_info, cs)

	determine_function_arg (AP_Variable name var_info (Yes {bind_src, bind_dst})) var_store
		= ({ fv_ident = bind_src, fv_info_ptr = bind_dst, fv_def_level = NotALevel, fv_count = 0 }, var_store)
	determine_function_arg (AP_Variable name var_info No) var_store
		= ({ fv_ident = name, fv_info_ptr = var_info, fv_def_level = NotALevel, fv_count = 0 }, var_store)
	determine_function_arg (AP_Algebraic _ _ _ opt_var) var_store
		# ({bind_src,bind_dst}, var_store) = determinePatternVariable opt_var var_store
		= ({ fv_ident = bind_src, fv_info_ptr = bind_dst, fv_def_level = NotALevel, fv_count = 0 }, var_store)
	determine_function_arg (AP_Basic _ opt_var) var_store
		# ({bind_src,bind_dst}, var_store) = determinePatternVariable opt_var var_store
		= ({ fv_ident = bind_src, fv_info_ptr = bind_dst, fv_def_level = NotALevel, fv_count = 0 }, var_store)
	determine_function_arg (AP_NewType _ _ _ opt_var) var_store
		# ({bind_src,bind_dst}, var_store) = determinePatternVariable opt_var var_store
		= ({ fv_ident = bind_src, fv_info_ptr = bind_dst, fv_def_level = NotALevel, fv_count = 0 }, var_store)
	determine_function_arg (AP_Dynamic _ _ opt_var) var_store
		# ({bind_src,bind_dst}, var_store) = determinePatternVariable opt_var var_store
		= ({ fv_ident = bind_src, fv_info_ptr = bind_dst, fv_def_level = NotALevel, fv_count = 0 }, var_store)
	determine_function_arg _ var_store
		# ({bind_src,bind_dst}, var_store) = determinePatternVariable No var_store
		= ({ fv_ident = bind_src, fv_info_ptr = bind_dst, fv_def_level = NotALevel, fv_count = 0 }, var_store)

	check_function_bodies free_vars fun_args [{pb_args,pb_rhs={rhs_alts,rhs_locals},pb_position} : bodies]
							e_input=:{ei_expr_level,ei_mod_index} e_state=:{es_var_heap,es_fun_defs} e_info cs
		# cs = pushErrorAdmin (newPosition function_ident_for_errors pb_position) cs
		# (aux_patterns, (var_env, array_patterns), {ps_var_heap,ps_fun_defs}, e_info, cs)
				= check_patterns pb_args { pi_def_level = ei_expr_level, pi_mod_index = ei_mod_index, pi_is_node_pattern = False } ([], [])
					{ps_var_heap = es_var_heap,ps_fun_defs = es_fun_defs} e_info cs
		# cs = popErrorAdmin cs
		  e_state = { e_state & es_var_heap = ps_var_heap,es_fun_defs = ps_fun_defs}
		  (rhs_expr, free_vars, e_state, e_info, cs) = checkRhs free_vars rhs_alts rhs_locals e_input e_state e_info cs
		  (rhs_expr, free_vars, e_state=:{es_dynamics=dynamics_in_rhs}, e_info, cs)
				= addArraySelections array_patterns rhs_expr free_vars e_input e_state e_info cs
	 	  cs_symbol_table = removeLocalIdentsFromSymbolTable ei_expr_level var_env cs.cs_symbol_table
		  (rhs_exprs, free_vars, e_state=:{es_dynamics,es_expr_heap,es_var_heap}, e_info, cs)
		  		= check_function_bodies free_vars fun_args bodies e_input { e_state & es_dynamics = [] } e_info { cs & cs_symbol_table = cs_symbol_table }
		  (rhs_expr, position, es_var_heap, es_expr_heap, dynamics_in_patterns, cs)
		  		= transform_patterns_into_cases aux_patterns fun_args rhs_expr pb_position
		  										 es_var_heap es_expr_heap dynamics_in_rhs cs
		= ([{ ca_rhs = rhs_expr, ca_position = position } : rhs_exprs], free_vars,
			{ e_state & es_var_heap = es_var_heap, es_expr_heap = es_expr_heap,
						es_dynamics = dynamics_in_patterns ++ es_dynamics }, e_info, cs)
	check_function_bodies free_vars fun_args [] e_input e_state e_info cs
		= ([], free_vars, e_state, e_info, cs) 
		
	transform_patterns_into_cases [pattern : patterns] [fun_arg : fun_args] result_expr pattern_position
									var_store expr_heap opt_dynamics cs
		# (patterns_expr, pattern_position, var_store, expr_heap, opt_dynamics, cs)
				= transform_succeeding_patterns_into_cases patterns fun_args result_expr pattern_position
															var_store expr_heap opt_dynamics cs
		= transform_pattern_into_cases pattern fun_arg patterns_expr pattern_position var_store expr_heap opt_dynamics cs
	where
		transform_succeeding_patterns_into_cases [] _ result_expr pattern_position var_store expr_heap opt_dynamics cs
			= (result_expr, pattern_position, var_store, expr_heap, opt_dynamics, cs)
		transform_succeeding_patterns_into_cases [pattern : patterns] [fun_arg : fun_args] result_expr pattern_position
												var_store expr_heap opt_dynamics cs
			# (patterns_expr, pattern_position, var_store, expr_heap, opt_dynamics, cs)
				= transform_succeeding_patterns_into_cases patterns fun_args result_expr pattern_position
															var_store expr_heap opt_dynamics cs
			= transform_pattern_into_cases pattern fun_arg patterns_expr pattern_position var_store expr_heap opt_dynamics cs

	transform_patterns_into_cases [] _ result_expr pattern_position var_store expr_heap opt_dynamics cs
		= (result_expr, pattern_position, var_store, expr_heap, opt_dynamics, cs)

	transform_pattern_into_cases :: !AuxiliaryPattern !FreeVar !Expression !Position !*VarHeap !*ExpressionHeap ![DynamicPtr] !*CheckState
		-> (!Expression, !Position, !*VarHeap, !*ExpressionHeap, ![DynamicPtr], !*CheckState)
	transform_pattern_into_cases (AP_Variable name var_info opt_var) fun_arg=:{fv_info_ptr,fv_ident} result_expr pattern_position
									var_store expr_heap opt_dynamics cs
		= case opt_var of
			Yes {bind_src, bind_dst}
				| bind_dst == fv_info_ptr
					# (var_expr_ptr, expr_heap) = newPtr EI_Empty expr_heap
					  (let_expr_ptr, expr_heap) = newPtr EI_Empty expr_heap
					-> (Let { let_strict_binds = [], let_lazy_binds= [
								{ lb_src = Var { var_ident = fv_ident, var_info_ptr = fv_info_ptr, var_expr_ptr = var_expr_ptr },
									lb_dst = { fv_ident = name, fv_info_ptr = var_info, fv_def_level = NotALevel, fv_count = 0 },
									lb_position = NoPos }],
							  let_expr = result_expr, let_info_ptr = let_expr_ptr, let_expr_position = NoPos }, 
						pattern_position, var_store, expr_heap, opt_dynamics, cs)
					# (var_expr_ptr1, expr_heap) = newPtr EI_Empty expr_heap
					  (var_expr_ptr2, expr_heap) = newPtr EI_Empty expr_heap
					  (let_expr_ptr, expr_heap) = newPtr EI_Empty expr_heap
					-> (Let { let_strict_binds = [], let_lazy_binds= [
								{ lb_src = Var { var_ident = fv_ident, var_info_ptr = fv_info_ptr, var_expr_ptr = var_expr_ptr1 },
									lb_dst = { fv_ident = name, fv_info_ptr = var_info, fv_def_level = NotALevel, fv_count = 0 },
									lb_position = NoPos },
								{ lb_src = Var { var_ident = fv_ident, var_info_ptr = fv_info_ptr, var_expr_ptr = var_expr_ptr2 },
									lb_dst = { fv_ident = bind_src, fv_info_ptr = bind_dst, fv_def_level = NotALevel, fv_count = 0 },
									lb_position = NoPos }],
							  let_expr = result_expr, let_info_ptr = let_expr_ptr, let_expr_position = NoPos }, 
						pattern_position, var_store, expr_heap, opt_dynamics, cs)
			No
				| var_info == fv_info_ptr
					-> (result_expr, pattern_position, var_store, expr_heap, opt_dynamics, cs)
					# (var_expr_ptr, expr_heap) = newPtr EI_Empty expr_heap
					  (let_expr_ptr, expr_heap) = newPtr EI_Empty expr_heap
					-> (Let { let_strict_binds = [], let_lazy_binds=
									[{ lb_src = Var { var_ident = fv_ident, var_info_ptr = fv_info_ptr, var_expr_ptr = var_expr_ptr },
										 lb_dst = { fv_ident = name, fv_info_ptr = var_info, fv_def_level = NotALevel, fv_count = 0 },
										 lb_position = NoPos }],
							  let_expr = result_expr, let_info_ptr = let_expr_ptr, let_expr_position = NoPos },
						pattern_position, var_store, expr_heap, opt_dynamics, cs)

	transform_pattern_into_cases (AP_Algebraic cons_symbol global_type_index args opt_var) fun_arg result_expr pattern_position
									var_store expr_heap opt_dynamics cs
		# (var_args, result_expr, pattern_position, var_store, expr_heap, opt_dynamics, cs)
				= convertSubPatterns args result_expr pattern_position var_store expr_heap opt_dynamics cs
	  	  (act_var, result_expr, expr_heap) = transform_pattern_variable fun_arg opt_var result_expr expr_heap
		  (case_expr_ptr, expr_heap) = newPtr EI_Empty expr_heap
		# alg_patterns = [{ ap_symbol = cons_symbol, ap_vars = var_args, ap_expr = result_expr, ap_position = pattern_position }]
		# (case_guards,expr_heap,cs) = make_case_guards cons_symbol global_type_index alg_patterns expr_heap cs
		= (Case { case_expr = act_var, case_guards = case_guards, case_default = No, case_ident = No,
				case_explicit = cCaseNotExplicit,
				case_info_ptr = case_expr_ptr, case_default_pos = NoPos },
				NoPos, var_store, expr_heap, opt_dynamics, cs)	
	transform_pattern_into_cases (AP_Basic basic_val opt_var) fun_arg result_expr pattern_position var_store expr_heap opt_dynamics cs
		# (basic_type, cs) = typeOfBasicValue basic_val cs
	  	  (act_var, result_expr, expr_heap) = transform_pattern_variable fun_arg opt_var result_expr expr_heap
		  case_guards = BasicPatterns basic_type [{ bp_value = basic_val, bp_expr = result_expr, bp_position = pattern_position }]
		  (case_expr_ptr, expr_heap) = newPtr EI_Empty expr_heap
		= (Case {	case_expr = act_var, case_guards = case_guards, case_default = No, case_ident = No,
					case_explicit = cCaseNotExplicit,
					case_info_ptr = case_expr_ptr, case_default_pos = NoPos },
			NoPos, var_store, expr_heap, opt_dynamics, cs)
	transform_pattern_into_cases (AP_NewType cons_symbol type_index arg opt_var) fun_arg result_expr pattern_position
									var_store expr_heap opt_dynamics cs
		# (var_arg, result_expr, pattern_position, var_store, expr_heap, opt_dynamics, cs)
				= convertSubPattern arg result_expr pattern_position var_store expr_heap opt_dynamics cs
		  type_symbol = {gi_module = cons_symbol.glob_module, gi_index = type_index}
	  	  (act_var, result_expr, expr_heap) = transform_pattern_variable fun_arg opt_var result_expr expr_heap
		  (case_expr_ptr, expr_heap) = newPtr EI_Empty expr_heap
		# alg_patterns = [{ ap_symbol = cons_symbol, ap_vars = [var_arg], ap_expr = result_expr, ap_position = pattern_position }]
		# case_guards = NewTypePatterns type_symbol alg_patterns
		= (Case { case_expr = act_var, case_guards = case_guards, case_default = No, case_ident = No,
				  case_explicit = cCaseNotExplicit, case_info_ptr = case_expr_ptr, case_default_pos = NoPos },
			NoPos, var_store, expr_heap, opt_dynamics, cs)
	transform_pattern_into_cases (AP_Dynamic pattern type opt_var) fun_arg result_expr pattern_position var_store expr_heap opt_dynamics cs
		# (var_arg, result_expr, pattern_position, var_store, expr_heap, opt_dynamics, cs)
				= convertSubPattern pattern result_expr pattern_position var_store expr_heap opt_dynamics cs
		  (type_case_info_ptr, expr_heap) = newPtr EI_Empty expr_heap
		  (dynamic_info_ptr, expr_heap) = newPtr (EI_DynamicType type opt_dynamics) expr_heap
	  	  (act_var, result_expr, expr_heap) = transform_pattern_variable fun_arg opt_var result_expr expr_heap
	  	  type_case_patterns = [{ dp_var = var_arg, dp_type	= dynamic_info_ptr,	dp_rhs = result_expr,
	  	  							dp_type_code = TCE_Empty, dp_position = pattern_position }]
		= (buildTypeCase act_var type_case_patterns No type_case_info_ptr cCaseNotExplicit, NoPos, var_store, expr_heap, [dynamic_info_ptr], cs)	
	transform_pattern_into_cases (AP_WildCard _) fun_arg result_expr pattern_position var_store expr_heap opt_dynamics cs
		= (result_expr, pattern_position, var_store, expr_heap, opt_dynamics, cs)	
	transform_pattern_into_cases AP_Empty fun_arg result_expr pattern_position var_store expr_heap opt_dynamics cs
		= (EE, pattern_position, var_store, expr_heap, opt_dynamics, cs)

	transform_pattern_variable :: !FreeVar !(Optional (Bind Ident VarInfoPtr)) !Expression !*ExpressionHeap
		-> (!Expression, !Expression, !*ExpressionHeap)
	transform_pattern_variable {fv_info_ptr,fv_ident} (Yes {bind_src,bind_dst}) result_expr expr_heap
		| bind_dst == fv_info_ptr
			# (var_expr_ptr, expr_heap) = newPtr EI_Empty expr_heap
			= (Var { var_ident = fv_ident, var_info_ptr = fv_info_ptr, var_expr_ptr = var_expr_ptr }, result_expr, expr_heap)
			# (var_expr_ptr1, expr_heap) = newPtr EI_Empty expr_heap
			  (var_expr_ptr2, expr_heap) = newPtr EI_Empty expr_heap
			  (let_expr_ptr, expr_heap) = newPtr EI_Empty expr_heap
			= (Var { var_ident = fv_ident, var_info_ptr = fv_info_ptr, var_expr_ptr = var_expr_ptr1 },
						Let { let_strict_binds = [], let_lazy_binds =
						 		[{ lb_src = Var { var_ident = fv_ident, var_info_ptr = fv_info_ptr, var_expr_ptr = var_expr_ptr2 },
									lb_dst = { fv_ident = bind_src, fv_info_ptr = bind_dst, fv_def_level = NotALevel, fv_count = 0 },
									lb_position = NoPos }],
							  let_expr = result_expr, let_info_ptr = let_expr_ptr, let_expr_position = NoPos }, expr_heap)
	transform_pattern_variable {fv_info_ptr,fv_ident} No result_expr expr_heap
		# (var_expr_ptr, expr_heap) = newPtr EI_Empty expr_heap
		= (Var { var_ident = fv_ident, var_info_ptr = fv_info_ptr, var_expr_ptr = var_expr_ptr }, result_expr, expr_heap)

checkFunctionBodies  GeneratedBody function_ident_for_errors e_input e_state e_info cs
	= (GeneratedBody, [], e_state, e_info, cs)
		//---> ("checkFunctionBodies: function to derive ", function_ident_for_errors)
checkFunctionBodies  _ function_ident_for_errors e_input=:{ei_expr_level,ei_mod_index} e_state=:{es_var_heap, es_fun_defs} e_info cs
	= abort ("checkFunctionBodies " +++ toString function_ident_for_errors +++ "\n")

removeLocalsFromSymbolTable :: !Index !Level ![Ident] !LocalDefs !Int !*{#FunDef} !*{#*{#FunDef}} !*(Heap SymbolTableEntry)
																  -> (!.{#FunDef},!.{#.{#FunDef}},!.Heap SymbolTableEntry)
removeLocalsFromSymbolTable module_index level loc_vars (CollectedLocalDefs {loc_functions,loc_in_icl_module}) local_functions_index_offset fun_defs macro_defs symbol_table	
	# loc_functions={ir_from=loc_functions.ir_from+local_functions_index_offset,ir_to=loc_functions.ir_to+local_functions_index_offset}
	# symbol_table=removeLocalIdentsFromSymbolTable level loc_vars symbol_table
	| loc_in_icl_module
		# (fun_defs,symbol_table) = removeLocalFunctionsFromSymbolTable level loc_functions fun_defs symbol_table
		= (fun_defs,macro_defs,symbol_table)
		# (macro_defs,symbol_table) = removeLocalDclMacrosFromSymbolTable level module_index loc_functions macro_defs symbol_table
		= (fun_defs,macro_defs,symbol_table)

:: LetBinds :== [([LetBind],[LetBind])]

checkRhs :: [FreeVar] OptGuardedAlts LocalDefs ExpressionInput *ExpressionState  *ExpressionInfo  *CheckState
								  -> *(!Expression,![FreeVar],!*ExpressionState,!*ExpressionInfo,!*CheckState);
checkRhs free_vars rhs_alts rhs_locals e_input=:{ei_expr_level,ei_mod_index,ei_local_functions_index_offset} e_state e_info cs
	# ei_expr_level = inc ei_expr_level
	  (loc_defs, (var_env, array_patterns), e_state, e_info, cs) = checkLhssOfLocalDefs ei_expr_level ei_mod_index rhs_locals ei_local_functions_index_offset e_state e_info cs
	  (es_fun_defs, e_info, heaps, cs)
	  		= checkLocalFunctions ei_mod_index ei_expr_level rhs_locals ei_local_functions_index_offset e_state.es_fun_defs e_info
	  			{ hp_var_heap = e_state.es_var_heap, hp_expression_heap = e_state.es_expr_heap, hp_type_heaps = e_state.es_type_heaps, hp_generic_heap = e_state.es_generic_heap } cs
	  (rhs_expr, _, free_vars, e_state, e_info, cs) 
	  		= check_opt_guarded_alts free_vars rhs_alts { e_input & ei_expr_level = ei_expr_level }
	  			{ e_state & es_fun_defs = es_fun_defs, es_var_heap = heaps.hp_var_heap, es_expr_heap = heaps.hp_expression_heap,
					es_type_heaps = heaps.hp_type_heaps,es_generic_heap=heaps.hp_generic_heap } e_info cs
	  (expr, free_vars, e_state, e_info, cs)
			= addArraySelections array_patterns rhs_expr free_vars e_input e_state e_info cs
	  (expr, free_vars, e_state, e_info, cs) = checkRhssAndTransformLocalDefs free_vars loc_defs expr e_input e_state e_info cs
	  (es_fun_defs,macro_defs,cs_symbol_table) = removeLocalsFromSymbolTable ei_mod_index ei_expr_level var_env rhs_locals ei_local_functions_index_offset e_state.es_fun_defs e_info.ef_macro_defs cs.cs_symbol_table
	= (expr, free_vars, { e_state & es_fun_defs = es_fun_defs}, {e_info & ef_macro_defs=macro_defs}, { cs & cs_symbol_table = cs_symbol_table })
where
	check_opt_guarded_alts free_vars (GuardedAlts guarded_alts default_expr) e_input e_state e_info cs
		# (let_vars_list, rev_guarded_exprs, last_expr_level, free_vars, e_state, e_info, cs)
				= check_guarded_expressions free_vars guarded_alts [] [] e_input e_state e_info cs
		  (default_expr, default_expr_position, free_vars, e_state, e_info, cs)
		  		= check_default_expr free_vars default_expr { e_input & ei_expr_level = last_expr_level } e_state e_info cs
		  cs = { cs & cs_symbol_table = remove_seq_let_vars e_input.ei_expr_level let_vars_list cs.cs_symbol_table }
	  	  (result_expr, result_expr_position , es_expr_heap) = convert_guards_to_cases rev_guarded_exprs default_expr default_expr_position e_state.es_expr_heap
	  	= (result_expr, result_expr_position, free_vars, { e_state & es_expr_heap = es_expr_heap }, e_info, cs)
	check_opt_guarded_alts free_vars (UnGuardedExpr unguarded_expr) e_input e_state e_info cs
		= check_unguarded_expression free_vars unguarded_expr e_input e_state e_info cs

	check_default_expr free_vars (Yes default_expr) e_input e_state e_info cs
		# (expr, expr_position, free_vars, e_state, e_info, cs) = check_unguarded_expression free_vars default_expr e_input e_state e_info cs
		= (Yes expr, expr_position, free_vars, e_state, e_info, cs)
	check_default_expr free_vars No e_input e_state e_info cs
		= (No, NoPos, free_vars, e_state, e_info, cs)

	convert_guards_to_cases [guard_expr] result_expr result_expr_position es_expr_heap
		= convert_guard_to_case guard_expr result_expr result_expr_position es_expr_heap
	convert_guards_to_cases [guard_expr : rev_guarded_exprs] result_expr result_expr_position es_expr_heap
		# (result_expr, result_expr_position, es_expr_heap) = convert_guard_to_case guard_expr result_expr result_expr_position es_expr_heap
		= convert_guards_to_cases rev_guarded_exprs (Yes result_expr) result_expr_position es_expr_heap

	convert_guard_to_case (let_binds, guard, expr, expr_position, guard_ident) result_expr result_expr_position es_expr_heap
		# (case_expr_ptr, es_expr_heap) = newPtr EI_Empty es_expr_heap
		  basic_pattern = {bp_value = (BVB True), bp_expr = expr, bp_position = expr_position }
		  case_expr = Case {case_expr = guard, case_guards = BasicPatterns BT_Bool [basic_pattern],
							case_default = result_expr, case_default_pos = result_expr_position,
							case_ident = Yes guard_ident, case_explicit = cCaseNotExplicit, case_info_ptr = case_expr_ptr }
		= build_sequential_lets let_binds case_expr NoPos es_expr_heap
	
	check_guarded_expressions :: [FreeVar] [GuardedExpr] [[Ident]] [(LetBinds,Expression,Expression,Position,Ident)] ExpressionInput *ExpressionState *ExpressionInfo *CheckState
													-> *([[Ident]],[(LetBinds,Expression,Expression,Position,Ident)],Int,[FreeVar],  *ExpressionState,*ExpressionInfo,*CheckState)
	check_guarded_expressions free_vars [gexpr : gexprs] let_vars_list rev_guarded_exprs e_input e_state e_info cs
		# (let_vars_list, rev_guarded_exprs, ei_expr_level, free_vars, e_state, e_info, cs)
				= check_guarded_expression free_vars gexpr let_vars_list rev_guarded_exprs e_input e_state e_info cs
		= check_guarded_expressions free_vars gexprs let_vars_list rev_guarded_exprs { e_input & ei_expr_level = ei_expr_level } e_state e_info cs
	check_guarded_expressions free_vars [] let_vars_list rev_guarded_exprs {ei_expr_level} e_state e_info cs
		= (let_vars_list, rev_guarded_exprs, ei_expr_level, free_vars, e_state, e_info, cs)

	check_guarded_expression free_vars {alt_nodes,alt_guard,alt_expr,alt_ident,alt_position}
			let_vars_list rev_guarded_exprs e_input=:{ei_expr_level,ei_mod_index} e_state e_info cs
		# (let_binds, let_vars_list, ei_expr_level, free_vars, e_state, e_info, cs) = check_sequential_lets free_vars alt_nodes let_vars_list
		  		{ e_input & ei_expr_level = inc ei_expr_level } e_state e_info cs
		  e_input = { e_input & ei_expr_level = ei_expr_level }
		  cs = pushErrorAdmin2 "guard" alt_position cs
	  	  (guard, free_vars, e_state, e_info, cs) = checkExpression free_vars alt_guard e_input e_state e_info cs
		  cs = popErrorAdmin cs
		  (expr, expr_position, free_vars, e_state, e_info, cs) = check_opt_guarded_alts free_vars alt_expr e_input e_state e_info cs
	  	= (let_vars_list, [(let_binds, guard, expr, expr_position, alt_ident) : rev_guarded_exprs], ei_expr_level, free_vars, e_state, e_info,  cs )

	check_unguarded_expression :: [FreeVar] ExprWithLocalDefs ExpressionInput *ExpressionState *ExpressionInfo *CheckState -> *(!Expression,!Position,![FreeVar],!*ExpressionState,!*ExpressionInfo,!*CheckState);
	check_unguarded_expression free_vars {ewl_nodes,ewl_expr,ewl_locals,ewl_position} e_input=:{ei_expr_level,ei_mod_index,ei_local_functions_index_offset} e_state e_info cs
		# this_expr_level = inc ei_expr_level
		  (loc_defs, (var_env, array_patterns), e_state, e_info, cs)
		 		= checkLhssOfLocalDefs this_expr_level ei_mod_index ewl_locals ei_local_functions_index_offset e_state e_info cs
		  (binds, let_vars_list, rhs_expr_level, free_vars, e_state, e_info, cs) = check_sequential_lets free_vars ewl_nodes [] { e_input & ei_expr_level = this_expr_level } e_state e_info cs
		  cs = pushErrorAdmin2 "" ewl_position cs
	  	  (expr, free_vars, e_state, e_info, cs) = checkExpression free_vars ewl_expr { e_input & ei_expr_level = rhs_expr_level } e_state e_info cs
		  cs = popErrorAdmin cs
		  (expr, free_vars, e_state, e_info, cs)
				= addArraySelections array_patterns expr free_vars e_input e_state e_info cs
		  cs = { cs & cs_symbol_table = remove_seq_let_vars rhs_expr_level let_vars_list cs.cs_symbol_table }
		  (seq_let_expr, expr_position, es_expr_heap) = build_sequential_lets binds expr ewl_position e_state.es_expr_heap
	  	  (expr, free_vars, e_state, e_info, cs)
				= checkRhssAndTransformLocalDefs free_vars loc_defs seq_let_expr e_input { e_state & es_expr_heap = es_expr_heap} e_info cs
	  	  (es_fun_defs, e_info, heaps, cs)
	  	  		= checkLocalFunctions ei_mod_index rhs_expr_level ewl_locals ei_local_functions_index_offset e_state.es_fun_defs e_info 
	  	  		{ hp_var_heap = e_state.es_var_heap, hp_expression_heap = e_state.es_expr_heap, hp_type_heaps = e_state.es_type_heaps,hp_generic_heap=e_state.es_generic_heap } cs
		  (es_fun_defs,macro_defs,cs_symbol_table) = removeLocalsFromSymbolTable ei_mod_index this_expr_level var_env ewl_locals ei_local_functions_index_offset es_fun_defs e_info.ef_macro_defs cs.cs_symbol_table
	  	= (expr, expr_position, free_vars, {e_state & es_fun_defs = es_fun_defs, es_var_heap = heaps.hp_var_heap,
	  			es_expr_heap = heaps.hp_expression_heap, es_type_heaps = heaps.hp_type_heaps, es_generic_heap=heaps.hp_generic_heap},
	  		{e_info & ef_macro_defs=macro_defs}, { cs & cs_symbol_table = cs_symbol_table} )
	
	remove_seq_let_vars level [] symbol_table
		= symbol_table
	remove_seq_let_vars level [let_vars : let_vars_list] symbol_table
		= remove_seq_let_vars (dec level) let_vars_list (removeLocalIdentsFromSymbolTable level let_vars symbol_table)
	
	check_sequential_lets :: [FreeVar] [NodeDefWithLocals] u:[[Ident]] !ExpressionInput *ExpressionState  *ExpressionInfo  *CheckState
										   -> *(!LetBinds,!u:[[Ident]],!Int,![FreeVar],!*ExpressionState,!*ExpressionInfo,!*CheckState);
	check_sequential_lets free_vars [seq_let:seq_lets] let_vars_list e_input=:{ei_expr_level,ei_mod_index} e_state e_info cs
		# ei_expr_level = inc ei_expr_level
		  e_input = { e_input & ei_expr_level = ei_expr_level }
		  (src_expr, pattern_expr, (let_vars, array_patterns), free_vars, e_state, e_info, cs)
		  		= check_sequential_let free_vars seq_let e_input e_state e_info cs
	      (binds, loc_envs, max_expr_level, free_vars, e_state, e_info, cs)
	      		= check_sequential_lets free_vars seq_lets [let_vars : let_vars_list] e_input e_state e_info cs
	    | seq_let.ndwl_strict
		    # (lazy_let_binds,strict_let_bind,es_var_heap, es_expr_heap, e_info, cs)
					= transfromPatternIntoStrictBind ei_mod_index ei_expr_level pattern_expr src_expr seq_let.ndwl_position
							e_state.es_var_heap e_state.es_expr_heap e_info cs
			  e_state = { e_state & es_var_heap = es_var_heap, es_expr_heap = es_expr_heap }
			  (strict_array_pattern_binds, lazy_array_pattern_binds, free_vars, e_state, e_info, cs)
					= buildArraySelections e_input array_patterns free_vars e_state e_info cs
			  all_binds = [ (strict_let_bind,lazy_let_binds), (strict_array_pattern_binds, lazy_array_pattern_binds) : binds]
		    = (all_binds, loc_envs, max_expr_level, free_vars, e_state, e_info, cs)
		    # (let_binds, es_var_heap, es_expr_heap, e_info, cs)
					= transfromPatternIntoBind ei_mod_index ei_expr_level pattern_expr src_expr seq_let.ndwl_position
							e_state.es_var_heap e_state.es_expr_heap e_info cs
			  e_state = { e_state & es_var_heap = es_var_heap, es_expr_heap = es_expr_heap }
			  (strict_array_pattern_binds, lazy_array_pattern_binds, free_vars, e_state, e_info, cs)
					= buildArraySelections e_input array_patterns free_vars e_state e_info cs
			  all_binds = [([],let_binds), (strict_array_pattern_binds, lazy_array_pattern_binds) : binds]
		    = (all_binds, loc_envs, max_expr_level, free_vars, e_state, e_info, cs)
	check_sequential_lets free_vars [] let_vars_list e_input=:{ei_expr_level} e_state e_info cs
		= ([], let_vars_list, ei_expr_level, free_vars, e_state, e_info, cs)

	check_sequential_let :: [FreeVar] NodeDefWithLocals ExpressionInput *ExpressionState *ExpressionInfo *CheckState -> *(!Expression,!AuxiliaryPattern,!(![Ident],![ArrayPattern]),![FreeVar],!*ExpressionState,!*ExpressionInfo,!*CheckState);
	check_sequential_let free_vars {ndwl_def={bind_src,bind_dst},ndwl_locals, ndwl_position} e_input=:{ei_expr_level,ei_mod_index,ei_local_functions_index_offset} e_state e_info cs
		# cs = pushErrorAdmin (newPosition {id_name="node definition", id_info=nilPtr} ndwl_position) cs
		  (loc_defs, (loc_env, loc_array_patterns), e_state, e_info, cs) = checkLhssOfLocalDefs ei_expr_level ei_mod_index ndwl_locals ei_local_functions_index_offset e_state e_info cs
		  (src_expr, free_vars, e_state, e_info, cs) = checkExpression free_vars bind_src e_input e_state e_info cs
		  (src_expr, free_vars, e_state, e_info, cs)
				= addArraySelections loc_array_patterns src_expr free_vars e_input e_state e_info cs
		  (src_expr, free_vars, e_state, e_info, cs) = checkRhssAndTransformLocalDefs free_vars loc_defs src_expr e_input e_state e_info cs
		  (es_fun_defs, e_info, {hp_var_heap,hp_expression_heap,hp_type_heaps,hp_generic_heap}, cs)
				= checkLocalFunctions ei_mod_index ei_expr_level ndwl_locals ei_local_functions_index_offset e_state.es_fun_defs e_info
	  				{ hp_var_heap = e_state.es_var_heap, hp_expression_heap = e_state.es_expr_heap, hp_type_heaps = e_state.es_type_heaps,hp_generic_heap=e_state.es_generic_heap} cs
	  	  (es_fun_defs,macro_defs,cs_symbol_table) = removeLocalsFromSymbolTable ei_mod_index ei_expr_level loc_env ndwl_locals ei_local_functions_index_offset es_fun_defs e_info.ef_macro_defs cs.cs_symbol_table
		  (pattern, accus, {ps_fun_defs,ps_var_heap}, e_info, cs)
				= checkPattern bind_dst No { pi_def_level = ei_expr_level, pi_mod_index = ei_mod_index, pi_is_node_pattern = True } ([], []) 
					{ps_var_heap = hp_var_heap,ps_fun_defs = es_fun_defs } {e_info & ef_macro_defs=macro_defs} { cs & cs_symbol_table = cs_symbol_table }
		  e_state = { e_state & es_var_heap = ps_var_heap, es_expr_heap = hp_expression_heap, es_type_heaps = hp_type_heaps,es_generic_heap=hp_generic_heap,es_fun_defs = ps_fun_defs }
		= (src_expr, pattern, accus, free_vars, e_state, e_info, popErrorAdmin cs)
	
	build_sequential_lets :: !LetBinds !Expression !Position !*ExpressionHeap -> (!Expression, !Position, !*ExpressionHeap)
	build_sequential_lets [] expr let_expr_position expr_heap
		= (expr, let_expr_position, expr_heap)
	build_sequential_lets [(strict_binds, lazy_binds) : seq_lets] expr let_expr_position expr_heap
		# (let_expr, let_expr_position, expr_heap) = build_sequential_lets seq_lets expr let_expr_position expr_heap
	  	  (let_expr, expr_heap) = buildLetExpression strict_binds lazy_binds let_expr let_expr_position expr_heap
		= ( let_expr, if (isEmpty strict_binds && isEmpty lazy_binds) let_expr_position NoPos, expr_heap)

checkLocalFunctions :: !Index !Level !LocalDefs !Int !*{#FunDef} !*ExpressionInfo !*Heaps !*CheckState
												 -> (!.{#FunDef},!.ExpressionInfo,!.Heaps,!.CheckState);
checkLocalFunctions mod_index level (CollectedLocalDefs {loc_functions={ir_from,ir_to},loc_in_icl_module}) local_functions_index_offset fun_defs e_info heaps cs
	# ir_from=ir_from+local_functions_index_offset
	# ir_to=ir_to+local_functions_index_offset
	| loc_in_icl_module
		= checkFunctions mod_index level ir_from ir_to local_functions_index_offset fun_defs e_info heaps cs
		# (e_info,heaps,cs) = checkDclMacros mod_index level ir_from ir_to e_info heaps cs
		= (fun_defs,e_info,heaps,cs)

checkExpression :: ![FreeVar] !ParsedExpr !ExpressionInput !*ExpressionState !*ExpressionInfo !*CheckState
							 -> *(!Expression, ![FreeVar], !*ExpressionState,!*ExpressionInfo,!*CheckState);
checkExpression free_vars (PE_List exprs) e_input e_state e_info cs	
	# (exprs, free_vars, e_state, e_info, cs) = check_expressions free_vars exprs e_input e_state e_info cs
	  (expr, e_state, cs_error) = build_expression exprs e_state cs.cs_error
	= (expr, free_vars, e_state, e_info, { cs & cs_error = cs_error })

where
	check_expressions free_vars [expr : exprs] e_input e_state e_info cs
		# (exprs, free_vars, e_state, e_info, cs) = check_expressions free_vars exprs e_input e_state e_info cs
		= case expr of
			PE_Ident id
				# (expr, free_vars, e_state, e_info, cs) = checkIdentExpression cIsInExpressionList free_vars id e_input e_state e_info cs
 				-> ([expr : exprs], free_vars, e_state, e_info, cs)
			PE_QualifiedIdent module_id ident_name
				# (expr, free_vars, e_state, e_info, cs) = checkQualifiedIdentExpression free_vars module_id ident_name cIsInExpressionList e_input e_state e_info cs
 				-> ([expr : exprs], free_vars, e_state, e_info, cs)
 			_
				# (expr, free_vars, e_state, e_info, cs) = checkExpression free_vars expr e_input e_state e_info cs
 				-> ([expr : exprs], free_vars, e_state, e_info, cs)
 	check_expressions free_vars [] e_input e_state e_info cs
		= ([], free_vars, e_state, e_info, cs)

	first_argument_of_infix_operator_missing
		= "first argument of infix operator missing"

	build_expression [Constant symb _ (Prio _ _) , _: _] e_state cs_error
		= (EE, e_state, checkError symb.symb_ident first_argument_of_infix_operator_missing cs_error)
	build_expression [Constant symb arity _] e_state cs_error
		= buildApplicationWithoutArguments symb e_state cs_error
	build_expression [expr] e_state cs_error
		= (expr, e_state, cs_error)
	build_expression [expr : exprs] e_state cs_error
		# (opt_opr, left, e_state, cs_error) = split_at_operator [expr] exprs e_state cs_error
		  (left_expr, e_state, cs_error) = combine_expressions left [] 0 e_state cs_error
		= case opt_opr of
			Yes (symb, arity, prio, right)
				-> case right of
					[Constant symb _ (Prio _ _):_]
						-> (EE, e_state, checkError symb.symb_ident first_argument_of_infix_operator_missing cs_error)
					_
						-> build_operator_expression [] left_expr (symb, arity, prio) right e_state cs_error
			No
				-> (left_expr, e_state, cs_error)
	where
		split_at_operator left [Constant symb arity NoPrio : exprs] e_state cs_error
			# (appl_exp, e_state, cs_error) = buildApplicationWithoutArguments symb e_state cs_error
			= split_at_operator [appl_exp : left] exprs e_state cs_error
		split_at_operator left [Constant symb arity (Prio _ _)] e_state cs_error
			= (No, left, e_state, checkError symb.symb_ident "second argument of infix operator missing" cs_error)
		split_at_operator left [Constant symb arity prio] e_state cs_error
			# (appl_exp, e_state, cs_error) = buildApplicationWithoutArguments symb e_state cs_error
			= (No, [appl_exp : left], e_state, cs_error)
		split_at_operator left [expr=:(Constant symb arity prio) : exprs] e_state cs_error
			= (Yes (symb, arity, prio, exprs), left, e_state, cs_error)
		split_at_operator left [expr : exprs] e_state cs_error
			= split_at_operator [expr : left] exprs e_state cs_error
		split_at_operator exp [] e_state cs_error
			= (No, exp, e_state, cs_error)

		combine_expressions [first_expr] args arity e_state cs_error
			= case first_expr of
				Constant symb form_arity _
					-> buildApplication symb form_arity arity args e_state cs_error
				_
					| arity == 0
						-> (first_expr, e_state, cs_error)
						-> (first_expr @ args, e_state, cs_error)
		combine_expressions [rev_arg : rev_args] args arity e_state cs_error
			= combine_expressions rev_args [rev_arg : args] (inc arity) e_state cs_error
		

 		build_operator_expression left_appls left1 (symb1, arity1, prio1) [re : res] e_state cs_error
			# (opt_opr, left2, e_state, cs_error) = split_at_operator [re] res e_state cs_error
			= case opt_opr of
				Yes (symb2, arity2, prio2, right)
					# optional_prio = determinePriority prio1 prio2
					-> case optional_prio of
						Yes priority
							| priority
						  		# (middle_exp, e_state, cs_error) = combine_expressions left2 [] 0 e_state cs_error
								  (new_left, e_state, cs_error) = buildApplication symb1 arity1 2 [left1,middle_exp] e_state cs_error
								  (left_appls, new_left, e_state, cs_error) = build_left_operand left_appls prio2 new_left e_state cs_error
								-> build_operator_expression left_appls new_left (symb2, arity2, prio2) right e_state cs_error
						  		# (middle_exp, e_state, cs_error) = combine_expressions left2 [] 0 e_state cs_error
								-> build_operator_expression [(symb1, arity1, prio1, left1) : left_appls]
										middle_exp (symb2, arity2, prio2) right e_state cs_error
						No
							-> (EE, e_state, checkError symb1.symb_ident "conflicting priorities" cs_error)
				No
					# (right, e_state, cs_error) = combine_expressions left2 [] 0 e_state cs_error
					  (result_expr, e_state, cs_error) = buildApplication symb1 arity1 2 [left1,right] e_state cs_error
					-> build_final_expression left_appls result_expr e_state cs_error

		build_left_operand [] _ result_expr e_state cs_error
			= ([], result_expr, e_state, cs_error)		
		build_left_operand la=:[(symb, arity, priol, left) : left_appls] prior result_expr e_state cs_error
			# optional_prio = determinePriority priol prior
			= case optional_prio of
				Yes priority
					| priority
						# (result_expr, e_state, cs_error) = buildApplication symb arity 2 [left,result_expr] e_state cs_error
						-> build_left_operand left_appls prior result_expr e_state cs_error
						-> (la, result_expr, e_state, cs_error)
				No
					-> (la, EE, e_state, checkError symb.symb_ident "conflicting priorities" cs_error)
		
		build_final_expression [] result_expr e_state cs_error
			= (result_expr, e_state, cs_error)		
		build_final_expression [(symb, arity, _, left) : left_appls] result_expr e_state cs_error
			# (result_expr, e_state, cs_error) = buildApplication symb arity 2 [left,result_expr] e_state cs_error
			= build_final_expression left_appls result_expr e_state cs_error
					
checkExpression free_vars (PE_Let let_locals expr) e_input=:{ei_expr_level,ei_mod_index,ei_local_functions_index_offset} e_state e_info cs
	# ei_expr_level = inc ei_expr_level
	  (loc_defs, (var_env, array_patterns), e_state, e_info, cs)
	  		= checkLhssOfLocalDefs ei_expr_level ei_mod_index let_locals ei_local_functions_index_offset e_state e_info cs
	  e_input = { e_input & ei_expr_level = ei_expr_level }
	  (let_expr, free_vars, e_state, e_info, cs) = checkExpression free_vars expr e_input e_state e_info cs
	  (expr, free_vars, e_state, e_info, cs)
			= addArraySelections array_patterns let_expr free_vars e_input e_state e_info cs
	  (expr, free_vars, e_state, e_info, cs) = checkRhssAndTransformLocalDefs free_vars loc_defs expr e_input e_state e_info cs
	  (es_fun_defs, e_info, heaps, cs)
			= checkLocalFunctions ei_mod_index ei_expr_level let_locals ei_local_functions_index_offset e_state.es_fun_defs e_info
	  			{ hp_var_heap = e_state.es_var_heap, hp_expression_heap = e_state.es_expr_heap, hp_type_heaps = e_state.es_type_heaps, hp_generic_heap = e_state.es_generic_heap } cs
	  (es_fun_defs,macro_defs,cs_symbol_table) = removeLocalsFromSymbolTable ei_mod_index ei_expr_level var_env let_locals ei_local_functions_index_offset es_fun_defs e_info.ef_macro_defs cs.cs_symbol_table
	= (expr, free_vars,
		{ e_state & es_fun_defs = es_fun_defs, es_var_heap = heaps.hp_var_heap, es_expr_heap = heaps.hp_expression_heap,
			es_type_heaps = heaps.hp_type_heaps,es_generic_heap = heaps.hp_generic_heap }, 
		{e_info & ef_macro_defs=macro_defs}, { cs & cs_symbol_table = cs_symbol_table })

checkExpression free_vars (PE_Case case_ident expr alts) e_input e_state e_info cs
	# (pattern_expr, free_vars, e_state, e_info, cs) = checkExpression free_vars expr e_input e_state e_info cs
	  (guards, _, pattern_variables, defaul, free_vars, e_state, e_info, cs)
	  		= check_case_alts free_vars alts [] case_ident.id_name e_input e_state e_info cs
	  (pattern_expr, binds, es_expr_heap) = bind_pattern_variables pattern_variables pattern_expr e_state.es_expr_heap
	  (case_expr, es_var_heap, es_expr_heap) = build_and_share_case guards defaul pattern_expr case_ident cCaseExplicit e_state.es_var_heap es_expr_heap
	  (result_expr, es_expr_heap) = buildLetExpression [] binds case_expr NoPos es_expr_heap
	= (result_expr, free_vars, { e_state & es_var_heap = es_var_heap,  es_expr_heap = es_expr_heap }, e_info, cs)
where
	check_case_alts free_vars [g] pattern_variables case_name e_input=:{ei_expr_level} e_state e_info cs
		# e_input = { e_input & ei_expr_level = inc ei_expr_level }
		= check_case_alt free_vars g NoPattern NoPattern pattern_variables No case_name e_input e_state e_info cs 
	check_case_alts free_vars [g : gs] pattern_variables case_name e_input=:{ei_expr_level} e_state e_info cs
		# e_input = { e_input & ei_expr_level = inc ei_expr_level }
		  (gs, pattern_scheme, pattern_variables, defaul, free_vars, e_state, e_info, cs)
		  	= check_case_alts free_vars gs pattern_variables case_name e_input e_state e_info cs
		= check_case_alt free_vars g gs pattern_scheme pattern_variables defaul case_name e_input e_state e_info cs 

	check_case_alt :: [FreeVar] CaseAlt CasePatterns CasePatterns [(Bind Ident (Ptr VarInfo))] (Optional ((Optional FreeVar),Expression)) {#Char} ExpressionInput *ExpressionState *ExpressionInfo *CheckState
												 -> *(CasePatterns,CasePatterns,[(Bind Ident (Ptr VarInfo))],(Optional ((Optional FreeVar),Expression)),[FreeVar],*ExpressionState,*ExpressionInfo,*CheckState)
	check_case_alt free_vars {calt_pattern,calt_rhs={rhs_alts,rhs_locals},calt_position} patterns pattern_scheme pattern_variables defaul case_name 
				e_input=:{ei_expr_level,ei_mod_index} e_state=:{es_fun_defs,es_var_heap,es_dynamics=outer_dynamics} e_info cs
		# (pattern, (var_env, array_patterns), {ps_fun_defs,ps_var_heap}, e_info, cs)
				= checkPattern calt_pattern No { pi_def_level = ei_expr_level, pi_mod_index = ei_mod_index, pi_is_node_pattern = False } ([], [])
					{ps_var_heap = es_var_heap,ps_fun_defs = es_fun_defs} e_info cs
		  e_state = { e_state & es_var_heap = ps_var_heap, es_fun_defs = ps_fun_defs, es_dynamics = [] }
		  (rhs_expr, free_vars, e_state, e_info, cs)
		  		= checkRhs free_vars rhs_alts rhs_locals e_input e_state e_info cs
		  (expr_with_array_selections, free_vars, e_state=:{es_dynamics = dynamics_in_rhs, es_expr_heap, es_var_heap}, e_info, cs)
				= addArraySelections array_patterns rhs_expr free_vars e_input e_state e_info cs
		  cs_symbol_table = removeLocalIdentsFromSymbolTable ei_expr_level var_env cs.cs_symbol_table
		  (guarded_expr, pattern_scheme, pattern_variables, defaul, es_var_heap, es_expr_heap, dynamics_in_patterns, cs)
		  		= transform_pattern pattern patterns pattern_scheme pattern_variables defaul expr_with_array_selections case_name calt_position
		  									es_var_heap es_expr_heap dynamics_in_rhs { cs & cs_symbol_table = cs_symbol_table }
		  e_state = { e_state & es_var_heap = es_var_heap, es_expr_heap = es_expr_heap, es_dynamics = dynamics_in_patterns ++ outer_dynamics }
		= (guarded_expr, pattern_scheme, pattern_variables, defaul, free_vars, e_state, e_info, cs)

	bind_pattern_variables [] pattern_expr expr_heap
		= (pattern_expr, [], expr_heap)
	bind_pattern_variables [{bind_src,bind_dst} : variables] this_pattern_expr expr_heap
		# free_var = { fv_ident = bind_src, fv_info_ptr = bind_dst, fv_def_level = NotALevel, fv_count = 0 }
		  (bound_var, expr_heap) = allocate_bound_var free_var expr_heap
		  (pattern_expr, binds, expr_heap) = bind_pattern_variables variables (Var bound_var) expr_heap
		= (pattern_expr, [{lb_src = this_pattern_expr, lb_dst = free_var, lb_position = NoPos } : binds], expr_heap)

checkExpression free_vars (PE_Selection selector_kind expr [PS_Array index_expr]) e_input e_state e_info cs	
	# (expr, free_vars, e_state, e_info, cs) = checkExpression free_vars expr e_input e_state e_info cs
	# (select_fun, selector_kind)
		= case selector_kind of
			ParsedNormalSelector
				-> (PD_ArraySelectFun, NormalSelector)
			ParsedUniqueSelector False
				-> (PD_UnqArraySelectFun, UniqueSingleArraySelector/*NormalSelector*/)
			ParsedUniqueSelector True
				-> (PD_UnqArraySelectFun, UniqueSingleArraySelectorUniqueElementResult)
	# (glob_select_symb, cs) = getPredefinedGlobalSymbol select_fun PD_StdArray STE_Member 2 cs
	  (selector, free_vars, e_state, e_info, cs) = checkArraySelection glob_select_symb free_vars index_expr e_input e_state e_info cs
	= (Selection selector_kind expr [selector], free_vars, e_state, e_info, cs)
checkExpression free_vars (PE_Selection selector_kind expr selectors) e_input e_state e_info cs	
	# (selectors, free_vars, e_state, e_info, cs) = checkSelectors cEndWithSelection free_vars selectors e_input e_state e_info cs
	  (expr, free_vars, e_state, e_info, cs) = checkExpression free_vars expr e_input e_state e_info cs
	= case selector_kind of
		ParsedNormalSelector
			-> (Selection NormalSelector expr selectors, free_vars, e_state, e_info, cs)
		ParsedUniqueSelector unique_element
			-> (Selection UniqueSelector expr selectors, free_vars, e_state, e_info, cs)
checkExpression free_vars (PE_Update expr1 selectors expr2) e_input e_state e_info cs	
	# (expr1, free_vars, e_state, e_info, cs) = checkExpression free_vars expr1 e_input e_state e_info cs
	  (selectors, free_vars, e_state, e_info, cs) = checkSelectors cEndWithUpdate free_vars selectors e_input e_state e_info cs
	  (expr2, free_vars, e_state, e_info, cs) = checkExpression free_vars expr2 e_input e_state e_info cs
	= (Update expr1 selectors expr2, free_vars, e_state, e_info, cs)
checkExpression free_vars (PE_Tuple exprs) e_input e_state e_info cs
	# (exprs, arity, free_vars, e_state, e_info, cs) = check_expression_list free_vars exprs e_input e_state e_info cs
	  ({glob_object={ds_ident,ds_index},glob_module}, cs)
	  		= getPredefinedGlobalSymbol (GetTupleConsIndex arity) PD_PredefinedModule STE_Constructor arity cs
	= (App { app_symb = { symb_ident = ds_ident, symb_kind = SK_Constructor { glob_object = ds_index, glob_module = glob_module }},
			 app_args = exprs, app_info_ptr = nilPtr }, free_vars, e_state, e_info, cs)
where
	check_expression_list free_vars [] e_input e_state e_info cs
		= ([], 0, free_vars, e_state, e_info, cs)
	check_expression_list free_vars [expr : exprs] e_input e_state e_info cs
		# (expr, free_vars, e_state, e_info, cs) = checkExpression free_vars expr e_input e_state e_info cs
		  (exprs, length, free_vars, e_state, e_info, cs) = check_expression_list free_vars exprs e_input e_state e_info cs
		= ([expr : exprs], inc length, free_vars, e_state, e_info, cs)

checkExpression free_vars rec=:(PE_Record record opt_type fields) e_input=:{ei_expr_level,ei_mod_index} e_state e_info cs
	# (opt_record_and_fields, e_info, cs) = checkFields ei_mod_index fields opt_type e_info cs
	= case opt_record_and_fields of
		Yes (cons=:{glob_module, glob_object}, _, new_fields)
			# {ds_ident,ds_index} = glob_object
			  rec_cons = { symb_ident = ds_ident, symb_kind = SK_Constructor { glob_object = ds_index, glob_module = glob_module } }
			-> case record of
				PE_Empty
					# (exprs, free_vars, e_state, e_info, cs) = check_field_exprs free_vars new_fields 0 RK_Constructor e_input e_state e_info cs
					-> (App { app_symb = rec_cons, app_args = remove_fields exprs, app_info_ptr = nilPtr }, free_vars, e_state, e_info, cs)
				_
					# (rec_expr, free_vars, e_state, e_info, cs) = checkExpression free_vars record e_input e_state e_info cs
					# (exprs, free_vars, e_state, e_info, cs) = check_field_exprs free_vars new_fields 0 RK_Update e_input e_state e_info cs
					-> (RecordUpdate cons rec_expr exprs, free_vars, e_state, e_info, cs)
		No
			-> (EE, free_vars, e_state, e_info, cs)
where
	remove_fields binds = [ bind_src \\ {bind_src} <- binds ]

	check_field_exprs :: [FreeVar] [Bind ParsedExpr (Global FieldSymbol)] Int RecordKind ExpressionInput !*ExpressionState !*ExpressionInfo !*CheckState -> *(![.Bind Expression (Global FieldSymbol)],![FreeVar],!*ExpressionState,!*ExpressionInfo,!*CheckState);
	check_field_exprs free_vars [] field_nr record_kind e_input e_state e_info cs
		= ([], free_vars, e_state, e_info, cs)
	check_field_exprs free_vars [field_expr : field_exprs] field_nr record_kind e_input e_state e_info cs
		# (expr, free_vars, e_state, e_info, cs)
			= check_field_expr free_vars field_expr field_nr record_kind e_input e_state e_info cs
		  (exprs, free_vars, e_state, e_info, cs) = check_field_exprs free_vars field_exprs (inc field_nr) record_kind e_input e_state e_info cs
		= ([expr : exprs], free_vars, e_state, e_info, cs)

	check_field_expr :: [FreeVar] (Bind ParsedExpr (Global FieldSymbol)) Int RecordKind ExpressionInput *ExpressionState *ExpressionInfo *CheckState -> *(!.Bind Expression (Global FieldSymbol),![FreeVar],!*ExpressionState,!*ExpressionInfo,!*CheckState);
	check_field_expr free_vars field=:{bind_src = PE_Empty, bind_dst={glob_object={fs_var,fs_ident,fs_index},glob_module}} field_nr record_kind e_input e_state e_info cs
		# (expr, free_vars, e_state, e_info, cs)
			= checkIdentExpression cIsNotInExpressionList free_vars fs_var e_input e_state e_info cs
		= ({ field & bind_src = expr }, free_vars, e_state, e_info, cs)
	check_field_expr free_vars field=:{bind_src = PE_WildCard, bind_dst={glob_object=fs_ident}} field_nr RK_Constructor e_input e_state e_info cs
		= ({ field & bind_src = NoBind nilPtr }, free_vars, e_state, e_info, { cs & cs_error = checkError fs_ident "field not specified" cs.cs_error })
	check_field_expr free_vars field=:{bind_src = PE_WildCard} field_nr RK_Update e_input e_state=:{es_expr_heap} e_info cs
		# (bind_expr_ptr, es_expr_heap) = newPtr EI_Empty es_expr_heap
		= ({ field & bind_src = NoBind bind_expr_ptr }, free_vars, { e_state & es_expr_heap = es_expr_heap }, e_info, cs)
	check_field_expr free_vars field=:{bind_src} field_nr upd_record e_input e_state e_info cs
		# (expr, free_vars, e_state, e_info, cs)
			= checkExpression free_vars bind_src e_input e_state e_info cs
		= ({ field & bind_src = expr }, free_vars, e_state, e_info, cs)

checkExpression free_vars (PE_Dynamic expr opt_type) e_input e_state=:{es_dynamics=outer_dynamics} e_info cs
	# (dyn_expr, free_vars, e_state=:{es_dynamics, es_expr_heap}, e_info, cs) = checkExpression free_vars expr e_input {e_state & es_dynamics = []} e_info cs
	  (dyn_info_ptr, es_expr_heap) = newPtr (EI_UnmarkedDynamic opt_type es_dynamics) es_expr_heap
	= (DynamicExpr { dyn_expr = dyn_expr, dyn_opt_type = opt_type, dyn_info_ptr = dyn_info_ptr, dyn_type_code = TCE_Empty},
			free_vars, { e_state & es_expr_heap = es_expr_heap, es_dynamics = [dyn_info_ptr : outer_dynamics]},
			e_info, { cs & cs_x.x_needed_modules = cs.cs_x.x_needed_modules bitor cNeedStdDynamic }) 

checkExpression free_vars (PE_Basic basic_value) e_input e_state e_info cs
	= (BasicExpr basic_value, free_vars, e_state, e_info, cs)

checkExpression free_vars (PE_ABC_Code code_sequence do_inline) e_input e_state e_info cs
	= (ABCCodeExpr code_sequence do_inline, free_vars, e_state, e_info, cs)
checkExpression free_vars (PE_Any_Code ins outs code_sequence) e_input e_state e_info cs
	# (ins, (free_vars, e_state, e_info, cs)) = check_in_parameters e_input ins (free_vars, e_state, e_info, cs)
	  (new_outs, (e_state, cs)) = check_out_parameters e_input.ei_expr_level outs (e_state, cs)
	  cs_symbol_table = remove_out_parameters_from_symbol_table e_input.ei_expr_level outs cs.cs_symbol_table
	= (AnyCodeExpr ins new_outs code_sequence, free_vars, e_state, e_info, { cs & cs_symbol_table = cs_symbol_table })
where
	check_in_parameters e_input params fv_es_ei_cs
		= mapSt (check_in_parameter e_input) params fv_es_ei_cs

	check_in_parameter e_input { bind_src, bind_dst } (free_vars, e_state, e_info, cs)
		# (id_expr, free_vars, e_state, e_info, cs) = checkIdentExpression cIsNotInExpressionList free_vars bind_dst e_input e_state e_info cs
		= case id_expr of
			Var var
				-> ({ bind_dst = var, bind_src = bind_src }, (free_vars, e_state, e_info, cs))
			_
				-> ({ bind_dst = { var_ident = bind_dst, var_info_ptr = nilPtr, var_expr_ptr = nilPtr }, bind_src = bind_src }, (free_vars, e_state, e_info,
						{ cs & cs_error = checkError bind_src "bound variable expected" cs.cs_error }))

	check_out_parameters expr_level params es_cs
		= mapSt (check_out_parameter expr_level) params es_cs

	check_out_parameter expr_level bind=:{ bind_src, bind_dst } (e_state, cs)
		| isLowerCaseName bind_dst.id_name
			# (entry, cs_symbol_table) = readPtr bind_dst.id_info cs.cs_symbol_table
			# (new_info_ptr, es_var_heap) = newPtr VI_Empty e_state.es_var_heap
			  cs = checkPatternVariable expr_level entry bind_dst new_info_ptr { cs & cs_symbol_table = cs_symbol_table }
			= (	{ bind & bind_dst = { fv_def_level = expr_level, fv_ident = bind_dst, fv_info_ptr = new_info_ptr, fv_count = 0 }},
					( { e_state & es_var_heap = es_var_heap }, cs))
			= ( { bind & bind_dst = { fv_def_level = expr_level, fv_ident = bind_dst, fv_info_ptr = nilPtr, fv_count = 0 }},
					( e_state, { cs & cs_error = checkError bind_src "variable expected" cs.cs_error }))

	remove_out_parameters_from_symbol_table expr_level idents symbol_table
		= foldSt (\{bind_dst} -> removeIdentFromSymbolTable expr_level bind_dst) idents symbol_table

checkExpression free_vars (PE_Ident id) e_input e_state e_info cs
	= checkIdentExpression cIsNotInExpressionList free_vars id e_input e_state e_info cs
checkExpression free_vars (PE_QualifiedIdent module_id ident_name) e_input e_state e_info cs
	= checkQualifiedIdentExpression free_vars module_id ident_name cIsNotInExpressionList e_input e_state e_info cs
checkExpression free_vars (PE_Generic id=:{id_name,id_info} kind) e_input e_state e_info cs=:{cs_symbol_table}
	# (entry, cs_symbol_table) = readPtr id_info cs_symbol_table
	= check_generic_expr free_vars entry id kind e_input e_state e_info {cs & cs_symbol_table = cs_symbol_table}
	where
		check_generic_expr :: ![FreeVar] !SymbolTableEntry !Ident !TypeKind !ExpressionInput !*ExpressionState !*ExpressionInfo !*CheckState
			-> (!Expression, ![FreeVar], !*ExpressionState, !*ExpressionInfo, !*CheckState)
		check_generic_expr free_vars entry=:{ste_kind=STE_Generic _,ste_index} id kind e_input=:{ei_mod_index} e_state e_info cs
			= check_it free_vars ei_mod_index ste_index id kind e_input e_state e_info cs
		check_generic_expr free_vars entry=:{ste_kind=STE_Imported (STE_Generic _) mod_index, ste_index} id kind e_input e_state e_info cs
			= check_it free_vars mod_index ste_index id kind e_input e_state e_info cs
		check_generic_expr free_vars entry=:{ste_kind=STE_Empty} id kind e_input e_state e_info cs=:{cs_error}
			= (EE, free_vars, e_state, e_info, { cs & cs_error = checkError id "undefined generic" cs_error })
		check_generic_expr free_vars entry id kind e_input e_state e_info cs=:{cs_error}
			= (EE, free_vars, e_state, e_info, { cs & cs_error = checkError id "not a generic" cs_error })

		check_it free_vars mod_index gen_index id kind e_input e_state=:{es_expr_heap} e_info cs
			#! symb_kind = SK_Generic {glob_object = gen_index, glob_module = mod_index} kind
		  	#! symbol = { symb_ident = id, symb_kind = symb_kind }			
			#! (new_info_ptr, es_expr_heap) = newPtr EI_Empty es_expr_heap
			#! app = { app_symb = symbol, app_args = [], app_info_ptr = new_info_ptr }
			#! e_state = { e_state & es_expr_heap = es_expr_heap }
			#! cs = { cs & cs_x.x_needed_modules = cs.cs_x.x_needed_modules bitor cNeedStdGeneric }
			= (App app, free_vars, e_state, e_info, cs)
checkExpression free_vars (PE_TypeSignature array_kind expr) e_input e_state e_info cs
	# (expr,free_vars,e_state,e_info,cs) = checkExpression free_vars expr e_input e_state e_info cs
	  predef_array_index = case array_kind of
								UnboxedArray -> PD_UnboxedArrayType
								StrictArray -> PD_StrictArrayType
	  ({pds_module,pds_def},cs) = cs!cs_predef_symbols.[predef_array_index]
	#! strict_array_ident = predefined_idents.[predef_array_index]
	# type_prop = { tsp_sign = BottomSignClass, tsp_propagation = NoPropClass, tsp_coercible = True }
	  strict_array_type_symb_ident = {type_ident=strict_array_ident,type_arity=1,type_index={glob_module=pds_module,glob_object=pds_def},type_prop=type_prop}
	  expr = TypeSignature (make_fresh_strict_array_type strict_array_type_symb_ident) expr
	= (expr,free_vars,e_state,e_info,cs)
	where
		make_fresh_strict_array_type strict_array_type_symb_ident var_store attr_store
			# element_type_var=TempV var_store
			  var_store=var_store+1
			  element_type_attr_var = TA_TempVar attr_store
			  attr_store=attr_store+1
			  array_type_attr_var = TA_TempVar attr_store
			  attr_store=attr_store+1
			  element_type = {at_attribute = element_type_attr_var, at_type = element_type_var}
			  strict_array_type = {at_attribute = array_type_attr_var, at_type = TA strict_array_type_symb_ident [element_type]}
			= (strict_array_type,var_store,attr_store)

/*	
	# {th_vars,th_attrs}=e_state.es_type_heaps
	# (element_type_var_ptr,th_vars) = newPtr TVI_Empty th_vars
	# (element_type_attr_ptr,th_attrs) = newPtr AVI_Empty th_attrs
	# (array_type_attr_ptr,th_attrs) = newPtr AVI_Empty th_attrs
	# e_state = {e_state & es_type_heaps = {th_vars=th_vars,th_attrs=th_attrs}}

	# element_type_var = {tv_ident = {id_name = "element_type_var", id_info = nilPtr}, tv_info_ptr = element_type_var_ptr}
	# element_type_attr_var = {av_ident = {id_name = "element_type_attr", id_info = nilPtr},av_info_ptr = element_type_attr_ptr}
	# array_type_attr_var = {av_ident = {id_name = "array_type_attr", id_info = nilPtr},av_info_ptr = array_type_attr_ptr}

	# element_type = {at_attribute = TA_Var element_type_attr_var, at_type = TV element_type_var}
	# strict_array_type = {at_attribute = TA_Var array_type_attr_var, at_type = TA strict_array_type_symb_ident [element_type]}

	# expr = TypeSignature strict_array_type expr
*/
checkExpression free_vars (PE_Matches case_ident expr pattern position) e_input=:{ei_expr_level,ei_mod_index} e_state e_info cs
	# (expr, free_vars, e_state, e_info, cs) = checkExpression free_vars expr e_input e_state e_info cs
	  {es_fun_defs,es_var_heap,es_expr_heap} = e_state
	  ps = {ps_var_heap = es_var_heap,ps_fun_defs = es_fun_defs}
	  (pattern, (_/*var_env*/, _/*array_patterns*/), {ps_fun_defs,ps_var_heap}, e_info, cs)
		= checkPattern pattern No { pi_def_level = ei_expr_level, pi_mod_index = ei_mod_index, pi_is_node_pattern = False } ([], []) ps e_info cs
	| is_single_constructor_pattern pattern
		= case pattern of
			AP_Algebraic cons_symbol global_type_index args _
				# is_cons_expr = IsConstructor expr cons_symbol (length args) global_type_index case_ident position
				  e_state & es_fun_defs=ps_fun_defs, es_var_heap = ps_var_heap, es_expr_heap = es_expr_heap
				-> (is_cons_expr, free_vars, e_state, e_info, cs)
		# fail_expr = Yes (No,BasicExpr (BVB False))
		  true_expr = BasicExpr (BVB True)
		  (guarded_expr, pattern_scheme, _/*pattern_variables*/, defaul, es_var_heap, es_expr_heap, _/*dynamics_in_patterns*/, cs)
			= transform_pattern pattern NoPattern NoPattern [] fail_expr true_expr case_ident.id_name position ps_var_heap es_expr_heap [] cs
		  (case_expr, es_var_heap, es_expr_heap)
		 	= build_and_share_case guarded_expr defaul expr case_ident cCaseExplicit es_var_heap es_expr_heap
		  e_state & es_fun_defs=ps_fun_defs, es_var_heap = es_var_heap, es_expr_heap = es_expr_heap
		= (case_expr, free_vars, e_state, e_info, cs)
where
	is_single_constructor_pattern (AP_Algebraic cons_symbol _ args No)
		| cons_symbol.glob_module==cPredefinedModuleIndex
			# pd_cons_index=cons_symbol.glob_object.ds_index+FirstConstructorPredefinedSymbolIndex
			| pd_cons_index==PD_UnboxedConsSymbol || pd_cons_index==PD_UnboxedNilSymbol ||
			  pd_cons_index==PD_UnboxedTailStrictConsSymbol || pd_cons_index==PD_UnboxedTailStrictNilSymbol ||
			  pd_cons_index==PD_OverloadedConsSymbol || pd_cons_index==PD_OverloadedNilSymbol
				= False
				= all_wild_card_args args
			= all_wild_card_args args
	is_single_constructor_pattern _
		= False

	all_wild_card_args [AP_WildCard No : args]
		= all_wild_card_args args
	all_wild_card_args [_:_]
		= False
	all_wild_card_args []
		= True
checkExpression free_vars expr e_input e_state e_info cs
	= abort "checkExpression (checkFunctionBodies.icl)" // <<- expr

transform_pattern :: !AuxiliaryPattern !CasePatterns !CasePatterns !(Env Ident VarInfoPtr) !(Optional (!Optional FreeVar, !Expression)) !Expression
		!String !Position !*VarHeap !*ExpressionHeap !Dynamics !*CheckState
	-> (!CasePatterns, !CasePatterns, !Env Ident VarInfoPtr, !Optional (!Optional FreeVar,!Expression), !*VarHeap, !*ExpressionHeap, ![DynamicPtr], !*CheckState)
transform_pattern (AP_Algebraic cons_symbol global_type_index args opt_var) patterns pattern_scheme pattern_variables defaul result_expr _ pos var_store expr_heap opt_dynamics cs
	# (var_args, result_expr, _, var_store, expr_heap, opt_dynamics, cs) = convertSubPatterns args result_expr pos var_store expr_heap opt_dynamics cs
	  pattern_variables = cons_optional opt_var pattern_variables
	# pattern = { ap_symbol = cons_symbol, ap_vars = var_args, ap_expr = result_expr, ap_position = pos}
	| cons_symbol.glob_module==cPredefinedModuleIndex
		# pd_cons_index=cons_symbol.glob_object.ds_index+FirstConstructorPredefinedSymbolIndex
		| pd_cons_index==PD_UnboxedConsSymbol || pd_cons_index==PD_UnboxedNilSymbol
			# (unboxed_list,decons_expr,expr_heap,cs) = make_unboxed_list global_type_index expr_heap cs
			= case pattern_scheme of
				OverloadedListPatterns (UnboxedList _ _ _ _) _ _
					# alg_patterns = alg_patterns_of_OverloadedListPatterns_or_NoPattern patterns
					-> (OverloadedListPatterns unboxed_list decons_expr [pattern : alg_patterns], pattern_scheme, pattern_variables, defaul, var_store, expr_heap, opt_dynamics, cs)
				OverloadedListPatterns (OverloadedList _ _ _ _) _ _
					# alg_patterns = alg_patterns_of_OverloadedListPatterns_or_NoPattern patterns
					# (alg_patterns,cs) = replace_overloaded_symbols_in_patterns alg_patterns PD_UnboxedConsSymbol PD_UnboxedNilSymbol cs
					-> (OverloadedListPatterns unboxed_list decons_expr [pattern : alg_patterns], OverloadedListPatterns unboxed_list decons_expr [], pattern_variables, defaul, var_store, expr_heap, opt_dynamics, cs)
				NoPattern
					-> (OverloadedListPatterns unboxed_list decons_expr [pattern], OverloadedListPatterns unboxed_list decons_expr [], pattern_variables, defaul, var_store, expr_heap, opt_dynamics, cs)
				_
					-> (patterns, pattern_scheme, pattern_variables, defaul, var_store, expr_heap, opt_dynamics,illegal_combination_of_patterns_error cons_symbol cs)

		| pd_cons_index==PD_UnboxedTailStrictConsSymbol || pd_cons_index==PD_UnboxedTailStrictNilSymbol
			# (unboxed_tail_strict_list,decons_expr,expr_heap,cs) = make_unboxed_tail_strict_list global_type_index expr_heap cs
			= case pattern_scheme of
				OverloadedListPatterns (UnboxedTailStrictList _ _ _ _) _ _
					# alg_patterns = alg_patterns_of_OverloadedListPatterns_or_NoPattern patterns
					-> (OverloadedListPatterns unboxed_tail_strict_list decons_expr [pattern : alg_patterns], pattern_scheme, pattern_variables, defaul, var_store, expr_heap, opt_dynamics, cs)
				OverloadedListPatterns (OverloadedList _ _ _ _) _ _
					# alg_patterns = alg_patterns_of_OverloadedListPatterns_or_NoPattern patterns
					# (alg_patterns,cs) = replace_overloaded_symbols_in_patterns alg_patterns PD_UnboxedTailStrictConsSymbol PD_UnboxedTailStrictNilSymbol cs
					-> (OverloadedListPatterns unboxed_tail_strict_list decons_expr [pattern : alg_patterns], OverloadedListPatterns unboxed_tail_strict_list decons_expr [], pattern_variables, defaul, var_store, expr_heap, opt_dynamics, cs)
				NoPattern
					-> (OverloadedListPatterns unboxed_tail_strict_list decons_expr [pattern], OverloadedListPatterns unboxed_tail_strict_list decons_expr [], pattern_variables, defaul, var_store, expr_heap, opt_dynamics, cs)
				_
					-> (patterns, pattern_scheme, pattern_variables, defaul, var_store, expr_heap, opt_dynamics,illegal_combination_of_patterns_error cons_symbol cs)
		
		| pd_cons_index==PD_OverloadedConsSymbol || pd_cons_index==PD_OverloadedNilSymbol
			= case pattern_scheme of
				OverloadedListPatterns (OverloadedList _ _ _ _) _ _
					# (overloaded_list,decons_expr,expr_heap,cs) = make_overloaded_list global_type_index expr_heap cs
					# alg_patterns = alg_patterns_of_OverloadedListPatterns_or_NoPattern patterns
					-> (OverloadedListPatterns overloaded_list decons_expr [pattern : alg_patterns], pattern_scheme, pattern_variables, defaul, var_store, expr_heap, opt_dynamics, cs)
				OverloadedListPatterns (UnboxedList _ _ _ _) _ _
					# (unboxed_list,decons_expr,expr_heap,cs) = make_unboxed_list global_type_index expr_heap cs
					# alg_patterns = alg_patterns_of_OverloadedListPatterns_or_NoPattern patterns
					# (pattern,cs) = replace_overloaded_symbol_in_pattern pattern PD_UnboxedConsSymbol PD_UnboxedNilSymbol cs
					-> (OverloadedListPatterns unboxed_list decons_expr [pattern : alg_patterns], pattern_scheme, pattern_variables, defaul, var_store, expr_heap, opt_dynamics, cs)
				OverloadedListPatterns (UnboxedTailStrictList _ _ _ _) _ _
					# (unboxed_tail_strict_list,decons_expr,expr_heap,cs) = make_unboxed_tail_strict_list global_type_index expr_heap cs
					# alg_patterns = alg_patterns_of_OverloadedListPatterns_or_NoPattern patterns
					# (pattern,cs) = replace_overloaded_symbol_in_pattern pattern PD_UnboxedTailStrictConsSymbol PD_UnboxedTailStrictNilSymbol cs
					-> (OverloadedListPatterns unboxed_tail_strict_list decons_expr [pattern : alg_patterns], pattern_scheme, pattern_variables, defaul, var_store, expr_heap, opt_dynamics, cs)
				AlgebraicPatterns alg_type _
					| alg_type.gi_module==cPredefinedModuleIndex
						# index=alg_type.gi_index+FirstTypePredefinedSymbolIndex
						| index==PD_ListType
							# alg_patterns = alg_patterns_of_AlgebraicPatterns_or_NoPattern patterns
							# (pattern,cs) = replace_overloaded_symbol_in_pattern pattern PD_ConsSymbol PD_NilSymbol cs
							-> (AlgebraicPatterns alg_type [pattern : alg_patterns], pattern_scheme, pattern_variables, defaul, var_store, expr_heap, opt_dynamics, cs)
						| index==PD_StrictListType
							# alg_patterns = alg_patterns_of_AlgebraicPatterns_or_NoPattern patterns
							# (pattern,cs) = replace_overloaded_symbol_in_pattern pattern PD_StrictConsSymbol PD_StrictNilSymbol cs
							-> (AlgebraicPatterns alg_type [pattern : alg_patterns], pattern_scheme, pattern_variables, defaul, var_store, expr_heap, opt_dynamics, cs)
						| index==PD_TailStrictListType
							# alg_patterns = alg_patterns_of_AlgebraicPatterns_or_NoPattern patterns
							# (pattern,cs) = replace_overloaded_symbol_in_pattern pattern PD_TailStrictConsSymbol PD_TailStrictNilSymbol cs
							-> (AlgebraicPatterns alg_type [pattern : alg_patterns], pattern_scheme, pattern_variables, defaul, var_store, expr_heap, opt_dynamics, cs)
						| index==PD_StrictTailStrictListType
							# alg_patterns = alg_patterns_of_AlgebraicPatterns_or_NoPattern patterns
							# (pattern,cs) = replace_overloaded_symbol_in_pattern pattern PD_StrictTailStrictConsSymbol PD_StrictTailStrictNilSymbol cs
							-> (AlgebraicPatterns alg_type [pattern : alg_patterns], pattern_scheme, pattern_variables, defaul, var_store, expr_heap, opt_dynamics, cs)
							-> (patterns, pattern_scheme, pattern_variables, defaul, var_store, expr_heap, opt_dynamics,illegal_combination_of_patterns_error cons_symbol cs)
						-> (patterns, pattern_scheme, pattern_variables, defaul, var_store, expr_heap, opt_dynamics,illegal_combination_of_patterns_error cons_symbol cs)
				NoPattern
					# (overloaded_list,decons_expr,expr_heap,cs) = make_overloaded_list global_type_index expr_heap cs
					-> (OverloadedListPatterns overloaded_list decons_expr [pattern], OverloadedListPatterns overloaded_list decons_expr [], pattern_variables, defaul, var_store, expr_heap, opt_dynamics, cs)
				_
					-> (patterns, pattern_scheme, pattern_variables, defaul, var_store, expr_heap, opt_dynamics,illegal_combination_of_patterns_error cons_symbol cs)
			= case pattern_scheme of
				AlgebraicPatterns alg_type _
					| global_type_index == alg_type
						# alg_patterns = alg_patterns_of_AlgebraicPatterns_or_NoPattern patterns
						-> (AlgebraicPatterns global_type_index [pattern : alg_patterns], pattern_scheme, pattern_variables, defaul, var_store, expr_heap, opt_dynamics, cs)
						-> (patterns, pattern_scheme, pattern_variables, defaul, var_store, expr_heap, opt_dynamics,
									{ cs & cs_error = checkError cons_symbol.glob_object.ds_ident "incompatible types of patterns" cs.cs_error })
				OverloadedListPatterns (OverloadedList _ _ _ _) _ _
					| global_type_index.gi_module==cPredefinedModuleIndex
						# index=global_type_index.gi_index+FirstTypePredefinedSymbolIndex
						| index==PD_ListType
							# alg_patterns = alg_patterns_of_OverloadedListPatterns_or_NoPattern patterns
							# (alg_patterns,cs) = replace_overloaded_symbols_in_patterns alg_patterns PD_ConsSymbol PD_NilSymbol cs
							-> (AlgebraicPatterns global_type_index [pattern:alg_patterns], AlgebraicPatterns global_type_index [], pattern_variables, defaul, var_store, expr_heap, opt_dynamics, cs)
						| index==PD_StrictListType
							# alg_patterns = alg_patterns_of_OverloadedListPatterns_or_NoPattern patterns
							# (alg_patterns,cs) = replace_overloaded_symbols_in_patterns alg_patterns PD_StrictConsSymbol PD_StrictNilSymbol cs
							-> (AlgebraicPatterns global_type_index [pattern:alg_patterns], AlgebraicPatterns global_type_index [], pattern_variables, defaul, var_store, expr_heap, opt_dynamics, cs)
						| index==PD_TailStrictListType
							# alg_patterns = alg_patterns_of_OverloadedListPatterns_or_NoPattern patterns
							# (alg_patterns,cs) = replace_overloaded_symbols_in_patterns alg_patterns PD_TailStrictConsSymbol PD_TailStrictNilSymbol cs
							-> (AlgebraicPatterns global_type_index [pattern:alg_patterns], AlgebraicPatterns global_type_index [], pattern_variables, defaul, var_store, expr_heap, opt_dynamics, cs)
						| index==PD_StrictTailStrictListType
							# alg_patterns = alg_patterns_of_OverloadedListPatterns_or_NoPattern patterns
							# (alg_patterns,cs) = replace_overloaded_symbols_in_patterns alg_patterns PD_StrictTailStrictConsSymbol PD_StrictTailStrictNilSymbol cs
							-> (AlgebraicPatterns global_type_index [pattern:alg_patterns], AlgebraicPatterns global_type_index [], pattern_variables, defaul, var_store, expr_heap, opt_dynamics, cs)
							-> (patterns, pattern_scheme, pattern_variables, defaul, var_store, expr_heap, opt_dynamics,illegal_combination_of_patterns_error cons_symbol cs)
				NoPattern
					-> (AlgebraicPatterns global_type_index [pattern], AlgebraicPatterns global_type_index [], pattern_variables, defaul, var_store, expr_heap, opt_dynamics, cs)
				_
					-> (patterns, pattern_scheme, pattern_variables, defaul, var_store, expr_heap, opt_dynamics,illegal_combination_of_patterns_error cons_symbol cs)
		= case pattern_scheme of
			AlgebraicPatterns alg_type _
				| global_type_index == alg_type
					# alg_patterns = alg_patterns_of_AlgebraicPatterns_or_NoPattern patterns
					-> (AlgebraicPatterns global_type_index [pattern : alg_patterns], pattern_scheme, pattern_variables, defaul, var_store, expr_heap, opt_dynamics, cs)
					# cs & cs_error = checkError cons_symbol.glob_object.ds_ident "incompatible types of patterns" cs.cs_error
					-> (patterns, pattern_scheme, pattern_variables, defaul, var_store, expr_heap, opt_dynamics, cs)
			NoPattern
				-> (AlgebraicPatterns global_type_index [pattern], AlgebraicPatterns global_type_index [], pattern_variables, defaul, var_store, expr_heap, opt_dynamics, cs)
			_
				-> (patterns, pattern_scheme, pattern_variables, defaul, var_store, expr_heap, opt_dynamics,illegal_combination_of_patterns_error cons_symbol cs)
	where
		alg_patterns_of_AlgebraicPatterns_or_NoPattern (AlgebraicPatterns _ alg_patterns) = alg_patterns
		alg_patterns_of_AlgebraicPatterns_or_NoPattern NoPattern = []

		alg_patterns_of_OverloadedListPatterns_or_NoPattern (OverloadedListPatterns _ _ alg_patterns) = alg_patterns
		alg_patterns_of_OverloadedListPatterns_or_NoPattern NoPattern = []

		illegal_combination_of_patterns_error cons_symbol cs
			= { cs & cs_error = checkError cons_symbol.glob_object.ds_ident "illegal combination of patterns" cs.cs_error }

		replace_overloaded_symbols_in_patterns [] pd_cons_symbol pd_nil_symbol cs
			= ([],cs)
		replace_overloaded_symbols_in_patterns [pattern=:{ap_symbol={glob_module,glob_object}}:patterns] pd_cons_symbol pd_nil_symbol cs
			# (pattern,cs) = replace_overloaded_symbol_in_pattern pattern pd_cons_symbol pd_nil_symbol cs	
			# (patterns,cs) = replace_overloaded_symbols_in_patterns patterns pd_cons_symbol pd_nil_symbol cs
			= ([pattern:patterns],cs)

		replace_overloaded_symbol_in_pattern pattern=:{ap_symbol={glob_module,glob_object}} pd_cons_symbol pd_nil_symbol cs
			| glob_module==cPredefinedModuleIndex
				# index=glob_object.ds_index+FirstConstructorPredefinedSymbolIndex
				| index==PD_OverloadedConsSymbol
					# ({pds_def},cs) = cs!cs_predef_symbols.[pd_cons_symbol]
					# pds_ident = predefined_idents.[pd_cons_symbol]
					# glob_object = {glob_object & ds_index=pds_def,ds_ident=pds_ident}
					= ({pattern & ap_symbol.glob_object=glob_object},cs)
				| index==PD_OverloadedNilSymbol
					# ({pds_def},cs) = cs!cs_predef_symbols.[pd_nil_symbol]
					# pds_ident = predefined_idents.[pd_nil_symbol]
					# glob_object = {glob_object & ds_index=pds_def,ds_ident=pds_ident}
					= ({pattern & ap_symbol.glob_object=glob_object},cs)
					= abort "replace_overloaded_symbol_in_pattern"

transform_pattern (AP_Basic basic_val opt_var) patterns pattern_scheme pattern_variables defaul result_expr _ pos var_store expr_heap opt_dynamics cs
	# pattern = { bp_value = basic_val, bp_expr = result_expr, bp_position = pos}
	  pattern_variables = cons_optional opt_var pattern_variables
	  (type_symbol, cs) = typeOfBasicValue basic_val cs
	= case pattern_scheme of
		BasicPatterns basic_type _
			| type_symbol == basic_type
				# basic_patterns = case patterns of
						BasicPatterns _ basic_patterns
							-> basic_patterns
						NoPattern
							-> [] 
				-> (BasicPatterns basic_type [pattern : basic_patterns], pattern_scheme, pattern_variables, defaul, var_store, expr_heap, opt_dynamics, cs)
				-> (patterns, pattern_scheme, pattern_variables, defaul, var_store, expr_heap, opt_dynamics,
						{ cs & cs_error = checkError basic_val "incompatible types of patterns" cs.cs_error })
		NoPattern
			-> (BasicPatterns type_symbol [pattern], BasicPatterns type_symbol [], pattern_variables, defaul, var_store, expr_heap, opt_dynamics, cs)
		_
			-> (patterns, pattern_scheme, pattern_variables, defaul, var_store, expr_heap, opt_dynamics,
					{ cs & cs_error = checkError basic_val "illegal combination of patterns" cs.cs_error})
transform_pattern (AP_Dynamic pattern type opt_var) patterns pattern_scheme pattern_variables defaul result_expr _ pos var_store expr_heap opt_dynamics cs
	# (var_arg, result_expr, _, var_store, expr_heap, opt_dynamics, cs) = convertSubPattern pattern result_expr pos var_store expr_heap opt_dynamics cs
	  (dynamic_info_ptr, expr_heap) = newPtr (EI_DynamicType type opt_dynamics) expr_heap
	  pattern = { dp_var = var_arg, dp_type	= dynamic_info_ptr,	dp_rhs = result_expr,
	  				dp_type_code = TCE_Empty, dp_position = pos }
	  pattern_variables = cons_optional opt_var pattern_variables
	= case pattern_scheme of
		DynamicPatterns _
			# dyn_patterns = case patterns of 
									DynamicPatterns dyn_patterns
										-> dyn_patterns
									NoPattern
										-> []
			-> (DynamicPatterns [pattern : dyn_patterns], pattern_scheme, pattern_variables, defaul, var_store, expr_heap, [dynamic_info_ptr], cs)
		NoPattern
			-> (DynamicPatterns [pattern], DynamicPatterns [], pattern_variables, defaul, var_store, expr_heap, [dynamic_info_ptr], cs)
		_
			-> (patterns, pattern_scheme, pattern_variables, defaul, var_store, expr_heap, opt_dynamics,
					{ cs & cs_error = checkError "<dynamic pattern>" "illegal combination of patterns" cs.cs_error })
transform_pattern (AP_Variable name var_info opt_var) NoPattern pattern_scheme pattern_variables No result_expr _ pos var_store expr_heap opt_dynamics cs
	= ( NoPattern, pattern_scheme, cons_optional opt_var pattern_variables, 
	  	Yes (Yes { fv_ident = name, fv_info_ptr = var_info, fv_def_level = NotALevel, fv_count = 0 }, result_expr),
		var_store, expr_heap, opt_dynamics, cs)
transform_pattern (AP_Variable name var_info opt_var) patterns pattern_scheme pattern_variables defaul result_expr case_name pos var_store expr_heap opt_dynamics cs
	# free_var = { fv_ident = name, fv_info_ptr = var_info, fv_def_level = NotALevel, fv_count = 0 }
	  (new_bound_var, expr_heap) = allocate_bound_var free_var expr_heap
	  case_ident = { id_name = case_name, id_info = nilPtr }
	  (new_case, var_store, expr_heap) = build_and_share_case patterns defaul (Var new_bound_var) case_ident cCaseExplicit var_store expr_heap
	  new_defaul = insert_as_default result_expr new_case
	= (NoPattern, pattern_scheme, (cons_optional opt_var pattern_variables), Yes (Yes free_var, new_defaul),
		var_store, expr_heap, opt_dynamics, cs)
where
	insert_as_default :: !Expression !Expression -> Expression
	insert_as_default (Let lad=:{let_expr}) to_insert
		= Let { lad & let_expr = insert_as_default let_expr to_insert }
	insert_as_default (Case kees=:{case_default,case_explicit=False}) to_insert
		= case case_default of
			No			-> Case { kees & case_default = Yes to_insert }
			Yes defaul	-> Case { kees & case_default = Yes (insert_as_default defaul to_insert)}
	insert_as_default expr _ = expr // checkWarning "pattern won't match"
transform_pattern (AP_NewType cons_symbol type_index arg opt_var) patterns pattern_scheme pattern_variables defaul result_expr _ pos var_store expr_heap opt_dynamics cs
	# (var_arg, result_expr, _, var_store, expr_heap, opt_dynamics, cs) = convertSubPattern arg result_expr pos var_store expr_heap opt_dynamics cs
	  type_symbol = {gi_module = cons_symbol.glob_module, gi_index = type_index}
	  pattern_variables = cons_optional opt_var pattern_variables
	# pattern = { ap_symbol = cons_symbol, ap_vars = [var_arg], ap_expr = result_expr, ap_position = pos}
	= case pattern_scheme of
		NewTypePatterns alg_type _
			| type_symbol == alg_type
				# newtype_patterns = case patterns of
											NewTypePatterns _ newtype_patterns -> newtype_patterns
											NoPattern -> []
				-> (NewTypePatterns type_symbol [pattern : newtype_patterns], pattern_scheme, pattern_variables, defaul, var_store, expr_heap, opt_dynamics, cs)
				-> (patterns, pattern_scheme, pattern_variables, defaul, var_store, expr_heap, opt_dynamics,
							{ cs & cs_error = checkError cons_symbol.glob_object.ds_ident "incompatible types of patterns" cs.cs_error })
		NoPattern
			-> (NewTypePatterns type_symbol [pattern], NewTypePatterns type_symbol [], pattern_variables, defaul, var_store, expr_heap, opt_dynamics, cs)
		_
			-> (patterns, pattern_scheme, pattern_variables, defaul, var_store, expr_heap, opt_dynamics,illegal_combination_of_patterns_error cons_symbol cs)
	where
		illegal_combination_of_patterns_error cons_symbol cs
			= { cs & cs_error = checkError cons_symbol.glob_object.ds_ident "illegal combination of patterns" cs.cs_error }
transform_pattern (AP_WildCard (Yes opt_var)) patterns pattern_scheme pattern_variables defaul result_expr case_name pos var_store expr_heap opt_dynamics cs
	= transform_pattern (AP_Variable opt_var.bind_src opt_var.bind_dst No) patterns pattern_scheme pattern_variables defaul
						result_expr case_name pos var_store expr_heap opt_dynamics cs
transform_pattern (AP_WildCard no) NoPattern pattern_scheme pattern_variables No result_expr _ pos var_store expr_heap opt_dynamics cs
	= (NoPattern, pattern_scheme, pattern_variables, Yes (No, result_expr), var_store, expr_heap, opt_dynamics, cs)
transform_pattern (AP_WildCard _) patterns pattern_scheme pattern_variables defaul result_expr case_name pos var_store expr_heap opt_dynamics cs
	# (new_info_ptr, var_store) = newPtr VI_Empty var_store
	= transform_pattern (AP_Variable (newVarId "wc") new_info_ptr No) patterns pattern_scheme pattern_variables defaul
						result_expr case_name pos var_store expr_heap opt_dynamics cs
transform_pattern AP_Empty patterns pattern_scheme pattern_variables defaul result_expr _ pos var_store expr_heap opt_dynamics cs
	= (patterns, pattern_scheme, pattern_variables, defaul, var_store, expr_heap, opt_dynamics, cs)

build_and_share_case patterns defaul expr case_ident explicit var_heap expr_heap
	# (expr, expr_heap) = build_case patterns defaul expr case_ident explicit expr_heap
	= share_case_expr expr var_heap expr_heap
where
	build_case NoPattern defaul expr case_ident explicit expr_heap
		= case defaul of
			Yes (opt_var, result)
				-> case opt_var of
					Yes var
						-> bind_default_variable expr var result expr_heap
					No
						-> (result, expr_heap)
			No
				-> (EE, expr_heap)
	build_case (DynamicPatterns patterns) defaul expr case_ident explicit expr_heap
		= case defaul of
			Yes (opt_var, result)
				-> case opt_var of
					Yes var
						# (type_case_info_ptr, expr_heap) = newPtr EI_Empty expr_heap
						  (bound_var, expr_heap) = allocate_bound_var var expr_heap
						  result = buildTypeCase (Var bound_var) patterns (Yes result) type_case_info_ptr cCaseExplicit
						-> bind_default_variable expr var result expr_heap
					No
						# (type_case_info_ptr, expr_heap) = newPtr EI_Empty expr_heap
						-> (buildTypeCase expr patterns (Yes result) type_case_info_ptr cCaseExplicit, expr_heap)
			No
				# (type_case_info_ptr, expr_heap) = newPtr EI_Empty expr_heap
				-> (buildTypeCase expr patterns No type_case_info_ptr cCaseExplicit, expr_heap)
	build_case patterns (Yes (opt_var,result)) expr case_ident explicit expr_heap
		= case opt_var of
			Yes var
				# (case_expr_ptr, expr_heap) = newPtr EI_Empty expr_heap
				  (bound_var, expr_heap) = allocate_bound_var var expr_heap
				  result = Case {case_expr = Var bound_var, case_guards = patterns, case_default = Yes result,
								 case_ident = Yes case_ident, case_info_ptr = case_expr_ptr,
								 case_explicit = explicit,
								 case_default_pos = NoPos }
				-> bind_default_variable expr var result expr_heap
			No
				#  (case_expr_ptr, expr_heap) = newPtr EI_Empty expr_heap
				-> (Case {case_expr = expr, case_guards = patterns, case_default = Yes result,
							case_explicit = explicit,
							case_ident = Yes case_ident, case_info_ptr = case_expr_ptr, case_default_pos = NoPos }, expr_heap)
	build_case patterns No expr case_ident explicit expr_heap
		# (case_expr_ptr, expr_heap) = newPtr EI_Empty expr_heap
		= (Case {case_expr = expr, case_guards = patterns, case_default = No, case_ident = Yes case_ident,
					case_explicit = explicit,
					case_info_ptr = case_expr_ptr, case_default_pos = NoPos }, expr_heap)

// make sure that the case_expr is a variable, because that's needed for merging
// the alternatives in cases (in transform.icl)
// FIXME: this should be represented in the syntax tree: change case_expr to
// case_var :: BoundVar in Case
share_case_expr (Let lad=:{let_expr}) var_heap expr_heap
	# (let_expr, var_heap, expr_heap) = share_case_expr let_expr var_heap expr_heap
	= (Let {lad & let_expr = let_expr}, var_heap, expr_heap)
share_case_expr expr=:(Case {case_expr=Var var_ptr}) var_heap expr_heap
	= (expr, var_heap, expr_heap)
share_case_expr (Case kees=:{case_expr}) var_heap expr_heap
	# (free_var, var_heap) = allocate_free_var { id_name = "_case_var", id_info = nilPtr } var_heap
	  (bound_var, expr_heap) = allocate_bound_var free_var expr_heap
	  (case_expression, expr_heap) = bind_default_variable case_expr free_var (Case {kees & case_expr = Var bound_var}) expr_heap
 	= (case_expression, var_heap, expr_heap)
share_case_expr expr var_heap expr_heap
	=  (expr, var_heap, expr_heap)			

bind_default_variable lb_src lb_dst result_expr expr_heap
	# (let_expr_ptr, expr_heap) = newPtr EI_Empty expr_heap
	= (Let {let_strict_binds = [], let_lazy_binds = [{ lb_src = lb_src, lb_dst = lb_dst, lb_position = NoPos }],
			let_expr = result_expr, let_info_ptr = let_expr_ptr, let_expr_position = NoPos }, expr_heap)

cons_optional (Yes var) variables
	= [ var : variables ]
cons_optional No variables
	= variables

no_TFAC_argument [{at_type=TFAC _ _ _}:_] = False
no_TFAC_argument [_:args] = no_TFAC_argument args
no_TFAC_argument [] = True

checkIdentExpression :: !Bool ![FreeVar] !Ident !ExpressionInput !*ExpressionState !u:ExpressionInfo !*CheckState
									-> (!Expression, ![FreeVar], !*ExpressionState,!u:ExpressionInfo,!*CheckState)
checkIdentExpression is_expr_list free_vars id=:{id_info} e_input e_state e_info cs=:{cs_symbol_table}
	# (entry, cs_symbol_table) = readPtr id_info cs_symbol_table
	= check_id_expression entry is_expr_list free_vars id e_input e_state e_info { cs & cs_symbol_table = cs_symbol_table }
where
	check_id_expression :: !SymbolTableEntry !Bool ![FreeVar] !Ident !ExpressionInput !*ExpressionState !u:ExpressionInfo !*CheckState
														 -> (!Expression, ![FreeVar], !*ExpressionState,!u:ExpressionInfo,!*CheckState)
	check_id_expression {ste_kind = STE_Empty} is_expr_list free_vars id e_input e_state e_info cs=:{cs_error,cs_predef_symbols,cs_x}
		# local_predefined_idents = predefined_idents
		# from_ident = local_predefined_idents.[PD_From]
		  from_then_ident = local_predefined_idents.[PD_FromThen]
		  from_to_ident = local_predefined_idents.[PD_FromTo]
		  from_then_to_ident = local_predefined_idents.[PD_FromThenTo]

		| id==from_ident || id==from_then_ident || id==from_to_ident || id==from_then_to_ident
			= (EE, free_vars, e_state, e_info, { cs & cs_x.x_needed_modules = cs_x.x_needed_modules bitor cStdEnumImportMissing})
				// instead of giving an error message remember that StdEnum should have been imported.
				// Error will be given in function check_needed_modules_are_imported
		| id==local_predefined_idents.[PD_FromS] || id==local_predefined_idents.[PD_FromTS] || id==local_predefined_idents.[PD_FromSTS]
		|| id==local_predefined_idents.[PD_FromU] || id==local_predefined_idents.[PD_FromUTS] || id==local_predefined_idents.[PD_FromO]
		|| id==local_predefined_idents.[PD_FromThenS] || id==local_predefined_idents.[PD_FromThenTS] || id==local_predefined_idents.[PD_FromThenSTS]
		|| id==local_predefined_idents.[PD_FromThenU] || id==local_predefined_idents.[PD_FromThenUTS] || id==local_predefined_idents.[PD_FromThenO]
		|| id==local_predefined_idents.[PD_FromToS] || id==local_predefined_idents.[PD_FromToTS] || id==local_predefined_idents.[PD_FromToSTS]
		|| id==local_predefined_idents.[PD_FromToU] || id==local_predefined_idents.[PD_FromToUTS] || id==local_predefined_idents.[PD_FromToO]
		|| id==local_predefined_idents.[PD_FromThenToS] || id==local_predefined_idents.[PD_FromThenToTS] || id==local_predefined_idents.[PD_FromThenToSTS]
		|| id==local_predefined_idents.[PD_FromThenToU] || id==local_predefined_idents.[PD_FromThenToUTS] || id==local_predefined_idents.[PD_FromThenToO]
			= (EE, free_vars, e_state, e_info, { cs & cs_x.x_needed_modules = cs_x.x_needed_modules bitor cNeedStdStrictLists})
		# createArray_ident = local_predefined_idents.[PD__CreateArrayFun]
		  uselect_ident = local_predefined_idents.[PD_UnqArraySelectFun]
		  update_ident = local_predefined_idents.[PD_ArrayUpdateFun]
		  usize_ident = local_predefined_idents.[PD_UnqArraySizeFun]
		| id==createArray_ident || id==uselect_ident || id==update_ident || id==usize_ident
			= (EE, free_vars, e_state, e_info, { cs & cs_x.x_needed_modules = cs_x.x_needed_modules bitor cStdArrayImportMissing})
				// instead of giving an error message remember that StdArray should have been be imported.
				//  Error will be given in function check_needed_modules_are_imported
		| id==local_predefined_idents.[PD_cons] || id==local_predefined_idents.[PD_decons]
		  || id==local_predefined_idents.[PD_cons_u] || id==local_predefined_idents.[PD_decons_u]
		  || id==local_predefined_idents.[PD_cons_uts] || id==local_predefined_idents.[PD_decons_uts]
		  || id==local_predefined_idents.[PD_nil] || id==local_predefined_idents.[PD_nil_u] || id==local_predefined_idents.[PD_nil_uts]
			= (EE, free_vars, e_state, e_info, { cs & cs_x.x_needed_modules = cs_x.x_needed_modules bitor cNeedStdStrictLists})
				// instead report that StdStrictLists should be imported in function check_needed_modules_are_imported
		= (EE, free_vars, e_state, e_info, { cs & cs_error = checkError id "undefined" cs_error })
	check_id_expression {ste_kind = STE_Variable info_ptr,ste_def_level} is_expr_list free_vars id e_input=:{ei_fun_level} e_state=:{es_expr_heap} e_info cs
		| ste_def_level < ei_fun_level
			# free_var = { fv_def_level = ste_def_level, fv_ident = id, fv_info_ptr = info_ptr, fv_count = 0 }
			  (free_var_added, free_vars) = newFreeVariable free_var free_vars
			= (FreeVar free_var, free_vars, e_state, e_info, cs)
			#! (var_expr_ptr, es_expr_heap) = newPtr EI_Empty es_expr_heap
			= (Var {var_ident = id, var_info_ptr = info_ptr, var_expr_ptr = var_expr_ptr}, free_vars,
					{e_state & es_expr_heap = es_expr_heap}, e_info, cs)
	check_id_expression {ste_kind = STE_Generic _} is_expr_list free_vars id e_input e_state e_info cs=:{cs_error}
		= (EE, free_vars, e_state, e_info, 
			{ cs & cs_error = checkError id "generic: missing kind argument" cs_error})
	check_id_expression {ste_kind = STE_Imported (STE_Generic _) _} is_expr_list free_vars id e_input e_state e_info cs=:{cs_error}
		= (EE, free_vars, e_state, e_info, 
			{ cs & cs_error = checkError id "generic: missing kind argument" cs_error})								
	check_id_expression entry is_expr_list free_vars id=:{id_info} e_input e_state e_info cs
		# (symb_kind, arity, priority, e_state, e_info, cs) = determine_info_of_symbol entry id_info e_input e_state e_info cs
		  symbol = { symb_ident = id, symb_kind = symb_kind }
  		| is_expr_list
			= (Constant symbol arity priority, free_vars, e_state, e_info, cs)
			= case symb_kind of
				SK_Constructor _
					# app_expr = App {app_symb = symbol, app_args = [], app_info_ptr = nilPtr}
					-> (app_expr, free_vars, e_state, e_info, cs)
				SK_OverloadedConstructor cons_index
					# (new_info_ptr, es_expr_heap) = newPtr EI_Empty e_state.es_expr_heap
					  app_expr = App {app_symb = {symbol & symb_kind=SK_Constructor cons_index}, app_args = [], app_info_ptr = new_info_ptr}
					-> (app_expr, free_vars, {e_state & es_expr_heap = es_expr_heap}, e_info, cs)
				SK_NewTypeConstructor _
					# cs = { cs & cs_error = checkError id "argument missing (for newtype constructor)" cs.cs_error}
					# app_expr = App { app_symb = symbol , app_args = [], app_info_ptr = nilPtr }
					-> (app_expr, free_vars, e_state, e_info, cs)
				_
					# (new_info_ptr, es_expr_heap) = newPtr EI_Empty e_state.es_expr_heap
					# app_expr = App { app_symb = symbol , app_args = [], app_info_ptr = new_info_ptr }
					-> (app_expr, free_vars, { e_state & es_expr_heap = es_expr_heap }, e_info, cs)	

	determine_info_of_symbol :: !SymbolTableEntry !SymbolPtr !ExpressionInput !*ExpressionState !u:ExpressionInfo !*CheckState
		-> (!SymbKind, !Int, !Priority, !*ExpressionState, !u:ExpressionInfo,!*CheckState)
	determine_info_of_symbol entry=:{ste_kind=STE_FunctionOrMacro calls,ste_index,ste_def_level} symb_info
				e_input=:{ei_fun_index} e_state=:{es_calls} e_info cs=:{cs_symbol_table,cs_x}
		# (fun_def,e_state) = e_state!es_fun_defs.[ste_index]
		# {fun_ident,fun_arity,fun_kind,fun_priority,fun_info={fi_properties}}=fun_def 
		# index = { glob_object = ste_index, glob_module = cs_x.x_main_dcl_module_n }
		# symbol_kind = convert_DefOrImpFunKind_to_icl_SymbKind fun_kind index fi_properties
		| is_called_before ei_fun_index calls
			= (symbol_kind, fun_arity, fun_priority, e_state, e_info, cs)
			# cs = { cs & cs_symbol_table = cs_symbol_table <:= (symb_info, { entry & ste_kind = STE_FunctionOrMacro [ ei_fun_index : calls ]})}
			# e_state = { e_state & es_calls = [FunCall ste_index ste_def_level : es_calls ]}
			= (symbol_kind, fun_arity, fun_priority, e_state, e_info, cs)
	determine_info_of_symbol entry=:{ste_kind=STE_DclMacroOrLocalMacroFunction calls,ste_index,ste_def_level} symb_info
				e_input=:{ei_fun_index, ei_mod_index} e_state=:{es_calls} e_info cs=:{cs_symbol_table}
		# (macro_def,e_info) = e_info!ef_macro_defs.[ei_mod_index,ste_index]
		# {fun_ident,fun_arity,fun_kind,fun_priority,fun_info={fi_properties}}=macro_def 
		# index = { glob_object = ste_index, glob_module = ei_mod_index }
		# symbol_kind = convert_DefOrImpFunKind_to_dcl_SymbKind fun_kind index fi_properties
		| is_called_before ei_fun_index calls
			= (symbol_kind, fun_arity, fun_priority, e_state, e_info, cs)
			# cs = { cs & cs_symbol_table = cs_symbol_table <:= (symb_info, { entry & ste_kind = STE_DclMacroOrLocalMacroFunction [ ei_fun_index : calls ]})}
			# e_state = { e_state & es_calls = [MacroCall ei_mod_index ste_index ste_def_level : es_calls ]}
			= (symbol_kind, fun_arity, fun_priority, e_state, e_info, cs)
	determine_info_of_symbol entry=:{ste_kind=STE_Imported (STE_DclMacroOrLocalMacroFunction calls) macro_mod_index,ste_index,ste_def_level} symb_info
				e_input=:{ei_fun_index} e_state=:{es_calls} e_info cs=:{cs_symbol_table}
		# (macro_def,e_info) = e_info!ef_macro_defs.[macro_mod_index,ste_index]
		# {fun_ident,fun_arity,fun_kind,fun_priority,fun_info={fi_properties}}=macro_def 
		# index = { glob_object = ste_index, glob_module = macro_mod_index }
		# symbol_kind = convert_DefOrImpFunKind_to_dcl_SymbKind fun_kind index fi_properties
		| is_called_before ei_fun_index calls
			= (symbol_kind, fun_arity, fun_priority, e_state, e_info, cs)
			# cs = { cs & cs_symbol_table = cs_symbol_table <:= (symb_info, { entry & ste_kind = STE_Imported (STE_DclMacroOrLocalMacroFunction [ ei_fun_index : calls ]) macro_mod_index})}
			# e_state = { e_state & es_calls = [MacroCall macro_mod_index ste_index ste_def_level : es_calls ]}
			= (symbol_kind, fun_arity, fun_priority, e_state, e_info, cs)
	determine_info_of_symbol entry=:{ste_kind=STE_Imported STE_DclFunction mod_index,ste_index} symb_index e_input e_state=:{es_calls} e_info=:{ef_is_macro_fun} cs
		# ({ft_type={st_arity},ft_priority}, e_info) = e_info!ef_modules.[mod_index].dcl_functions.[ste_index]
		# kind = SK_Function { glob_object = ste_index, glob_module = mod_index }
		| not ef_is_macro_fun
			= (kind, st_arity, ft_priority, e_state, e_info, cs)
		| dcl_fun_is_called_before ste_index mod_index es_calls
			= (kind, st_arity, ft_priority, e_state, e_info , cs)
			# e_state = { e_state & es_calls = [DclFunCall mod_index ste_index : es_calls ]}
			= (kind, st_arity, ft_priority, e_state, e_info, cs)
	determine_info_of_symbol entry=:{ste_kind=STE_Imported kind mod_index,ste_index} symb_index e_input e_state e_info=:{ef_modules} cs
		# (mod_def, ef_modules) = ef_modules![mod_index]
		# (kind, arity, priority) = ste_kind_to_symbol_kind kind ste_index mod_index mod_def
		= (kind, arity, priority, e_state, { e_info & ef_modules = ef_modules }, cs)
	where
		ste_kind_to_symbol_kind :: !STE_Kind !Index !Index !DclModule -> (!SymbKind, !Int, !Priority);
		ste_kind_to_symbol_kind STE_Member def_index mod_index {dcl_common={com_member_defs}}
			# {me_type={st_arity},me_priority} = com_member_defs.[def_index]
			= (SK_OverloadedFunction { glob_object = def_index, glob_module = mod_index }, st_arity, me_priority)
		ste_kind_to_symbol_kind STE_Constructor def_index mod_index {dcl_common={com_cons_defs}}
			# {cons_type={st_arity,st_args,st_context},cons_priority,cons_number} = com_cons_defs.[def_index]
			| cons_number <> -2
				| isEmpty st_context && no_TFAC_argument st_args
					= (SK_Constructor {glob_object = def_index, glob_module = mod_index}, st_arity, cons_priority)
					= (SK_OverloadedConstructor {glob_object = def_index, glob_module = mod_index}, st_arity, cons_priority)
				= (SK_NewTypeConstructor {gi_index = def_index, gi_module = mod_index}, st_arity, cons_priority)
	determine_info_of_symbol {ste_kind=STE_Member, ste_index} _ e_input=:{ei_mod_index} e_state e_info=:{ef_member_defs} cs
		# ({me_type={st_arity},me_priority}, ef_member_defs) = ef_member_defs![ste_index]
		= (SK_OverloadedFunction { glob_object = ste_index, glob_module = ei_mod_index}, st_arity, me_priority,
				e_state, { e_info & ef_member_defs = ef_member_defs }, cs)
	determine_info_of_symbol {ste_kind=STE_Constructor, ste_index} _ e_input=:{ei_mod_index} e_state e_info cs
		# ({cons_type={st_arity,st_args,st_context},cons_priority,cons_number}, e_info) = e_info!ef_cons_defs.[ste_index]
		| cons_number <> -2
			| isEmpty st_context && no_TFAC_argument st_args
				= (SK_Constructor {glob_object = ste_index, glob_module = ei_mod_index}, st_arity, cons_priority, e_state, e_info, cs)
				= (SK_OverloadedConstructor {glob_object = ste_index, glob_module = ei_mod_index}, st_arity, cons_priority, e_state, e_info, cs)
			= (SK_NewTypeConstructor {gi_index = ste_index, gi_module = ei_mod_index}, st_arity, cons_priority, e_state, e_info, cs)
	determine_info_of_symbol {ste_kind=STE_DclFunction, ste_index} _ e_input=:{ei_mod_index} e_state=:{es_calls} e_info=:{ef_is_macro_fun} cs
		# ({ft_type={st_arity},ft_priority}, e_info) = e_info!ef_modules.[ei_mod_index].dcl_functions.[ste_index]
		# kind = SK_Function { glob_object = ste_index, glob_module = ei_mod_index }
		| not ef_is_macro_fun
			= (kind, st_arity, ft_priority, e_state, e_info, cs)
		| dcl_fun_is_called_before ste_index ei_mod_index es_calls
			= (kind, st_arity, ft_priority, e_state, e_info, cs)
			# e_state = { e_state & es_calls = [DclFunCall ei_mod_index ste_index : es_calls ]}
			= (kind, st_arity, ft_priority, e_state, e_info, cs)

	convert_DefOrImpFunKind_to_icl_SymbKind FK_Macro index fi_properties
		= SK_IclMacro index.glob_object;
	convert_DefOrImpFunKind_to_icl_SymbKind _ index fi_properties
		| fi_properties bitand FI_IsMacroFun <> 0
			= SK_LocalMacroFunction index.glob_object
			= SK_Function index

checkQualifiedIdentExpression free_vars module_id ident_name is_expr_list e_input=:{ei_fun_index,ei_mod_index} e_state e_info cs
	# (found,{decl_kind,decl_ident,decl_index},cs) = search_qualified_ident module_id ident_name ExpressionNameSpaceN cs
	| not found
		= (EE, free_vars, e_state, e_info, cs)
		= case decl_kind of
			STE_Imported STE_DclFunction mod_index
				# ({ft_type={st_arity},ft_priority}, e_info) = e_info!ef_modules.[mod_index].dcl_functions.[decl_index]
				# kind = SK_Function { glob_object = decl_index, glob_module = mod_index }
				# symbol = { symb_ident = decl_ident, symb_kind = kind }
				# (app_expr, e_state) = build_application_or_constant_for_function symbol st_arity ft_priority e_state
				| not e_info.ef_is_macro_fun || dcl_fun_is_called_before decl_index mod_index e_state.es_calls
					-> (app_expr, free_vars, e_state, e_info, cs)
					# e_state = { e_state & es_calls = [DclFunCall mod_index decl_index : e_state.es_calls ]}
					-> (app_expr, free_vars, e_state, e_info, cs)
			STE_Imported STE_Constructor mod_index
				# ({cons_type={st_arity,st_args,st_context},cons_priority,cons_number}, e_info) = e_info!ef_modules.[mod_index].dcl_common.com_cons_defs.[decl_index]
				| cons_number <> -2
					| isEmpty st_context
						| no_TFAC_argument st_args
							# kind = SK_Constructor { glob_object = decl_index, glob_module = mod_index }
							  symbol = { symb_ident = decl_ident, symb_kind = kind }
							  (app_expr,e_state) = build_application_or_constant_for_function symbol st_arity cons_priority e_state
							-> (app_expr, free_vars, e_state, e_info, cs)
							# kind = SK_OverloadedConstructor { glob_object = decl_index, glob_module = mod_index }
							  symbol = { symb_ident = decl_ident, symb_kind = kind }
							  (app_expr,e_state) = build_application_or_constant_for_function symbol st_arity cons_priority e_state
							-> (app_expr, free_vars, e_state, e_info, cs)
						# kind = SK_OverloadedConstructor { glob_object = decl_index, glob_module = mod_index }
						  symbol = { symb_ident = decl_ident, symb_kind = kind }
						  app_expr = build_application_or_constant_for_constructor symbol st_arity cons_priority
						-> (app_expr, free_vars, e_state, e_info, cs)
					# kind = SK_NewTypeConstructor { gi_index = decl_index, gi_module = mod_index }
					# symbol = { symb_ident = decl_ident, symb_kind = kind }
					# app_expr = build_application_or_constant_for_constructor symbol st_arity cons_priority
					-> (app_expr, free_vars, e_state, e_info, cs)
			STE_Imported STE_Member mod_index
				# ({me_type={st_arity},me_priority}, e_info) = e_info!ef_modules.[mod_index].dcl_common.com_member_defs.[decl_index]
				# kind = SK_OverloadedFunction { glob_object = decl_index, glob_module = mod_index }
				# symbol = { symb_ident = decl_ident, symb_kind = kind }
				# (app_expr, e_state) = build_application_or_constant_for_function symbol st_arity me_priority e_state
				-> (app_expr, free_vars, e_state, e_info, cs)
			STE_Imported (STE_DclMacroOrLocalMacroFunction _) mod_index
				# (macro_def,e_info) = e_info!ef_macro_defs.[mod_index,decl_index]
				# {fun_ident,fun_arity,fun_kind,fun_priority,fun_info={fi_properties}}=macro_def 
				# index = { glob_object = decl_index, glob_module = mod_index }
				# symbol_kind = convert_DefOrImpFunKind_to_dcl_SymbKind fun_kind index fi_properties
				# (e_state,cs) = add_call e_state decl_ident.id_info cs
					with
						add_call e_state=:{es_calls} symbol_table_ptr cs
							# (entry=:{ste_kind,ste_index,ste_def_level},cs_symbol_table) = readPtr symbol_table_ptr cs.cs_symbol_table
							# cs = {cs & cs_symbol_table=cs_symbol_table}
							= case ste_kind of
								/* also imported unqualified */
								STE_Imported (STE_DclMacroOrLocalMacroFunction calls) ste_mod_index
							 		| ste_index==decl_index && ste_mod_index==mod_index
										| is_called_before ei_fun_index calls
											-> (e_state,cs)
											# entry = {entry & ste_kind = STE_DclMacroOrLocalMacroFunction [ ei_fun_index : calls ]}
											# cs = {cs & cs_symbol_table = writePtr symbol_table_ptr entry cs.cs_symbol_table}
											-> ({e_state & es_calls = [MacroCall ste_mod_index ste_index ste_def_level : es_calls ]},cs)
								/* also imported unqualified */
								STE_DclMacroOrLocalMacroFunction calls
							 		| ste_index==decl_index && mod_index==ei_mod_index
										| is_called_before ei_fun_index calls
											-> (e_state,cs)
											# entry = {entry & ste_kind = STE_DclMacroOrLocalMacroFunction [ ei_fun_index : calls ]}
											# cs = {cs & cs_symbol_table = writePtr symbol_table_ptr entry cs.cs_symbol_table}
											-> ({e_state & es_calls = [MacroCall ei_mod_index ste_index ste_def_level : es_calls ]},cs)
								_
									| macro_is_called_before decl_index mod_index es_calls
										-> (e_state,cs)
										-> ({ e_state & es_calls = [MacroCall mod_index decl_index (-1) : es_calls ]},cs)

						macro_is_called_before decl_index mod_index []
							= False
						macro_is_called_before decl_index mod_index [MacroCall macro_mod_index macro_index level:calls]
							= (decl_index==macro_index && mod_index==macro_mod_index && level==(-1)) || macro_is_called_before decl_index mod_index calls
						macro_is_called_before decl_index mod_index [_:calls]
							= macro_is_called_before decl_index mod_index calls
				# symbol = { symb_ident = decl_ident, symb_kind = symbol_kind }
				# (app_expr, e_state) = build_application_or_constant_for_function symbol fun_arity fun_priority e_state
				-> (app_expr, free_vars, e_state, e_info, cs)
			_
				-> (EE, free_vars, e_state, e_info, { cs & cs_error = checkError ("'"+++module_id.id_name+++"'."+++ident_name) "not imported" cs.cs_error })
	where
		build_application_or_constant_for_function symbol arity priority e_state
	  		| is_expr_list
				= (Constant symbol arity priority, e_state)
				# (new_info_ptr, es_expr_heap) = newPtr EI_Empty e_state.es_expr_heap
				# app = { app_symb = symbol , app_args = [], app_info_ptr = new_info_ptr }
				= (App app, { e_state & es_expr_heap = es_expr_heap })

		build_application_or_constant_for_constructor symbol arity priority
	  		| is_expr_list
				= Constant symbol arity priority
				= App { app_symb = symbol , app_args = [], app_info_ptr = nilPtr }

convert_DefOrImpFunKind_to_dcl_SymbKind FK_Macro index fi_properties
	= SK_DclMacro index;
convert_DefOrImpFunKind_to_dcl_SymbKind _ index fi_properties
	| fi_properties bitand FI_IsMacroFun <> 0
		= SK_LocalDclMacroFunction index
		= SK_Function index

is_called_before caller_index []
	= False
is_called_before caller_index [called_index : calls]
	= caller_index == called_index || is_called_before caller_index calls

dcl_fun_is_called_before ste_index mod_index []
	= False
dcl_fun_is_called_before ste_index mod_index [DclFunCall dcl_fun_mod_index dcl_fun_index:calls]
	= (ste_index==dcl_fun_index && mod_index==dcl_fun_mod_index) || dcl_fun_is_called_before ste_index mod_index calls
dcl_fun_is_called_before ste_index mod_index [_:calls]
	= dcl_fun_is_called_before ste_index mod_index calls

checkPattern :: !ParsedExpr !(Optional (Bind Ident VarInfoPtr)) !PatternInput !(![Ident], ![ArrayPattern]) !*PatternState !*ExpressionInfo !*CheckState
									-> (!AuxiliaryPattern, !(![Ident], ![ArrayPattern]), !*PatternState, !*ExpressionInfo, !*CheckState)
checkPattern (PE_List [exp]) opt_var p_input accus ps e_info cs=:{cs_symbol_table}
	= case exp of
		PE_Ident ident
			-> checkIdentPattern cIsNotInExpressionList ident opt_var p_input accus ps e_info cs
		PE_QualifiedIdent module_id ident_name
			-> checkQualifiedIdentPattern cIsNotInExpressionList module_id ident_name opt_var p_input accus ps e_info cs
		_
			-> checkPattern exp opt_var p_input accus ps e_info cs

checkPattern (PE_List [exp1, exp2 : exps]) opt_var p_input accus ps e_info cs
	# (exp_pat, accus, ps, e_info, cs) = check_pattern exp1 p_input accus ps e_info cs
	= check_patterns [exp_pat] exp2 exps opt_var p_input accus ps e_info cs
	where
		check_patterns left middle [] opt_var p_input=:{pi_mod_index} accus ps e_info cs
			# (mid_pat, accus, ps, e_info, cs) = checkPattern middle No p_input accus ps e_info cs
			  (pat, ps, e_info, cs) = combine_patterns pi_mod_index opt_var [mid_pat : left] [] 0 ps e_info cs
			= (pat, accus, ps, e_info, cs)
		check_patterns left middle [right:rest] opt_var p_input=:{pi_mod_index} accus ps e_info cs
			# (mid_pat, accus, ps, e_info, cs) = check_pattern middle p_input accus ps e_info cs
			= case mid_pat of
				AP_Constant kind constant=:{glob_object={ds_arity,ds_ident}} prio
					| ds_arity == 0
						# (pattern, ps, e_info, cs) = buildPattern pi_mod_index kind constant [] No ps e_info cs
						-> check_patterns [pattern: left] right rest opt_var p_input accus ps e_info cs
					| is_infix_constructor prio
						# (left_arg, ps, e_info, cs) = combine_patterns pi_mod_index No left [] 0 ps e_info cs
						  (right_pat, accus, ps, e_info, cs) = check_pattern right p_input accus ps e_info cs
						-> check_infix_pattern [] left_arg kind constant prio [right_pat] rest
									opt_var p_input accus ps e_info cs
						-> (AP_Empty, accus, ps, e_info,
								{ cs & cs_error = checkError ds_ident "arguments of constructor are missing" cs.cs_error })
				_
					-> check_patterns [mid_pat : left] right rest opt_var p_input accus ps e_info cs

		check_pattern (PE_Ident id) p_input accus ps e_info cs
			= checkIdentPattern cIsInExpressionList id No p_input accus ps e_info cs
		check_pattern (PE_QualifiedIdent module_id ident_name) p_input accus ps e_info cs
			= checkQualifiedIdentPattern cIsInExpressionList module_id ident_name No p_input accus ps e_info cs
		check_pattern expr p_input accus ps e_info cs
			= checkPattern expr No p_input accus ps e_info cs
		
	 	check_infix_pattern left_args left kind cons prio middle [] opt_var p_input=:{pi_mod_index} accus ps e_info cs
			# (middle_pat, ps, e_info, cs) = combine_patterns pi_mod_index No middle [] 0 ps e_info cs
			  (pattern, ps, e_info, cs) = buildPattern pi_mod_index kind cons [left,middle_pat] opt_var ps e_info cs
			  (pattern, ps, e_info, cs) = build_final_pattern pi_mod_index left_args pattern ps e_info cs
			= (pattern, accus, ps, e_info, cs)
	 	check_infix_pattern left_args left kind cons prio middle [right] opt_var  p_input=:{pi_mod_index} accus ps e_info cs
			# (right_pat, accus, ps, e_info, cs) = checkPattern right No p_input accus ps e_info cs
			  (right_arg, ps, e_info, cs) = combine_patterns pi_mod_index No [right_pat : middle] [] 0 ps e_info cs
			  (pattern, ps, e_info, cs) = buildPattern pi_mod_index kind cons [left,right_arg] opt_var ps e_info cs
			  (pattern, ps, e_info, cs) = build_final_pattern pi_mod_index left_args pattern ps e_info cs
			= (pattern, accus, ps, e_info, cs)
	 	check_infix_pattern left_args left kind1 cons1 prio1 middle [inf_cons, arg : rest] opt_var p_input=:{pi_mod_index} accus ps e_info cs
			# (inf_cons_pat, accus, ps, e_info, cs) = check_pattern inf_cons p_input accus ps e_info cs
			= case inf_cons_pat of
				AP_Constant kind2 cons2=:{glob_object={ds_ident,ds_arity}} prio2
					| ds_arity == 0
						# (middle_pat, ps, e_info, cs) = combine_patterns pi_mod_index No middle [] 0 ps e_info cs
						  (pattern2, ps, e_info, cs) = buildPattern pi_mod_index kind2 cons2 [] No ps e_info cs
						  (pattern1, ps, e_info, cs) = buildPattern pi_mod_index kind1 cons1 [left,middle_pat] No ps e_info cs
						  (pattern1, ps, e_info, cs) = build_final_pattern pi_mod_index left_args pattern1 ps e_info cs
						-> check_patterns [pattern2,pattern1] arg rest opt_var p_input accus ps e_info cs
					| is_infix_constructor prio2
						# optional_prio = determinePriority prio1 prio2
						-> case optional_prio of
							Yes priority
								# (arg_pat, accus, ps, e_info, cs) = check_pattern arg p_input accus ps e_info cs
								| priority
									# (middle_pat, ps, e_info, cs) = combine_patterns pi_mod_index No middle [] 0 ps e_info cs
								      (pattern, ps, e_info, cs) = buildPattern pi_mod_index kind1 cons1 [left,middle_pat] No ps e_info cs
								      (left_args, pattern, ps, e_info, cs) = build_left_pattern pi_mod_index left_args prio2 pattern ps e_info cs
									-> check_infix_pattern left_args pattern kind2 cons2 prio2 [arg_pat] rest opt_var p_input accus ps e_info cs 
									# (middle_pat, ps, e_info, cs) = combine_patterns pi_mod_index No middle [] 0 ps e_info cs
									-> check_infix_pattern [(kind1, cons1, prio1, left) : left_args]
									  				middle_pat kind2 cons2 prio2 [arg_pat] rest No p_input accus ps e_info cs
							No
								-> (AP_Empty, accus, ps, e_info, { cs & cs_error = checkError ds_ident "conflicting priorities" cs.cs_error })
						-> (AP_Empty, accus, ps, e_info, { cs & cs_error = checkError ds_ident "arguments of constructor are missing" cs.cs_error })
				_
					-> check_infix_pattern left_args left kind1 cons1 prio1 [inf_cons_pat : middle] [arg : rest] opt_var p_input accus ps e_info cs 

		is_infix_constructor (Prio _ _) = True
		is_infix_constructor _ = False

		build_left_pattern mod_index [] _ result_pattern ps e_info cs
			= ([], result_pattern, ps, e_info, cs)		
		build_left_pattern mod_index la=:[(kind, cons, priol, left) : left_args] prior result_pattern ps e_info cs
			# optional_prio = determinePriority priol prior
			= case optional_prio of
				Yes priority
					| priority
						# (result_pattern,  ps, e_info, cs) = buildPattern mod_index kind cons [left,result_pattern] No ps e_info cs
						-> build_left_pattern mod_index left_args prior result_pattern ps e_info cs
						-> (la, result_pattern,  ps, e_info, cs)
				No
					-> (la, result_pattern,  ps, e_info,{ cs & cs_error = checkError cons.glob_object.ds_ident "conflicting priorities" cs.cs_error })

		build_final_pattern mod_index [] result_pattern ps e_info cs
			= (result_pattern,  ps, e_info, cs)		
		build_final_pattern mod_index [(kind, cons, priol, left) : left_appls] result_pattern ps e_info cs
			# (result_pattern, ps, e_info, cs) = buildPattern mod_index kind cons [left,result_pattern] No ps e_info cs
			= build_final_pattern mod_index left_appls result_pattern ps e_info cs

		combine_patterns mod_index opt_var [first_expr] args nr_of_args ps e_info cs
			= case first_expr of
				AP_Constant kind constant=:{glob_object={ds_ident,ds_arity}} _
					| ds_arity == nr_of_args || (case kind of
												  APK_Macro _ -> True
												  _ -> False)
						# (pattern, ps, e_info, cs) = buildPattern mod_index kind constant args opt_var ps e_info cs
						-> (pattern, ps, e_info, cs)
						-> (AP_Empty, ps, e_info, { cs & cs_error = checkError ds_ident "used with wrong arity" cs.cs_error})
				_
					| nr_of_args == 0
						-> (first_expr, ps, e_info, cs)
						-> (first_expr, ps, e_info, { cs & cs_error = checkError "<pattern>" "(curried) application not allowed " cs.cs_error })
		combine_patterns mod_index opt_var [rev_arg : rev_args] args arity ps e_info cs
			= combine_patterns mod_index opt_var rev_args [rev_arg : args] (inc arity) ps e_info cs

checkPattern (PE_DynamicPattern pattern type) opt_var p_input accus ps e_info cs
	# (dyn_pat, accus, ps, e_info, cs) = checkPattern pattern No p_input accus ps e_info cs
	= (AP_Dynamic dyn_pat type opt_var, accus, ps, e_info, { cs & cs_x.x_needed_modules = cs.cs_x.x_needed_modules bitor cNeedStdDynamic })

checkPattern (PE_Basic basic_value) opt_var p_input accus ps e_info cs
	= (AP_Basic basic_value opt_var, accus, ps, e_info, cs)

checkPattern (PE_Tuple tuple_args) opt_var p_input accus ps e_info cs
	# (patterns, arity, accus, ps, e_info, cs) = check_tuple_patterns tuple_args p_input accus ps e_info cs
	  (tuple_symbol, cs) = getPredefinedGlobalSymbol (GetTupleConsIndex arity) PD_PredefinedModule STE_Constructor arity cs
	# ({cons_type_index}, e_info) = e_info!ef_modules.[tuple_symbol.glob_module].dcl_common.com_cons_defs.[tuple_symbol.glob_object.ds_index]
	# global_type_index = {gi_module = cPredefinedModuleIndex, gi_index = cons_type_index} 
	= (AP_Algebraic tuple_symbol global_type_index patterns opt_var, accus, ps, e_info, cs)
where
	check_tuple_patterns [] p_input accus ps e_info cs
		= ([], 0, accus, ps, e_info, cs)
	check_tuple_patterns [expr : exprs] p_input accus ps e_info cs
		# (pattern, accus, ps, e_info, cs) = checkPattern expr No p_input accus ps e_info cs
		  (patterns, length, accus, ps, e_info, cs) = check_tuple_patterns exprs p_input accus ps e_info cs
		= ([pattern : patterns], inc length, accus, ps, e_info, cs)

checkPattern (PE_Record record opt_type fields) opt_var p_input=:{pi_mod_index, pi_is_node_pattern} accus=:(var_env, array_patterns) ps e_info cs
	# (opt_record_and_fields, e_info, cs) = checkFields pi_mod_index fields opt_type e_info cs
	= case opt_record_and_fields of
		Yes (record_symbol, type_index, new_fields)
			# (patterns, (var_env, array_patterns, ps, e_info, cs)) = mapSt (check_field_pattern p_input) new_fields (var_env, array_patterns, ps, e_info, cs)
			  (patterns, ps_var_heap) = bind_opt_record_variable opt_var pi_is_node_pattern patterns new_fields ps.ps_var_heap
			  global_type_index = {gi_module = record_symbol.glob_module, gi_index = type_index}
			-> (AP_Algebraic record_symbol global_type_index patterns opt_var, (var_env, array_patterns), {ps & ps_var_heap = ps_var_heap}, e_info, cs)
		No
			-> (AP_Empty, accus, ps, e_info, cs)
where

	check_field_pattern p_input=:{pi_def_level} {bind_src = PE_Empty, bind_dst = {glob_object={fs_var}}} 
						(var_env, array_patterns, ps, e_info, cs)
		# (entry, cs_symbol_table) = readPtr fs_var.id_info cs.cs_symbol_table
		# (new_info_ptr, ps_var_heap) = newPtr VI_Empty ps.ps_var_heap
		  cs = checkPatternVariable pi_def_level entry fs_var new_info_ptr { cs & cs_symbol_table = cs_symbol_table }
		= (AP_Variable fs_var new_info_ptr No, ([ fs_var : var_env ], array_patterns, { ps & ps_var_heap = ps_var_heap }, e_info, cs))
	check_field_pattern p_input {bind_src = PE_WildCard, bind_dst={glob_object={fs_var}}} (var_env, array_patterns, ps, e_info, cs)
		# (new_info_ptr, ps_var_heap) = newPtr VI_Empty ps.ps_var_heap
		= (AP_WildCard (Yes { bind_src = fs_var, bind_dst = new_info_ptr}), (var_env, array_patterns, { ps & ps_var_heap = ps_var_heap }, e_info, cs))
	check_field_pattern p_input {bind_src,bind_dst} (var_env, array_patterns, ps, e_info, cs)
		# (pattern, (var_env, array_patterns), ps, e_info, cs) = checkPattern bind_src No p_input (var_env, array_patterns) ps e_info cs
		= (pattern, (var_env, array_patterns, ps, e_info, cs))

	add_bound_variable (AP_Algebraic symbol index patterns No) {bind_dst = {glob_object={fs_var}}} ps_var_heap
		# (new_info_ptr, ps_var_heap) = newPtr VI_Empty ps_var_heap
		= (AP_Algebraic symbol index patterns (Yes { bind_src = fs_var, bind_dst = new_info_ptr}), ps_var_heap)
	add_bound_variable (AP_Basic bas_val No) {bind_dst = {glob_object={fs_var}}} ps_var_heap
		# (new_info_ptr, ps_var_heap) = newPtr VI_Empty ps_var_heap
		= (AP_Basic bas_val (Yes { bind_src = fs_var, bind_dst = new_info_ptr}), ps_var_heap)
	add_bound_variable (AP_NewType symbol index pattern No) {bind_dst = {glob_object={fs_var}}} ps_var_heap
		# (new_info_ptr, ps_var_heap) = newPtr VI_Empty ps_var_heap
		= (AP_NewType symbol index pattern (Yes { bind_src = fs_var, bind_dst = new_info_ptr}), ps_var_heap)
	add_bound_variable (AP_Dynamic dynamic_pattern dynamic_type No) {bind_dst = {glob_object={fs_var}}} ps_var_heap
		# (new_info_ptr, ps_var_heap) = newPtr VI_Empty ps_var_heap
		= (AP_Dynamic dynamic_pattern dynamic_type (Yes { bind_src = fs_var, bind_dst = new_info_ptr}), ps_var_heap)
	add_bound_variable pattern _ ps_var_heap
		= (pattern, ps_var_heap)

	add_bound_variables [] _ var_heap
		= ([] , var_heap)
	add_bound_variables [ap : aps] [field : fields] var_heap
		# (ap, var_heap) = add_bound_variable ap field var_heap
		  (aps, var_heap) = add_bound_variables aps fields var_heap
		= ([ap : aps], var_heap)

	bind_opt_record_variable (Yes {bind_dst}) False patterns fields var_heap
		# (patterns, var_heap) = add_bound_variables patterns fields var_heap
		= (patterns, var_heap <:= (bind_dst, VI_Record patterns))
	bind_opt_record_variable no is_node_pattern patterns _ var_heap
		= (patterns, var_heap)

checkPattern (PE_Bound bind) opt_var p_input accus ps e_info cs
	= checkBoundPattern bind opt_var p_input accus ps e_info cs

checkPattern (PE_Ident id) opt_var p_input accus ps e_info cs
	= checkIdentPattern cIsNotInExpressionList id opt_var p_input accus ps e_info cs
checkPattern (PE_QualifiedIdent module_id ident_name) opt_var p_input accus ps e_info cs
	= checkQualifiedIdentPattern cIsNotInExpressionList module_id ident_name opt_var p_input accus ps e_info cs

checkPattern PE_WildCard opt_var p_input accus ps e_info cs
	= (AP_WildCard No, accus, ps, e_info, cs)

checkPattern (PE_ArrayPattern selections) opt_var p_input (var_env, array_patterns) ps e_info cs
	# (var_env, ap_selections, ps_var_heap, cs)
			= foldSt (check_array_selection p_input.pi_def_level) selections (var_env, [], ps.ps_var_heap, cs)
	  array_var_ident = case opt_var of 
	  						Yes {bind_src}
	  							-> bind_src
	  						No
	  							-> { id_name = "_a", id_info = nilPtr }
	  (array_var, ps_var_heap) = allocate_free_var array_var_ident ps_var_heap
	= (AP_Variable array_var_ident array_var.fv_info_ptr No, 
		(var_env, [{ ap_opt_var = opt_var, ap_array_var = array_var, ap_selections = ap_selections } :array_patterns]),
		{ ps & ps_var_heap = ps_var_heap }, e_info, cs)
  where
	check_array_selection def_level bind=:{bind_dst} states
		= check_rhs def_level bind (foldSt check_index_expr bind_dst states)
		
	check_index_expr (PE_Ident {id_name}) states
		| isLowerCaseName id_name
			= states
		// further with next alternative
	check_index_expr (PE_Basic (BVI _)) states
			= states
	check_index_expr (PE_Basic (BVInt _)) states
			= states
	check_index_expr _ (var_env, ap_selections, var_heap, cs)
		= (var_env, ap_selections, var_heap, { cs & cs_error = checkError "variable or integer constant expected as index expression" "" cs.cs_error })

	check_rhs def_level {bind_src=PE_Ident ident, bind_dst} (var_env, ap_selections, var_heap, cs)
		| isLowerCaseName ident.id_name
			# (entry,cs_symbol_table) = readPtr ident.id_info cs.cs_symbol_table
			# (rhs_var, var_heap) = allocate_free_var ident var_heap
			  cs = checkPatternVariable def_level entry ident rhs_var.fv_info_ptr { cs & cs_symbol_table = cs_symbol_table }
			= ([ident : var_env], [ { bind_src = rhs_var, bind_dst = bind_dst } : ap_selections], var_heap, cs)
		// further with next alternative
	check_rhs _ _ (var_env, ap_selections, var_heap, cs)
		= (var_env, ap_selections, var_heap, 
			{ cs & cs_error = checkError "variable expected on right hand side of array pattern" "" cs.cs_error })

checkPattern expr opt_var p_input accus ps e_info cs
	= abort "checkPattern: do not know how to handle pattern" ---> expr

checkMacroPatternConstructor macro=:{fun_ident,fun_arity,fun_kind,fun_priority} macro_mod_index mod_index is_dcl_macro is_expr_list ste_index ident opt_var ps e_info cs=:{cs_error}
	| case fun_kind of FK_Macro->True; _ -> False
		| is_expr_list
			# macro_symbol = { glob_object = MakeDefinedSymbol fun_ident ste_index fun_arity, glob_module = macro_mod_index }
	 		= (AP_Constant (APK_Macro is_dcl_macro) macro_symbol fun_priority, ps, e_info, cs)
		| fun_arity == 0
			# (pattern, ps, ef_modules, ef_cons_defs, cs_error)
					= unfoldPatternMacro macro mod_index [] opt_var ps e_info.ef_modules e_info.ef_cons_defs cs_error
			= (pattern, ps, { e_info & ef_modules = ef_modules, ef_cons_defs = ef_cons_defs }, { cs & cs_error = cs_error })
			= (AP_Empty, ps, e_info, { cs & cs_error = checkError ident "not defined" cs_error })
		= (AP_Empty, ps, e_info, { cs & cs_error = checkError fun_ident "not allowed in a pattern" cs_error })

checkQualifiedMacroPatternConstructor macro=:{fun_ident,fun_arity,fun_kind,fun_priority} macro_mod_index mod_index is_dcl_macro is_expr_list ste_index module_name ident_name opt_var ps e_info cs=:{cs_error}
	| case fun_kind of FK_Macro->True; _ -> False
		| is_expr_list
			# macro_symbol = { glob_object = MakeDefinedSymbol fun_ident ste_index fun_arity, glob_module = macro_mod_index }
	 		= (AP_Constant (APK_Macro is_dcl_macro) macro_symbol fun_priority, ps, e_info, cs)
		| fun_arity == 0
			# (pattern, ps, ef_modules, ef_cons_defs, cs_error)
					= unfoldPatternMacro macro mod_index [] opt_var ps e_info.ef_modules e_info.ef_cons_defs cs_error
			= (pattern, ps, { e_info & ef_modules = ef_modules, ef_cons_defs = ef_cons_defs }, { cs & cs_error = cs_error })
			# name="'"+++module_name+++"'."+++ident_name
			= (AP_Empty, ps, e_info, { cs & cs_error = checkError name "not defined" cs_error })
		# name="'"+++module_name+++"'."+++ident_name
		= (AP_Empty, ps, e_info, { cs & cs_error = checkError name "not allowed in a pattern" cs_error })

checkPatternConstructor :: !Index !Bool !SymbolTableEntry !Ident !(Optional (Bind Ident VarInfoPtr))
						   !*PatternState !*ExpressionInfo !*CheckState
	-> (!AuxiliaryPattern, !*PatternState,!*ExpressionInfo,!*CheckState);
checkPatternConstructor _ _ {ste_kind = STE_Empty} ident _  ps e_info cs=:{cs_error}
	= (AP_Empty, ps, e_info, { cs & cs_error = checkError ident "not defined" cs_error })
checkPatternConstructor mod_index is_expr_list {ste_kind = STE_FunctionOrMacro _,ste_index} ident opt_var ps e_info cs=:{cs_x}
	# (macro,ps) = ps!ps_fun_defs.[ste_index]
	= checkMacroPatternConstructor macro cs_x.x_main_dcl_module_n mod_index False is_expr_list ste_index ident opt_var ps e_info cs
checkPatternConstructor mod_index is_expr_list {ste_kind = STE_DclMacroOrLocalMacroFunction _,ste_index} ident opt_var ps e_info cs=:{cs_x}
	# (macro,e_info) = e_info!ef_macro_defs.[mod_index,ste_index]
	= checkMacroPatternConstructor macro mod_index mod_index True is_expr_list ste_index ident opt_var ps e_info cs
checkPatternConstructor mod_index is_expr_list {ste_kind = STE_Imported (STE_DclMacroOrLocalMacroFunction _) macro_module_index,ste_index} ident opt_var ps e_info cs
	# (macro,e_info) = e_info!ef_macro_defs.[macro_module_index,ste_index]
	= checkMacroPatternConstructor macro macro_module_index mod_index True is_expr_list ste_index ident opt_var ps e_info cs
checkPatternConstructor mod_index is_expr_list {ste_index, ste_kind} cons_ident opt_var ps
		e_info=:{ef_cons_defs,ef_modules} cs=:{cs_error}
	# (cons_index, cons_module, cons_arity, cons_priority, cons_type_index, cons_number, ef_cons_defs, ef_modules, cs_error)
			= determine_pattern_symbol mod_index ste_index ste_kind cons_ident.id_name ef_cons_defs ef_modules cs_error
	  e_info = { e_info & ef_cons_defs = ef_cons_defs, ef_modules = ef_modules }
	  cons_symbol = { glob_object = MakeDefinedSymbol cons_ident cons_index cons_arity, glob_module = cons_module }
	| cons_number > -2
		# global_type_index = {gi_module = cons_module, gi_index = cons_type_index}
		| is_expr_list
			= (AP_Constant (APK_Constructor global_type_index) cons_symbol cons_priority, ps, e_info, {cs & cs_error = cs_error})
			| cons_arity == 0
				= (AP_Algebraic cons_symbol global_type_index [] opt_var, ps, e_info, {cs & cs_error = cs_error})
				# cs & cs_error = checkError cons_ident "constructor arguments are missing" cs_error
				= (AP_Algebraic cons_symbol global_type_index [] opt_var, ps, e_info, cs)
	| cons_number == -2
		| is_expr_list
			= (AP_Constant (APK_NewTypeConstructor cons_type_index) cons_symbol cons_priority, ps, e_info, {cs & cs_error = cs_error})
			# cs & cs_error = checkError cons_ident "constructor argument is missing" cs_error
			= (AP_NewType cons_symbol cons_type_index AP_Empty opt_var, ps, e_info, cs)
	// cons_number == -3
		# (type_rhs,e_info)
			= case ste_kind of
				STE_Constructor
					-> e_info!ef_type_defs.[cons_type_index].td_rhs
				_
					-> e_info!ef_modules.[cons_module].dcl_common.com_type_defs.[cons_type_index].td_rhs
		# (AlgConses _ global_type_index) = type_rhs
		| is_expr_list
			= (AP_Constant (APK_Constructor global_type_index) cons_symbol cons_priority, ps, e_info, {cs & cs_error = cs_error})
			| cons_arity == 0
				= (AP_Algebraic cons_symbol global_type_index [] opt_var, ps, e_info, {cs & cs_error = cs_error})
				# cs & cs_error = checkError cons_ident "constructor arguments are missing" cs_error
				= (AP_Algebraic cons_symbol global_type_index [] opt_var, ps, e_info, cs)
where
	determine_pattern_symbol mod_index id_index STE_Constructor id_name cons_defs modules error
		# ({cons_type={st_arity},cons_priority,cons_type_index,cons_number}, cons_defs) = cons_defs![id_index]
		= (id_index, mod_index, st_arity, cons_priority, cons_type_index, cons_number, cons_defs, modules, error)
	determine_pattern_symbol mod_index id_index (STE_Imported STE_Constructor import_mod_index) id_name cons_defs modules error
		# ({dcl_common},modules) = modules![import_mod_index]
		  {cons_type={st_arity},cons_priority,cons_type_index,cons_number} = dcl_common.com_cons_defs.[id_index]
		= (id_index, import_mod_index, st_arity, cons_priority, cons_type_index, cons_number, cons_defs, modules, error)
	determine_pattern_symbol mod_index id_index id_kind id_name cons_defs modules error
		= (id_index, NoIndex, 0, NoPrio, NoIndex, NoIndex, cons_defs, modules, checkError id_name "constructor expected" error)

checkQualifiedPatternConstructor :: !STE_Kind !Index !Ident !{#Char} !{#Char} !Index !Bool !(Optional (Bind Ident VarInfoPtr)) !*PatternState !*ExpressionInfo !*CheckState
	-> (!AuxiliaryPattern, !*PatternState, !*ExpressionInfo, !*CheckState);
checkQualifiedPatternConstructor STE_Empty _ decl_ident module_name ident_name _ _ _  ps e_info cs=:{cs_error}
	# name="'"+++module_name+++"'."+++ident_name
	= (AP_Empty, ps, e_info, { cs & cs_error = checkError name "not defined" cs_error })
checkQualifiedPatternConstructor (STE_FunctionOrMacro _) ste_index decl_ident module_name ident_name mod_index is_expr_list opt_var ps e_info cs=:{cs_x}
	# (macro,ps) = ps!ps_fun_defs.[ste_index]
	= checkQualifiedMacroPatternConstructor macro cs_x.x_main_dcl_module_n mod_index False is_expr_list ste_index module_name ident_name opt_var ps e_info cs
checkQualifiedPatternConstructor (STE_DclMacroOrLocalMacroFunction _) ste_index decl_ident module_name ident_name mod_index is_expr_list opt_var ps e_info cs=:{cs_x}
	# (macro,e_info) = e_info!ef_macro_defs.[mod_index,ste_index]
	= checkQualifiedMacroPatternConstructor macro mod_index mod_index True is_expr_list ste_index module_name ident_name opt_var ps e_info cs
checkQualifiedPatternConstructor (STE_Imported (STE_DclMacroOrLocalMacroFunction _) macro_module_index) ste_index decl_ident module_name ident_name mod_index is_expr_list opt_var ps e_info cs
	# (macro,e_info) = e_info!ef_macro_defs.[macro_module_index,ste_index]
	= checkQualifiedMacroPatternConstructor macro macro_module_index mod_index True is_expr_list ste_index module_name ident_name opt_var ps e_info cs
checkQualifiedPatternConstructor ste_kind ste_index decl_ident module_name ident_name mod_index is_expr_list opt_var ps
		e_info=:{ef_cons_defs,ef_modules} cs=:{cs_error}
	# (cons_index, cons_module, cons_arity, cons_priority, cons_type_index, cons_number, ef_cons_defs, ef_modules, cs_error)
			= determine_pattern_symbol mod_index ste_index ste_kind module_name ident_name ef_cons_defs ef_modules cs_error
	  e_info = { e_info & ef_cons_defs = ef_cons_defs, ef_modules = ef_modules }
	  cons_symbol = { glob_object = MakeDefinedSymbol decl_ident cons_index cons_arity, glob_module = cons_module }
	| cons_number > -2
		# global_type_index = {gi_module = cons_module, gi_index = cons_type_index}
	   	| is_expr_list
			= (AP_Constant (APK_Constructor global_type_index) cons_symbol cons_priority, ps, e_info, {cs & cs_error = cs_error})
			| cons_arity == 0
				= (AP_Algebraic cons_symbol global_type_index [] opt_var, ps, e_info, {cs & cs_error = cs_error})
				# cs & cs_error = checkError ident_name "constructor arguments are missing" cs_error
				= (AP_Algebraic cons_symbol global_type_index [] opt_var, ps, e_info, cs)
	| cons_number == -2
	   	| is_expr_list
			= (AP_Constant (APK_NewTypeConstructor cons_type_index) cons_symbol cons_priority, ps, e_info, {cs & cs_error = cs_error})
			# cs & cs_error = checkError ident_name "constructor argument is missing" cs_error
			= (AP_NewType cons_symbol cons_type_index AP_Empty opt_var, ps, e_info, cs)
	// cons_number == -3
		# (type_rhs,e_info)
			= case ste_kind of
				STE_Constructor
					-> e_info!ef_type_defs.[cons_type_index].td_rhs
				_
					-> e_info!ef_modules.[cons_module].dcl_common.com_type_defs.[cons_type_index].td_rhs
		# (AlgConses _ global_type_index) = type_rhs
	   	| is_expr_list
			= (AP_Constant (APK_Constructor global_type_index) cons_symbol cons_priority, ps, e_info, {cs & cs_error = cs_error})
			| cons_arity == 0
				= (AP_Algebraic cons_symbol global_type_index [] opt_var, ps, e_info, {cs & cs_error = cs_error})
				# cs & cs_error = checkError ident_name "constructor arguments are missing" cs_error
				= (AP_Algebraic cons_symbol global_type_index [] opt_var, ps, e_info, cs)
where
	determine_pattern_symbol mod_index id_index STE_Constructor module_name ident_name cons_defs modules error
		# ({cons_type={st_arity},cons_priority,cons_type_index,cons_number}, cons_defs) = cons_defs![id_index]
		= (id_index, mod_index, st_arity, cons_priority, cons_type_index, cons_number, cons_defs, modules, error)
	determine_pattern_symbol mod_index id_index (STE_Imported STE_Constructor import_mod_index) module_name ident_name cons_defs modules error
		# ({dcl_common},modules) = modules![import_mod_index]
		  {cons_type={st_arity},cons_priority,cons_type_index,cons_number} = dcl_common.com_cons_defs.[id_index]
		= (id_index, import_mod_index, st_arity, cons_priority, cons_type_index, cons_number, cons_defs, modules, error)
	determine_pattern_symbol mod_index id_index id_kind module_name ident_name cons_defs modules error
		= (id_index, NoIndex, 0, NoPrio, NoIndex, NoIndex, cons_defs, modules, checkError ("'"+++module_name+++"'."+++ident_name) "constructor expected" error)

checkBoundPattern {bind_src,bind_dst} opt_var p_input (var_env, array_patterns) ps e_info cs=:{cs_symbol_table}
	| isLowerCaseName bind_dst.id_name
		# (entry, cs_symbol_table) = readPtr bind_dst.id_info cs_symbol_table
		# (new_info_ptr, ps_var_heap) = newPtr VI_Empty ps.ps_var_heap
		  cs = checkPatternVariable p_input.pi_def_level entry bind_dst new_info_ptr { cs & cs_symbol_table = cs_symbol_table }
		  ps = { ps & ps_var_heap = ps_var_heap }
		  new_var_env = [ bind_dst : var_env ]
		= case opt_var of
			Yes bind
				-> checkPattern bind_src (Yes { bind_src = bind_dst, bind_dst = new_info_ptr }) p_input (new_var_env, array_patterns) ps
					 	e_info { cs & cs_error = checkError bind.bind_src "pattern may be bound once only" cs.cs_error }
			No
				-> checkPattern bind_src (Yes { bind_src = bind_dst, bind_dst = new_info_ptr }) p_input (new_var_env, array_patterns) ps e_info cs
	= checkPattern bind_src opt_var p_input (var_env, array_patterns) ps e_info { cs & cs_error = checkError bind_dst "variable expected" cs.cs_error }

checkPatternVariable :: !Level !SymbolTableEntry !Ident !VarInfoPtr !*CheckState -> *CheckState
checkPatternVariable def_level entry=:{ste_def_level,ste_kind} ident=:{id_info} var_info cs=:{cs_symbol_table,cs_error}
	| ste_kind == STE_Empty || def_level > ste_def_level
		# entry = {ste_kind = STE_Variable var_info, ste_index = NoIndex, ste_def_level = def_level, ste_previous = entry, ste_doc = No }
		= { cs & cs_symbol_table = cs_symbol_table <:= (id_info,entry)}
		= { cs & cs_error = checkError ident "(pattern variable) already defined" cs_error }

checkIdentPattern :: !Bool !Ident !(Optional (Bind Ident VarInfoPtr)) !PatternInput !(![Ident], ![ArrayPattern]) !*PatternState !*ExpressionInfo !*CheckState
	-> (!AuxiliaryPattern, !(![Ident], ![ArrayPattern]), !*PatternState, !*ExpressionInfo, !*CheckState)
checkIdentPattern is_expr_list id=:{id_name,id_info} opt_var {pi_def_level, pi_mod_index} accus=:(var_env, array_patterns)
					ps e_info cs=:{cs_symbol_table}
	# (entry, cs_symbol_table) = readPtr id_info cs_symbol_table
	| isLowerCaseName id_name
		# (new_info_ptr, ps_var_heap) = newPtr VI_Empty ps.ps_var_heap
		  cs = checkPatternVariable pi_def_level entry id new_info_ptr { cs & cs_symbol_table = cs_symbol_table }
		= (AP_Variable id new_info_ptr opt_var, ([ id : var_env ], array_patterns), { ps & ps_var_heap = ps_var_heap}, e_info, cs)
		# (pattern, ps, e_info, cs) = checkPatternConstructor pi_mod_index is_expr_list entry id opt_var ps e_info { cs & cs_symbol_table = cs_symbol_table }
		= (pattern, accus, ps, e_info, cs)

checkQualifiedIdentPattern is_expr_list module_id ident_name opt_var {pi_mod_index} accus ps e_info cs
	# (found,{decl_kind,decl_ident,decl_index},cs) = search_qualified_ident module_id ident_name ExpressionNameSpaceN cs
	| not found
		= (AP_Empty, accus, ps, e_info, cs)
		= case decl_kind of
			STE_Imported _ _
				# (pattern, ps, e_info, cs) = checkQualifiedPatternConstructor decl_kind decl_index decl_ident module_id.id_name ident_name pi_mod_index is_expr_list opt_var ps e_info cs
				-> (pattern, accus, ps, e_info, cs)
			_
				-> (AP_Empty, accus, ps, e_info, { cs & cs_error = checkError ("'"+++module_id.id_name+++"'."+++ident_name) "not imported" cs.cs_error })

convertSubPatterns :: [AuxiliaryPattern] Expression  Position  *VarHeap  *ExpressionHeap  u:[ExprInfoPtr]  *CheckState
					   -> *(!.[FreeVar],!Expression,!Position,!*VarHeap,!*ExpressionHeap,!u:[ExprInfoPtr],!*CheckState);
convertSubPatterns [] result_expr pattern_position var_store expr_heap opt_dynamics cs
	= ([], result_expr, pattern_position, var_store, expr_heap, opt_dynamics, cs)
convertSubPatterns [pattern : patterns] result_expr pattern_position var_store expr_heap opt_dynamics cs
	# (var_args, result_expr, pattern_position, var_store, expr_heap, opt_dynamics, cs) 
			= convertSubPatterns patterns result_expr pattern_position var_store expr_heap opt_dynamics cs
	  (var_arg, result_expr, pattern_position, var_store, expr_heap, opt_dynamics, cs)
	  		= convertSubPattern pattern result_expr pattern_position var_store expr_heap opt_dynamics cs
	= ([var_arg : var_args], result_expr, pattern_position, var_store, expr_heap, opt_dynamics, cs)

convertSubPattern :: AuxiliaryPattern Expression  Position  *VarHeap  *ExpressionHeap  u:[ExprInfoPtr]  *CheckState
					   -> *(!FreeVar,!Expression,!Position,!*VarHeap,!*ExpressionHeap,!u:[ExprInfoPtr],!*CheckState);
convertSubPattern (AP_Variable name var_info (Yes {bind_src,bind_dst})) result_expr pattern_position var_store expr_heap opt_dynamics cs
	# (var_expr_ptr, expr_heap) = newPtr EI_Empty expr_heap
	  bound_var = { var_ident = bind_src, var_info_ptr = bind_dst, var_expr_ptr = var_expr_ptr }
	  free_var = { fv_ident = bind_src, fv_info_ptr = bind_dst, fv_def_level = NotALevel, fv_count = 0 }
	  (let_expr, expr_heap)	= buildLetExpression [] [{lb_src = Var bound_var,
	  			lb_dst = { fv_ident = name, fv_info_ptr = var_info, fv_def_level = NotALevel, fv_count = 0 },
	  			lb_position = NoPos }] result_expr NoPos expr_heap
	= (free_var, let_expr, pattern_position, var_store, expr_heap, opt_dynamics, cs)
convertSubPattern (AP_Variable name var_info No) result_expr pattern_position var_store expr_heap opt_dynamics cs
	= ({ fv_ident = name, fv_info_ptr = var_info, fv_def_level = NotALevel, fv_count = 0 }, result_expr, pattern_position, 
		var_store, expr_heap, opt_dynamics, cs)
convertSubPattern (AP_Algebraic cons_symbol global_type_index args opt_var) result_expr pattern_position
					var_store expr_heap opt_dynamics cs
	# (var_args, result_expr, pattern_position, var_store, expr_heap, opt_dynamics, cs)
			= convertSubPatterns args result_expr pattern_position var_store expr_heap opt_dynamics cs
	  ({bind_src,bind_dst}, var_store) = determinePatternVariable opt_var var_store
	  (var_expr_ptr, expr_heap) = newPtr EI_Empty expr_heap
	  (case_expr_ptr, expr_heap) = newPtr EI_Empty expr_heap
	# alg_patterns = [{ ap_symbol = cons_symbol, ap_vars = var_args, ap_expr = result_expr, ap_position = pattern_position }]
	# (case_guards,expr_heap,cs) = make_case_guards cons_symbol global_type_index alg_patterns expr_heap cs
	= ({ fv_ident = bind_src, fv_info_ptr = bind_dst, fv_def_level = NotALevel, fv_count = 0 },
		Case { case_expr = Var { var_ident = bind_src, var_info_ptr = bind_dst, var_expr_ptr = var_expr_ptr },
				case_guards = case_guards, case_default = No, case_ident = No, case_info_ptr = case_expr_ptr,
				case_explicit = cCaseNotExplicit,
				case_default_pos = NoPos },
		NoPos, var_store, expr_heap, opt_dynamics, cs)
convertSubPattern (AP_Basic basic_val opt_var) result_expr pattern_position var_store expr_heap opt_dynamics cs
	# (basic_type, cs) = typeOfBasicValue basic_val cs
	  case_guards = BasicPatterns basic_type [{ bp_value = basic_val, bp_expr = result_expr, bp_position = pattern_position }]
  	  ({bind_src,bind_dst}, var_store) = determinePatternVariable opt_var var_store
	  (var_expr_ptr, expr_heap) = newPtr EI_Empty expr_heap
	  (case_expr_ptr, expr_heap) = newPtr EI_Empty expr_heap
	= ({ fv_ident = bind_src, fv_info_ptr = bind_dst, fv_def_level = NotALevel, fv_count = 0 },
		Case { case_expr = Var { var_ident = bind_src, var_info_ptr = bind_dst, var_expr_ptr = var_expr_ptr },
			  case_guards = case_guards, case_default = No, case_ident = No, case_info_ptr = case_expr_ptr,
			  case_explicit = cCaseNotExplicit,
			  case_default_pos = NoPos},
		NoPos, var_store, expr_heap, opt_dynamics, cs)
convertSubPattern (AP_NewType cons_symbol type_index arg opt_var) result_expr pattern_position
					var_store expr_heap opt_dynamics cs
	# (var_arg, result_expr, pattern_position, var_store, expr_heap, opt_dynamics, cs)
			= convertSubPattern arg result_expr pattern_position var_store expr_heap opt_dynamics cs
	  type_symbol = { gi_module = cons_symbol.glob_module, gi_index = type_index }
	  ({bind_src,bind_dst}, var_store) = determinePatternVariable opt_var var_store
	  (var_expr_ptr, expr_heap) = newPtr EI_Empty expr_heap
	  (case_expr_ptr, expr_heap) = newPtr EI_Empty expr_heap
	# alg_patterns = [{ ap_symbol = cons_symbol, ap_vars = [var_arg], ap_expr = result_expr, ap_position = pattern_position }]
	# case_guards = NewTypePatterns type_symbol alg_patterns
	= ({ fv_ident = bind_src, fv_info_ptr = bind_dst, fv_def_level = NotALevel, fv_count = 0 },
		Case { case_expr = Var { var_ident = bind_src, var_info_ptr = bind_dst, var_expr_ptr = var_expr_ptr },
				case_guards = case_guards, case_default = No, case_ident = No, case_info_ptr = case_expr_ptr,
				case_explicit = cCaseNotExplicit,
				case_default_pos = NoPos },
		NoPos, var_store, expr_heap, opt_dynamics, cs)
convertSubPattern (AP_Dynamic pattern type opt_var) result_expr pattern_position var_store expr_heap opt_dynamics cs
	# (var_arg, result_expr, pattern_position, var_store, expr_heap, opt_dynamics, cs)
			= convertSubPattern pattern result_expr pattern_position var_store expr_heap opt_dynamics cs
 	  ({bind_src,bind_dst}, var_store) = determinePatternVariable opt_var var_store
	  (var_expr_ptr, expr_heap) = newPtr EI_Empty expr_heap
	  (type_case_info_ptr, expr_heap) = newPtr EI_Empty expr_heap
	  (dynamic_info_ptr, expr_heap) = newPtr (EI_DynamicType type opt_dynamics) expr_heap
 	  type_case_patterns = [{ dp_var = var_arg, dp_type = dynamic_info_ptr, dp_rhs = result_expr,
 	  						dp_type_code = TCE_Empty, dp_position = pattern_position }]
	= ({ fv_ident = bind_src, fv_info_ptr = bind_dst, fv_def_level = NotALevel, fv_count = 0 },
		buildTypeCase (Var { var_ident = bind_src, var_info_ptr = bind_dst, var_expr_ptr = var_expr_ptr }) 
							type_case_patterns No type_case_info_ptr cCaseNotExplicit,
		NoPos, var_store, expr_heap, [dynamic_info_ptr], cs)
convertSubPattern (AP_WildCard opt_var) result_expr pattern_position var_store expr_heap opt_dynamics cs
 	# ({bind_src,bind_dst}, var_store) = determinePatternVariable opt_var var_store
	= ({ fv_ident = bind_src, fv_info_ptr = bind_dst, fv_def_level = NotALevel, fv_count = 0  }, result_expr, pattern_position,
		var_store, expr_heap, opt_dynamics, cs)
convertSubPattern AP_Empty result_expr pattern_position var_store expr_heap opt_dynamics cs
	= convertSubPattern (AP_WildCard No) EE pattern_position var_store expr_heap opt_dynamics cs

checkAndTransformPatternIntoBind free_vars [{nd_dst,nd_alts,nd_locals,nd_position} : local_defs] e_input=:{ei_expr_level,ei_mod_index} e_state e_info cs
	# cs = pushErrorAdmin (newPosition {id_name="node definition", id_info=nilPtr} nd_position) cs
	# (bind_src, free_vars, e_state, e_info, cs) = checkRhs free_vars nd_alts nd_locals
			{e_input & ei_expr_level = ei_expr_level + 1} e_state e_info cs	
	  (binds_of_bind, es_var_heap, es_expr_heap, e_info, cs)
			= transfromPatternIntoBind ei_mod_index ei_expr_level nd_dst bind_src nd_position
				e_state.es_var_heap e_state.es_expr_heap e_info cs
	  e_state = { e_state & es_var_heap = es_var_heap, es_expr_heap = es_expr_heap }
	  (binds_of_local_defs, free_vars, e_state, e_info, cs) = checkAndTransformPatternIntoBind free_vars local_defs e_input e_state e_info cs
	= (binds_of_bind ++ binds_of_local_defs, free_vars, e_state, e_info, popErrorAdmin cs)
checkAndTransformPatternIntoBind free_vars [] e_input e_state e_info cs
	= ([], free_vars, e_state, e_info, cs)

transfromPatternIntoBind :: !Index !Level !AuxiliaryPattern !Expression !Position !*VarHeap !*ExpressionHeap !*ExpressionInfo !*CheckState
	-> *(![LetBind], !*VarHeap, !*ExpressionHeap,  !*ExpressionInfo, !*CheckState)
transfromPatternIntoBind mod_index def_level (AP_Variable name var_info (Yes {bind_src,bind_dst})) src_expr position var_store expr_heap e_info cs
	# (var_expr_ptr, expr_heap) = newPtr EI_Empty expr_heap
	  bound_var = {var_ident = bind_src, var_info_ptr = bind_dst, var_expr_ptr = var_expr_ptr}
	  free_var = {fv_ident = bind_src, fv_info_ptr = bind_dst, fv_def_level = NotALevel, fv_count = 0}
	  bind1 = {lb_src = src_expr, lb_dst = free_var, lb_position = position}
	  bind2 = {lb_src = Var bound_var, lb_dst = {fv_ident = name, fv_info_ptr = var_info, fv_def_level = NotALevel, fv_count = 0}, lb_position = position}
	= ([bind1,bind2], var_store, expr_heap, e_info, cs)
transfromPatternIntoBind mod_index def_level (AP_Variable name var_info No) src_expr position var_store expr_heap e_info cs
	# bind = {lb_src = src_expr, lb_dst = { fv_ident = name, fv_info_ptr = var_info, fv_def_level = def_level, fv_count = 0 }, lb_position = position }
	= ([bind], var_store, expr_heap, e_info, cs)
transfromPatternIntoBind mod_index def_level (AP_Algebraic cons_symbol=:{glob_module,glob_object=ds_cons=:{ds_arity, ds_index, ds_ident}} global_type_index args opt_var)
		src_expr position var_store expr_heap e_info=:{ef_type_defs,ef_modules} cs
	# (src_expr, opt_var_bind, var_store, expr_heap) = bind_opt_var opt_var src_expr position var_store expr_heap
	| ds_arity == 0
		= ([], var_store, expr_heap, e_info, { cs & cs_error = checkError ds_ident "constant not allowed in a node pattern" cs.cs_error})
	# (is_tuple, cs) = is_tuple_symbol glob_module ds_index cs
	| is_tuple
		# (tuple_var, tuple_bind, var_store, expr_heap) = bind_match_expr src_expr opt_var_bind position def_level var_store expr_heap
		= transform_sub_patterns mod_index def_level args ds_cons 0 tuple_var tuple_bind position var_store expr_heap e_info cs
		# ({td_rhs}, ef_type_defs, ef_modules) = get_type_def mod_index global_type_index ef_type_defs ef_modules
		  e_info = { e_info & ef_type_defs = ef_type_defs, ef_modules = ef_modules }
		= case td_rhs of
			RecordType {rt_fields}
				| size rt_fields == 1
					-> transform_sub_patterns_of_record mod_index def_level args rt_fields glob_module 0
							src_expr opt_var_bind position var_store expr_heap e_info cs
					# (record_var, record_bind, var_store, expr_heap)
						= bind_match_expr src_expr opt_var_bind position def_level var_store expr_heap
					-> transform_sub_patterns_of_record mod_index def_level args rt_fields glob_module 0
							record_var record_bind position var_store expr_heap e_info cs
			_
				| ds_arity == 1
		  			# (binds, var_store, expr_heap, e_info, cs)
						= transfromPatternIntoBind mod_index def_level (hd args) (MatchExpr cons_symbol src_expr)
								position var_store expr_heap e_info cs
					-> (opt_var_bind ++ binds, var_store, expr_heap, e_info, cs)
					# (tuple_cons, cs) = getPredefinedGlobalSymbol (GetTupleConsIndex ds_arity) PD_PredefinedModule STE_Constructor ds_arity cs
					# (src_expr,expr_heap,cs) = add_decons_call_for_overloaded_lists glob_module ds_index src_expr expr_heap cs
					# (match_var, match_bind, var_store, expr_heap)
						=  bind_match_expr (MatchExpr cons_symbol src_expr) opt_var_bind position def_level var_store expr_heap
					-> transform_sub_patterns mod_index def_level args tuple_cons.glob_object 0 match_var match_bind
							position var_store expr_heap e_info cs
transfromPatternIntoBind mod_index def_level (AP_NewType cons_symbol type_index arg opt_var) src_expr position var_store expr_heap e_info cs
	# (src_expr, opt_var_bind, var_store, expr_heap) = bind_opt_var opt_var src_expr position var_store expr_heap
	# (binds, var_store, expr_heap, e_info, cs)
		= transfromPatternIntoBind mod_index def_level arg (MatchExpr {cons_symbol & glob_object.ds_arity = -2} src_expr) position var_store expr_heap e_info cs
	= (opt_var_bind ++ binds, var_store, expr_heap, e_info, cs)
transfromPatternIntoBind mod_index def_level (AP_WildCard _) src_expr _ var_store expr_heap e_info cs
	= ([], var_store, expr_heap, e_info, cs)
transfromPatternIntoBind _ _ pattern src_expr _ var_store expr_heap e_info cs
	= ([], var_store, expr_heap, e_info, { cs & cs_error = checkError "<pattern>" "illegal node pattern" cs.cs_error})

transfromPatternIntoStrictBind :: !Index !Level !AuxiliaryPattern !Expression !Position !*VarHeap !*ExpressionHeap !*ExpressionInfo !*CheckState
	-> *(![LetBind],![LetBind],!*VarHeap, !*ExpressionHeap,  !*ExpressionInfo, !*CheckState)
transfromPatternIntoStrictBind mod_index def_level (AP_Variable name var_info _) src_expr position var_store expr_heap e_info cs
	# bind = {lb_src = src_expr, lb_dst = { fv_ident = name, fv_info_ptr = var_info, fv_def_level = def_level, fv_count = 0 }, lb_position = position }
	= ([],[bind], var_store, expr_heap, e_info, cs)
transfromPatternIntoStrictBind mod_index def_level (AP_Algebraic cons_symbol=:{glob_module,glob_object=ds_cons=:{ds_arity, ds_index, ds_ident}} global_type_index args opt_var)
		src_expr position var_store expr_heap e_info=:{ef_type_defs,ef_modules} cs
	# (src_expr, src_bind, var_store, expr_heap) = bind_opt_var_or_create_new_var opt_var src_expr position def_level var_store expr_heap
	| ds_arity == 0
		= ([],[],var_store, expr_heap, e_info, { cs & cs_error = checkError ds_ident "constant not allowed in a node pattern" cs.cs_error})
	# (is_tuple, cs) = is_tuple_symbol glob_module ds_index cs
	| is_tuple
		# (lazy_binds,var_store,expr_heap,e_info,cs) = transform_sub_patterns mod_index def_level args ds_cons 0 src_expr [] position var_store expr_heap e_info cs
		= (lazy_binds,src_bind,var_store,expr_heap,e_info,cs)
		# ({td_rhs}, ef_type_defs, ef_modules) = get_type_def mod_index global_type_index ef_type_defs ef_modules
		  e_info = { e_info & ef_type_defs = ef_type_defs, ef_modules = ef_modules }
		= case td_rhs of
			RecordType {rt_fields}
				# (lazy_binds,var_store,expr_heap,e_info,cs) = transform_sub_patterns_of_record mod_index def_level args rt_fields glob_module 0
																								src_expr [] position var_store expr_heap e_info cs
				-> (lazy_binds,src_bind,var_store,expr_heap,e_info,cs)
			_
				| ds_arity == 1
		  			# (binds, var_store, expr_heap, e_info, cs)
						= transfromPatternIntoBind mod_index def_level (hd args) (MatchExpr cons_symbol src_expr)
								position var_store expr_heap e_info cs
					-> (binds,src_bind, var_store, expr_heap, e_info, cs)
					# (tuple_cons, cs) = getPredefinedGlobalSymbol (GetTupleConsIndex ds_arity) PD_PredefinedModule STE_Constructor ds_arity cs
					# (src_expr,expr_heap,cs) = add_decons_call_for_overloaded_lists glob_module ds_index src_expr expr_heap cs
					# (match_var, match_bind, var_store, expr_heap)
						=  bind_match_expr (MatchExpr cons_symbol src_expr) [] position def_level var_store expr_heap
					# (lazy_binds,var_store,expr_heap,e_info,cs) = transform_sub_patterns mod_index def_level args tuple_cons.glob_object 0 match_var match_bind
																		position var_store expr_heap e_info cs
					-> (lazy_binds,src_bind,var_store,expr_heap,e_info,cs)
transfromPatternIntoStrictBind mod_index def_level (AP_NewType cons_symbol type_index arg opt_var) src_expr position var_store expr_heap e_info cs
	# (src_expr, src_bind, var_store, expr_heap) = bind_opt_var_or_create_new_var opt_var src_expr position def_level var_store expr_heap
	# (binds, var_store, expr_heap, e_info, cs)
		= transfromPatternIntoBind mod_index def_level arg (MatchExpr {cons_symbol & glob_object.ds_arity = -2} src_expr) position var_store expr_heap e_info cs
	= (binds,src_bind, var_store, expr_heap, e_info, cs)
transfromPatternIntoStrictBind mod_index def_level (AP_WildCard _) src_expr _ var_store expr_heap e_info cs
	= ([],[],var_store, expr_heap, e_info, cs)
transfromPatternIntoStrictBind _ _ pattern src_expr _ var_store expr_heap e_info cs
	= ([],[],var_store, expr_heap, e_info, { cs & cs_error = checkError "<pattern>" "illegal node pattern" cs.cs_error})

get_type_def mod_index global_type_index=:{gi_module,gi_index} ef_type_defs ef_modules
	| mod_index == gi_module
		# (type_def, ef_type_defs) = ef_type_defs![gi_index]
		= (type_def, ef_type_defs, ef_modules)
		# ({dcl_common},  ef_modules) = ef_modules![gi_module]
		= (dcl_common.com_type_defs.[gi_index], ef_type_defs, ef_modules)
	
is_tuple_symbol cons_module cons_index cs
	# (tuple_2_symbol, cs) = getPredefinedGlobalSymbol (GetTupleConsIndex 2) PD_PredefinedModule STE_Constructor 2 cs
	= (tuple_2_symbol.glob_module == cons_module &&
	   tuple_2_symbol.glob_object.ds_index <= cons_index && cons_index <= tuple_2_symbol.glob_object.ds_index + 30, cs)

transform_sub_patterns mod_index def_level [pattern : patterns] tup_id tup_index arg_var all_binds position var_store expr_heap e_info cs
	# (this_arg_var, expr_heap)
			= adjust_match_expression arg_var expr_heap
	  match_expr
	  		= TupleSelect tup_id tup_index this_arg_var
	  (binds, var_store, expr_heap, e_info, cs)
	  		= transfromPatternIntoBind mod_index def_level pattern match_expr position var_store expr_heap e_info cs
	= transform_sub_patterns mod_index def_level patterns tup_id (inc tup_index) arg_var (binds ++ all_binds)
			position var_store expr_heap e_info cs
transform_sub_patterns mod_index _ [] _ _ _ binds _ var_store expr_heap e_info cs
	= (binds, var_store, expr_heap, e_info, cs)

transform_sub_patterns_of_record mod_index def_level [pattern : patterns] fields field_module field_index record_expr
		all_binds position var_store expr_heap e_info cs
	# {fs_ident, fs_index} = fields.[field_index]
	  selector = { glob_module = field_module, glob_object = MakeDefinedSymbol fs_ident fs_index 1}
	  (this_record_expr, expr_heap) = adjust_match_expression record_expr expr_heap
	  (binds, var_store, expr_heap, e_info, cs)
			= transfromPatternIntoBind mod_index def_level pattern (Selection NormalSelector this_record_expr [ RecordSelection selector field_index ])
					position var_store expr_heap e_info cs
	= transform_sub_patterns_of_record mod_index def_level patterns fields field_module (inc field_index) record_expr
			(binds ++ all_binds) position var_store expr_heap e_info cs
transform_sub_patterns_of_record mod_index _ [] _ _ _ _ binds _ var_store expr_heap e_info cs
	= (binds, var_store, expr_heap, e_info, cs)

bind_opt_var (Yes {bind_src,bind_dst}) src_expr position var_heap expr_heap
	# free_var = { fv_ident = bind_src, fv_info_ptr = bind_dst, fv_def_level = NotALevel, fv_count = 0 }
	  (var_expr_ptr, expr_heap) = newPtr EI_Empty expr_heap
	  bound_var = { var_ident = bind_src, var_info_ptr = bind_dst, var_expr_ptr = var_expr_ptr }
	= (Var bound_var, [{lb_src = src_expr, lb_dst = free_var, lb_position = position}], var_heap <:= (bind_dst, VI_Empty), expr_heap)
bind_opt_var No src_expr _ var_heap expr_heap
	= (src_expr, [], var_heap, expr_heap)

bind_opt_var_or_create_new_var (Yes {bind_src,bind_dst}) src_expr position def_level var_heap expr_heap
	# free_var = { fv_ident = bind_src, fv_info_ptr = bind_dst, fv_def_level = NotALevel, fv_count = 0 }
	  (var_expr_ptr, expr_heap) = newPtr EI_Empty expr_heap
	  bound_var = { var_ident = bind_src, var_info_ptr = bind_dst, var_expr_ptr = var_expr_ptr }
	= (Var bound_var, [{lb_dst = free_var, lb_src = src_expr, lb_position = position}], var_heap <:= (bind_dst, VI_Empty), expr_heap)
bind_opt_var_or_create_new_var No src_expr position def_level var_heap expr_heap
	# new_name = newVarId "_x"
	  (var_info_ptr, var_heap) = newPtr VI_Empty var_heap
	  (var_expr_ptr, expr_heap) = newPtr EI_Empty expr_heap
	  bound_var = { var_ident = new_name, var_info_ptr = var_info_ptr, var_expr_ptr = var_expr_ptr }
	  free_var = { fv_ident = new_name, fv_info_ptr = var_info_ptr, fv_def_level = def_level, fv_count = 0 }
	= (Var bound_var, [{lb_dst = free_var, lb_src = src_expr, lb_position = position }], var_heap, expr_heap)

bind_match_expr var_expr=:(Var var) opt_var_bind _ def_level var_heap expr_heap
	= (var_expr, opt_var_bind, var_heap, expr_heap)
bind_match_expr match_expr opt_var_bind position def_level var_heap expr_heap
	# new_name = newVarId "_x"
	  (var_info_ptr, var_heap) = newPtr VI_Empty var_heap
//	  (var_expr_ptr, expr_heap) = newPtr EI_Empty expr_heap
	  bound_var = { var_ident = new_name, var_info_ptr = var_info_ptr, var_expr_ptr = nilPtr }
	  free_var = { fv_ident = new_name, fv_info_ptr = var_info_ptr, fv_def_level = def_level, fv_count = 0 }
	= (Var bound_var, [{lb_src = match_expr, lb_dst = free_var, lb_position = position } : opt_var_bind], var_heap, expr_heap)

adjust_match_expression (Var var) expr_heap
	# (var_expr_ptr, expr_heap) = newPtr EI_Empty expr_heap
	= (Var { var & var_expr_ptr = var_expr_ptr }, expr_heap)
adjust_match_expression match_expr expr_heap
	= (match_expr, expr_heap)

add_decons_call_for_overloaded_lists glob_module ds_index src_expr expr_heap cs
	| glob_module==cPredefinedModuleIndex
		# pd_cons_index=ds_index+FirstConstructorPredefinedSymbolIndex
		| pd_cons_index==PD_UnboxedConsSymbol			
			# (stdStrictLists_index,_,decons_u_index,_,decons_u_ident,cs) = get_unboxed_list_indices_and_decons_u_ident cs
			# (new_info_ptr,expr_heap) = newPtr EI_Empty expr_heap
			  app_symb = {symb_ident=decons_u_ident,symb_kind=SK_OverloadedFunction {glob_object=decons_u_index,glob_module=stdStrictLists_index}}
			# decons_u_expr = App {app_symb=app_symb,app_args=[src_expr],app_info_ptr=new_info_ptr}			
			= (decons_u_expr,expr_heap,cs)
		| pd_cons_index==PD_UnboxedTailStrictConsSymbol
			# (stdStrictLists_index,_,decons_uts_index,_,decons_uts_ident,cs) = get_unboxed_tail_strict_list_indices_and_decons_uts_ident cs
			# (new_info_ptr,expr_heap) = newPtr EI_Empty expr_heap
			  app_symb = {symb_ident=decons_uts_ident,symb_kind=SK_OverloadedFunction {glob_object=decons_uts_index,glob_module=stdStrictLists_index}}
			# decons_uts_expr = App {app_symb=app_symb,app_args=[src_expr],app_info_ptr=new_info_ptr}
			= (decons_uts_expr,expr_heap,cs)
		| pd_cons_index==PD_OverloadedConsSymbol
			# (stdStrictLists_index,_,decons_index,_,decons_ident,cs) = get_overloaded_list_indices_and_decons_ident cs
			# (new_info_ptr,expr_heap) = newPtr EI_Empty expr_heap
			  app_symb = {symb_ident=decons_ident,symb_kind=SK_OverloadedFunction {glob_object=decons_index,glob_module=stdStrictLists_index}}
			# decons_expr = App {app_symb=app_symb,app_args=[src_expr],app_info_ptr=new_info_ptr}
			= (decons_expr,expr_heap,cs)
			= (src_expr,expr_heap,cs)
		= (src_expr,expr_heap,cs)

unfoldPatternMacro macro=:{fun_body=TransformedBody {tb_args,tb_rhs}} mod_index all_macro_args opt_var ps=:{ps_var_heap} modules cons_defs error
	| no_sharing tb_args
		# length_macro_args = length tb_args
		  (macro_args, extra_args)
			= if (length all_macro_args==length_macro_args) 
					(all_macro_args, []) 
					(splitAt length_macro_args all_macro_args)
		  ums = { ums_var_heap = fold2St bind_var tb_args macro_args ps_var_heap, ums_modules = modules, ums_cons_defs = cons_defs, ums_error = error }
		  (pattern, {ums_var_heap,ums_modules,ums_cons_defs,ums_error}) = unfold_pattern_macro mod_index macro.fun_ident opt_var extra_args tb_rhs ums
		= (pattern, { ps & ps_var_heap = ums_var_heap}, ums_modules, ums_cons_defs, ums_error)
		= (AP_Empty, { ps & ps_var_heap = ps_var_heap}, modules, cons_defs, checkError macro.fun_ident "sharing not allowed" error)
where
	no_sharing [{fv_count} : args]
		= fv_count <= 1 && no_sharing args
	no_sharing []
		= True
	
	bind_var {fv_info_ptr} pattern ps_var_heap
		= ps_var_heap <:= (fv_info_ptr, VI_Pattern pattern)

	unfold_pattern_macro mod_index macro_ident _ extra_args (Var {var_ident,var_info_ptr}) ums=:{ums_var_heap, ums_error}
		| not (isEmpty extra_args)
			= (AP_Empty, { ums & ums_error = checkError macro_ident "too many arguments for pattern macro" ums_error })
		# (VI_Pattern pattern, ums_var_heap) = readPtr var_info_ptr ums_var_heap
		= (pattern, { ums & ums_var_heap = ums_var_heap})
	unfold_pattern_macro mod_index macro_ident opt_var extra_args (App {app_symb={symb_kind=SK_Constructor {glob_module,glob_object},symb_ident},app_args})
						ums=:{ums_cons_defs,ums_modules,ums_error}
		# (cons_def, cons_index, ums_cons_defs, ums_modules) = get_cons_def mod_index glob_module glob_object ums_cons_defs ums_modules
		| cons_def.cons_type.st_arity == length app_args+length extra_args
			# (patterns, ums) = mapSt (unfold_pattern_macro mod_index macro_ident No []) app_args { ums & ums_cons_defs = ums_cons_defs, ums_modules = ums_modules }
			  cons_symbol = { glob_object = MakeDefinedSymbol symb_ident cons_index cons_def.cons_type.st_arity, glob_module = glob_module }
			  global_type_index = {gi_module = glob_module, gi_index = cons_def.cons_type_index}
			= (AP_Algebraic cons_symbol global_type_index (patterns++extra_args) opt_var, ums)
			= (AP_Empty, { ums & ums_cons_defs = ums_cons_defs, ums_modules = ums_modules,
					ums_error = checkError cons_def.cons_ident "incorrect number of arguments" ums_error })
	where
		get_cons_def mod_index cons_mod cons_index cons_defs modules
			| mod_index == cons_mod
				# (cons_def, cons_defs) = cons_defs![cons_index]
				= (cons_def, cons_index, cons_defs, modules)
				# ({dcl_common}, modules) = modules![cons_mod]
				  cons_def = dcl_common.com_cons_defs.[cons_index]
				= (cons_def, cons_index, cons_defs, modules)
	unfold_pattern_macro mod_index macro_ident opt_var extra_args (BasicExpr bv) ums=:{ums_error}
		| not (isEmpty extra_args)
			= (AP_Empty, { ums & ums_error = checkError macro_ident "too many arguments for pattern macro" ums_error })
		= (AP_Basic bv opt_var, ums)
	unfold_pattern_macro mod_index macro_ident opt_var _ expr ums=:{ums_error}
		= (AP_Empty, { ums & ums_error = checkError macro_ident "illegal rhs for a pattern macro" ums_error })
unfoldPatternMacro macro mod_index all_macro_args opt_var ps=:{ps_var_heap} modules cons_defs error
	= (AP_Empty, { ps & ps_var_heap = ps_var_heap}, modules, cons_defs, checkError macro.fun_ident "illegal macro in pattern" error)

checkSelectors end_with_update free_vars [ selector : selectors ] e_input e_state e_info cs
	| isEmpty selectors
		# (selector, free_vars, e_state, e_info, cs) = check_selector end_with_update free_vars selector e_input e_state e_info cs
		= ([ selector ], free_vars, e_state, e_info, cs)
		# (selector, free_vars, e_state, e_info, cs) = check_selector cEndWithSelection free_vars selector e_input e_state e_info cs
		  (selectors, free_vars, e_state, e_info, cs) = checkSelectors end_with_update free_vars selectors e_input e_state e_info cs
		= ([ selector : selectors ], free_vars, e_state, e_info, cs)
where
	check_selector _ free_vars (PS_Record selector=:{id_info,id_name} opt_type) e_input=:{ei_mod_index} e_state
			e_info=:{ef_selector_defs, ef_modules} cs=:{cs_symbol_table}
		# (entry, cs_symbol_table) = readPtr id_info cs_symbol_table
		# selectors = retrieveSelectorIndexes ei_mod_index entry
		  (field_module, field_index, field_nr, ef_selector_defs, ef_modules, cs)
		  		= get_field_nr ei_mod_index opt_type selectors id_name ef_selector_defs ef_modules { cs & cs_symbol_table = cs_symbol_table }
		= (RecordSelection { glob_object = MakeDefinedSymbol selector field_index 1, glob_module = field_module } field_nr, free_vars, e_state,
								{e_info & ef_selector_defs = ef_selector_defs, ef_modules = ef_modules }, cs)

	check_selector _ free_vars (PS_QualifiedRecord module_id field_name opt_type) e_input=:{ei_mod_index} e_state
			e_info cs=:{cs_symbol_table}
		# (entry, symbol_table) = readPtr module_id.id_info cs_symbol_table
		# cs = {cs & cs_symbol_table=symbol_table}
		= case entry.ste_kind of
			STE_ModuleQualifiedImports sorted_qualified_imports
				# selectors = retrieve_qualified_selector_indices field_name sorted_qualified_imports
				# {ef_selector_defs, ef_modules}=e_info
				  (field_module, field_index, field_nr, ef_selector_defs, ef_modules, cs)
			  		= get_field_nr ei_mod_index opt_type selectors field_name ef_selector_defs ef_modules cs
				  selector = {id_name=field_name,id_info=nilPtr}
				-> (RecordSelection { glob_object = MakeDefinedSymbol selector field_index 1, glob_module = field_module } field_nr, free_vars, e_state,
													{e_info & ef_selector_defs = ef_selector_defs, ef_modules = ef_modules }, cs)
			STE_ClosedModule
				-> not_imported_error cs
			STE_Module _
				-> not_imported_error cs
			_
				# selector = {id_name=field_name,id_info=nilPtr}
				-> (RecordSelection {glob_object = MakeDefinedSymbol selector NoIndex 1,glob_module = NoIndex}
									NoIndex, free_vars, e_state, e_info,
					{cs & cs_error = checkError module_id "not defined" cs.cs_error })
	where
		not_imported_error cs
			# selector = {id_name=field_name,id_info=nilPtr}
			= (RecordSelection {glob_object = MakeDefinedSymbol selector NoIndex 1,glob_module = NoIndex} NoIndex,
				free_vars, e_state, e_info, {cs & cs_error = checkError ("'"+++module_id.id_name+++"'."+++field_name) "not imported" cs.cs_error })

	check_selector end_with_update free_vars (PS_Array index_expr) e_input e_state e_info cs
		| end_with_update
			# (glob_select_symb, cs) = getPredefinedGlobalSymbol PD_ArrayUpdateFun PD_StdArray STE_Member 3 cs
			= checkArraySelection glob_select_symb free_vars index_expr e_input e_state e_info cs
			# (glob_select_symb, cs) = getPredefinedGlobalSymbol PD_ArraySelectFun PD_StdArray STE_Member 2 cs
			= checkArraySelection glob_select_symb free_vars index_expr e_input e_state e_info cs

get_field_nr :: !Index !OptionalRecordName ![Global Index] !{#Char} !u:{#SelectorDef} !v:{# DclModule} !*CheckState
		-> (!Index, !Index, !Index, u:{#SelectorDef}, v:{#DclModule}, !*CheckState)
get_field_nr mod_index _ [] id_name selector_defs modules cs=:{cs_error}
	= (NoIndex, NoIndex, NoIndex, selector_defs, modules, { cs & cs_error = checkError id_name "selector not defined" cs_error })
get_field_nr mod_index (RecordNameIdent type_id=:{id_info}) selectors id_name selector_defs modules cs=:{cs_symbol_table,cs_error}
	# (entry, cs_symbol_table) = readPtr id_info cs_symbol_table
	# (type_index, type_module) = retrieveGlobalDefinition entry STE_Type mod_index
	| type_index <> NotFound
		# (selector_index, selector_offset, selector_defs, modules)
				= determine_selector mod_index type_module type_index selectors selector_defs modules
		| selector_offset <> NoIndex
			= (type_module, selector_index, selector_offset, selector_defs, modules, { cs & cs_symbol_table = cs_symbol_table })
			= (NoIndex, NoIndex, NoIndex, selector_defs, modules, { cs & cs_symbol_table = cs_symbol_table,
						cs_error = checkError id_name "selector not defined" cs_error })
		= (NoIndex, NoIndex, NoIndex, selector_defs, modules, { cs & cs_symbol_table = cs_symbol_table,
				cs_error = checkError type_id "type not defined" cs_error })
get_field_nr mod_index (RecordNameQualifiedIdent module_id record_name) selectors id_name selector_defs modules cs
	# (found,{decl_kind,decl_ident,decl_index},cs) = search_qualified_ident module_id record_name TypeNameSpaceN cs
	| not found
		= (NoIndex, NoIndex, NoIndex, selector_defs, modules, cs)
		= case decl_kind of
			STE_Imported STE_Type type_mod_index
				# (selector_index, selector_offset, selector_defs, modules)
					= determine_selector mod_index type_mod_index decl_index selectors selector_defs modules
				| selector_offset <> NoIndex
					-> (type_mod_index, selector_index, selector_offset, selector_defs, modules, cs)
					-> (NoIndex, NoIndex, NoIndex, selector_defs, modules,
						{cs & cs_error = checkError id_name "selector not defined" cs.cs_error })
			_
				-> (NoIndex, NoIndex, NoIndex, selector_defs, modules,
					{cs & cs_error = checkError ("'"+++module_id.id_name+++"'."+++record_name) "type not defined" cs.cs_error} )
get_field_nr mod_index NoRecordName [{glob_object,glob_module}] id_name selector_defs modules cs
	| mod_index == glob_module
		# (selector_offset,selector_defs) = selector_defs![glob_object].sd_field_nr
		= (glob_module, glob_object, selector_offset, selector_defs, modules, cs)
		# (selector_offset,modules) = modules![glob_module].dcl_common.com_selector_defs.[glob_object].sd_field_nr
		= (glob_module, glob_object, selector_offset, selector_defs, modules, cs)
get_field_nr mod_index NoRecordName _  id_name selector_defs modules cs=:{cs_error}
	= (NoIndex, NoIndex, NoIndex, selector_defs, modules, { cs & cs_error = checkError id_name "ambiguous selector specified" cs_error })

determine_selector :: !Index !Index !Index ![Global Index] !u:{# SelectorDef} !v:{# DclModule} -> (!Int, !Int, !u:{# SelectorDef}, !v:{# DclModule})
determine_selector mod_index type_mod_index type_index [] selector_defs modules
	= (NoIndex, NoIndex, selector_defs, modules)
determine_selector mod_index type_mod_index type_index [{glob_module, glob_object} : selectors] selector_defs modules
	| type_mod_index == glob_module
		| type_mod_index == mod_index
			# (selector_def,selector_defs) = selector_defs![glob_object]
			| selector_def.sd_type_index == type_index
				= (glob_object, selector_def.sd_field_nr, selector_defs, modules)
				= determine_selector mod_index type_mod_index type_index selectors selector_defs modules
			# (selector_def, modules) = modules![glob_module].dcl_common.com_selector_defs.[glob_object]
			| selector_def.sd_type_index == type_index
				= (glob_object, selector_def.sd_field_nr, selector_defs, modules)
				= determine_selector mod_index type_mod_index type_index selectors selector_defs modules
		= determine_selector mod_index type_mod_index type_index selectors selector_defs modules

checkArraySelection glob_select_symb free_vars index_expr e_input e_state e_info cs
	# (index_expr, free_vars, e_state, e_info, cs) = checkExpression free_vars index_expr e_input e_state e_info cs
	  (new_info_ptr, es_expr_heap) = newPtr EI_Empty e_state.es_expr_heap
	= (ArraySelection glob_select_symb new_info_ptr index_expr, free_vars, { e_state & es_expr_heap = es_expr_heap }, e_info, cs)

checkFields :: !Index ![FieldAssignment] !OptionalRecordName !u:ExpressionInfo !*CheckState
	-> (!Optional ((Global DefinedSymbol), Index, [Bind ParsedExpr (Global FieldSymbol)]), !u:ExpressionInfo, !*CheckState)
checkFields mod_index field_ass opt_type e_info=:{ef_selector_defs,ef_type_defs,ef_modules} cs
	# (ok, field_ass, cs) = check_fields field_ass cs
	| ok
		# (opt_type_def, ef_selector_defs, ef_type_defs, ef_modules, cs)
				= determine_record_type mod_index opt_type field_ass ef_selector_defs ef_type_defs ef_modules cs
		  e_info = { e_info & ef_selector_defs = ef_selector_defs, ef_type_defs = ef_type_defs, ef_modules = ef_modules}
		= case opt_type_def of
			Yes ({td_index,td_rhs = RecordType {rt_constructor,rt_fields}}, type_mod_index)
				# (field_exprs, cs_error) = check_and_rearrange_fields type_mod_index 0 rt_fields field_ass cs.cs_error
				#! cons_symbol = {glob_object = rt_constructor, glob_module = type_mod_index}
				-> (Yes (cons_symbol, td_index, field_exprs), e_info, {cs & cs_error = cs_error})
			Yes _
				# (RecordNameIdent type_ident) = opt_type
				-> (No, e_info, { cs & cs_error = checkError type_ident "not a record constructor" cs.cs_error })
			No
				-> (No, e_info, cs)
		= (No, e_info, cs)
where
	check_fields [ bind=:{bind_dst=bind_dst=:FieldName field_ident} : field_ass ] cs=:{cs_symbol_table,cs_error}
		# (entry, cs_symbol_table) = readPtr field_ident.id_info cs_symbol_table
		# fields = retrieveSelectorIndexes mod_index entry 
		| isEmpty fields
			= (False, [], { cs & cs_symbol_table = cs_symbol_table, cs_error = checkError field_ident "not defined as a record field" cs_error })
			# (ok, field_ass, cs) = check_fields field_ass { cs & cs_symbol_table = cs_symbol_table }
			= (ok, [{bind & bind_dst = (bind_dst, fields)} : field_ass], cs)
	check_fields [ bind=:{bind_dst=bind_dst=:QualifiedFieldName module_id field_name} : field_ass ] cs=:{cs_symbol_table}
		# (entry, symbol_table) = readPtr module_id.id_info cs_symbol_table
		# cs = {cs & cs_symbol_table=symbol_table}
		= case entry.ste_kind of
			STE_ModuleQualifiedImports sorted_qualified_imports
				# fields = retrieve_qualified_selector_indices field_name sorted_qualified_imports
				| isEmpty fields
					-> not_imported_error cs
					# (ok, field_ass, cs) = check_fields field_ass cs
					-> (ok, [{bind & bind_dst = (bind_dst, fields)} : field_ass], cs)
			STE_ClosedModule
				-> not_imported_error cs
			STE_Module _
				-> not_imported_error cs
			_
				-> (False, [], { cs & cs_error = checkError module_id "not defined" cs.cs_error })
		where
			not_imported_error cs
				= (False, [], { cs & cs_error = checkError ("'"+++module_id.id_name+++"'."+++field_name) "not defined as a record field" cs.cs_error })
	check_fields [] cs
		= (True, [], cs)

	try_to_get_unique_field []
		= No
	try_to_get_unique_field [ {bind_dst = (field_id, [field])} : fields ]
		= Yes field
	try_to_get_unique_field [ _ : fields ]
		= try_to_get_unique_field fields
	
	determine_record_type mod_index (RecordNameIdent type_id=:{id_info}) _ selector_defs type_defs modules cs=:{cs_symbol_table, cs_error}
		# (entry, cs_symbol_table) = readPtr id_info cs_symbol_table
		# (type_index, type_mod_index) = retrieveGlobalDefinition entry STE_Type mod_index
		| type_index <> NotFound
			| mod_index == type_mod_index
			 	# (type_def, type_defs) = type_defs![type_index]
			 	= (Yes (type_def, type_mod_index), selector_defs, type_defs, modules, { cs & cs_symbol_table = cs_symbol_table })
				# (type_def, modules) = modules![type_mod_index].dcl_common.com_type_defs.[type_index]
				= (Yes (type_def, type_mod_index), selector_defs, type_defs, modules, { cs & cs_symbol_table = cs_symbol_table })
			= (No, selector_defs, type_defs, modules, { cs & cs_error = checkError type_id "not defined" cs_error, cs_symbol_table = cs_symbol_table})

	determine_record_type mod_index (RecordNameQualifiedIdent module_id record_name) _ selector_defs type_defs modules cs
		# (found,{decl_kind,decl_ident,decl_index},cs) = search_qualified_ident module_id record_name TypeNameSpaceN cs
		| not found
			= (No, selector_defs, type_defs, modules, cs)
			= case decl_kind of
				STE_Imported STE_Type type_mod_index
					| type_mod_index==mod_index
					 	# (type_def, type_defs) = type_defs![decl_index]
					 	-> (Yes (type_def, type_mod_index), selector_defs, type_defs, modules, cs)
						# (type_def, modules) = modules![type_mod_index].dcl_common.com_type_defs.[decl_index]
						-> (Yes (type_def, type_mod_index), selector_defs, type_defs, modules, cs)
				_
					-> (No, selector_defs, type_defs, modules, { cs & cs_error = checkError ("'"+++module_id.id_name+++"'."+++record_name) "not imported" cs.cs_error })

	determine_record_type mod_index NoRecordName fields selector_defs type_defs modules cs=:{cs_error}
		# succ = try_to_get_unique_field fields
		= case succ of
			Yes {glob_module, glob_object}
				| glob_module == mod_index
					# (selector_def, selector_defs) = selector_defs![glob_object]
					  (type_def, type_defs) = type_defs![selector_def.sd_type_index]
					-> (Yes (type_def, glob_module), selector_defs, type_defs, modules, cs)
					# ({dcl_common={com_selector_defs,com_type_defs}}, modules) = modules![glob_module]
					  {sd_type_index} = com_selector_defs.[glob_object]
					  type_def = com_type_defs.[sd_type_index]
					-> (Yes (type_def,glob_module), selector_defs, type_defs, modules, cs)
			No
				-> (No, selector_defs, type_defs, modules, { cs & cs_error = checkError "could not determine the type of this record" "" cs.cs_error })

	check_and_rearrange_fields :: !Int !Int !{#FieldSymbol} ![Bind ParsedExpr (FieldNameOrQualifiedFieldName,[Global .Int])] !*ErrorAdmin -> (![Bind ParsedExpr .(Global FieldSymbol)],!.ErrorAdmin);
	check_and_rearrange_fields mod_index field_index fields field_ass cs_error
		| field_index < size fields
			# (field_expr, field_ass) = look_up_field mod_index fields.[field_index] field_ass
		 	  (field_exprs, cs_error) = check_and_rearrange_fields mod_index (inc field_index) fields field_ass cs_error
			= ([field_expr : field_exprs], cs_error)
		| isEmpty field_ass
			= ([], cs_error)
			= ([], foldSt field_error field_ass cs_error)

	where			
		look_up_field mod_index field []
			= ({bind_src = PE_WildCard,  bind_dst = { glob_object = field, glob_module = mod_index }}, [])
		look_up_field mod_index field=:{fs_index} [ass=:{bind_src, bind_dst = (_, fields)} : field_ass]
			| field_list_contains_field mod_index fs_index fields
				= ({bind_src = bind_src, bind_dst = { glob_module = mod_index, glob_object = field}}, field_ass)
				# (field_expr, field_ass) = look_up_field mod_index field field_ass
				= (field_expr, [ass : field_ass])

		field_list_contains_field mod_index fs_index []
			= False
		field_list_contains_field mod_index fs_index [{glob_object,glob_module} : fields]
			= mod_index == glob_module && fs_index == glob_object || field_list_contains_field mod_index fs_index fields

		field_error {bind_dst=(field_id,_)} error
			= checkError field_id "field is either multiply used or not a part of this record" error

checkRhssAndTransformLocalDefs free_vars [] rhs_expr e_input e_state e_info cs
	= (rhs_expr, free_vars, e_state, e_info, cs)
checkRhssAndTransformLocalDefs free_vars loc_defs rhs_expr e_input e_state e_info cs
	# (binds, free_vars, e_state, e_info, cs) = checkAndTransformPatternIntoBind free_vars loc_defs e_input e_state e_info cs
	  (rhs_expr, es_expr_heap) = buildLetExpression [] binds rhs_expr NoPos e_state.es_expr_heap
	= (rhs_expr, free_vars, { e_state & es_expr_heap = es_expr_heap }, e_info, cs)

checkLhssOfLocalDefs :: .Int .Int LocalDefs Int *ExpressionState *ExpressionInfo *CheckState -> (!.[NodeDef AuxiliaryPattern],!(![Ident],![ArrayPattern]),!.ExpressionState,!.ExpressionInfo,!.CheckState);
checkLhssOfLocalDefs def_level mod_index (CollectedLocalDefs {loc_functions={ir_from,ir_to},loc_nodes,loc_in_icl_module}) local_functions_index_offset e_state=:{es_var_heap,es_fun_defs} e_info=:{ef_is_macro_fun} cs
	# ir_from=ir_from+local_functions_index_offset
	# ir_to=ir_to+local_functions_index_offset
	# (loc_defs, accus, {ps_fun_defs,ps_var_heap}, e_info, cs)
			= check_patterns loc_nodes {pi_def_level = def_level, pi_mod_index = mod_index, pi_is_node_pattern = True } ([], [])
					{ps_fun_defs = es_fun_defs, ps_var_heap = es_var_heap} e_info cs
	| loc_in_icl_module
		# (fun_defs, cs_symbol_table, cs_error) = addLocalFunctionDefsToSymbolTable def_level ir_from ir_to ef_is_macro_fun ps_fun_defs cs.cs_symbol_table cs.cs_error
		= (loc_defs, accus, { e_state & es_fun_defs = fun_defs, es_var_heap = ps_var_heap }, e_info, { cs & cs_symbol_table = cs_symbol_table, cs_error = cs_error })
		# (macro_defs, cs_symbol_table, cs_error) = addLocalDclMacroDefsToSymbolTable def_level mod_index ir_from ir_to e_info.ef_macro_defs cs.cs_symbol_table cs.cs_error
		= (loc_defs, accus, { e_state & es_fun_defs = ps_fun_defs, es_var_heap = ps_var_heap }, {e_info & ef_macro_defs=macro_defs}, { cs & cs_symbol_table = cs_symbol_table, cs_error = cs_error })
where
	check_patterns [ node_def : node_defs ] p_input accus var_store e_info cs
		# (pattern, accus, var_store, e_info, cs) = check_local_lhs_pattern node_def.nd_dst No p_input accus var_store e_info cs
		  (patterns, accus, var_store, e_info, cs) = check_patterns node_defs p_input accus var_store e_info cs
		= ([{ node_def & nd_dst = pattern } : patterns], accus, var_store, e_info, cs)
	check_patterns [] p_input accus var_store e_info cs
		= ([], accus, var_store, e_info, cs)

	/* RWS: FIXME
		This is a patch for the case
			...
			where
				X = 10
		in which X should be a node-id (a.k.a. AP_Variable) and not a pattern.
		I think the distinction between node-ids and constructors should be done
		in an earlier phase, but this will need a larger rewrite.
	*/
	check_local_lhs_pattern (PE_Ident id=:{id_name, id_info}) opt_var {pi_def_level, pi_mod_index} accus=:(var_env, array_patterns)
				 ps e_info cs=:{cs_symbol_table}
		# (entry, cs_symbol_table) = readPtr id_info cs_symbol_table
		# (new_info_ptr, ps_var_heap) = newPtr VI_Empty ps.ps_var_heap
		  cs = checkPatternVariable pi_def_level entry id new_info_ptr { cs & cs_symbol_table = cs_symbol_table }
		= (AP_Variable id new_info_ptr opt_var, ([ id : var_env ], array_patterns), { ps & ps_var_heap = ps_var_heap}, e_info, cs)
	check_local_lhs_pattern pattern opt_var p_input accus var_store e_info cs
		= checkPattern pattern opt_var p_input accus var_store e_info cs

addArraySelections [] rhs_expr free_vars e_input e_state e_info cs
	= (rhs_expr, free_vars, e_state, e_info, cs)
addArraySelections array_patterns rhs_expr free_vars e_input e_state e_info cs
	# (let_strict_binds, let_lazy_binds, free_vars, e_state, e_info, cs)
			= buildArraySelections e_input array_patterns free_vars e_state e_info cs
	  (let_expr_ptr, es_expr_heap) = newPtr EI_Empty e_state.es_expr_heap
	= ( Let {let_strict_binds = let_strict_binds, let_lazy_binds = let_lazy_binds,
				let_expr = rhs_expr, let_info_ptr = let_expr_ptr, let_expr_position = NoPos }
	  , free_vars , { e_state & es_expr_heap = es_expr_heap} , e_info, cs )

buildArraySelections e_input array_patterns free_vars e_state e_info cs
	= foldSt (buildSelections e_input) array_patterns ([], [], free_vars, e_state, e_info, cs)

buildSelections e_input {ap_selections=[]}
	  (strict_binds, lazy_binds, free_vars, e_state, e_info, cs)
	= (strict_binds, lazy_binds, free_vars, e_state, e_info, cs) // if an error occurs in checkPattern
buildSelections e_input {ap_opt_var, ap_array_var, ap_selections}
					(strict_binds, lazy_binds, free_vars, e_state, e_info, cs)
	# (ap_array_var, [last_array_selection:lazy_binds], free_vars, e_state, e_info, cs)
			= foldSt (build_sc e_input) (reverse ap_selections) // reverse to make cycle-in-spine behaviour compatible to Clean 1.3
						(ap_array_var, lazy_binds, free_vars, e_state, e_info, cs)
	  (lazy_binds, e_state)
	  		= case ap_opt_var of
	  			Yes { bind_src = opt_var_ident, bind_dst = opt_var_var_info_ptr }
					# (bound_array_var, es_expr_heap) = allocate_bound_var ap_array_var e_state.es_expr_heap
					  free_var = { fv_ident = opt_var_ident, fv_info_ptr = opt_var_var_info_ptr, fv_def_level = NotALevel,
					  				fv_count = 0 }
	  				-> ([{ lb_dst = free_var, lb_src = Var bound_array_var, lb_position = NoPos }: lazy_binds],
	  					{ e_state & es_expr_heap = es_expr_heap })
	  			no	-> (lazy_binds, e_state)
	= ([last_array_selection:strict_binds], lazy_binds, free_vars, e_state, e_info, cs)
  where
	build_sc e_input {bind_dst=parsed_index_exprs, bind_src=array_element_var} (ap_array_var, binds, free_vars, e_state, e_info, cs)
		# (var_for_uselect_result, es_var_heap)
				= allocate_free_var { id_name = "_x", id_info = nilPtr } e_state.es_var_heap
		  (new_array_var, es_var_heap)
		  		= allocate_free_var ap_array_var.fv_ident es_var_heap
		  (bound_array_var, es_expr_heap)
		  		= allocate_bound_var ap_array_var e_state.es_expr_heap
		  (bound_var_for_uselect_result, es_expr_heap)
		  		= allocate_bound_var var_for_uselect_result es_expr_heap
		  dimension
		  		= length parsed_index_exprs
		  (new_expr_ptrs, es_expr_heap)
		  		= mapSt newPtr (repeatn dimension EI_Empty) es_expr_heap
		  (tuple_cons, cs)
		  		= getPredefinedGlobalSymbol (GetTupleConsIndex 2) PD_PredefinedModule STE_Constructor 2 cs
		  (glob_select_symb, selector_kind, cs)
		  		= case dimension of
		  			1	# (unq_select_symb, cs) = getPredefinedGlobalSymbol PD_UnqArraySelectFun PD_StdArray STE_Member 2 cs
		  				-> (unq_select_symb, UniqueSingleArraySelector, cs)
		  			_	# (select_symb, cs) = getPredefinedGlobalSymbol PD_ArraySelectFun PD_StdArray STE_Member 2 cs
		  				-> (select_symb, UniqueSelector, cs)
		  e_state = { e_state & es_var_heap = es_var_heap, es_expr_heap = es_expr_heap }
		  (index_exprs, (free_vars, e_state, e_info, cs))
		  		= mapSt (check_index_expr e_input) parsed_index_exprs (free_vars, e_state, e_info, cs)
		  selections = [ ArraySelection glob_select_symb new_expr_ptr index_expr \\ new_expr_ptr<-new_expr_ptrs & index_expr<-index_exprs ]
		= (	new_array_var
		  ,	[ {lb_dst = var_for_uselect_result, lb_src = Selection selector_kind (Var bound_array_var) selections, lb_position = NoPos }
		    , {lb_dst = new_array_var, lb_src = TupleSelect tuple_cons.glob_object 1 (Var bound_var_for_uselect_result), lb_position = NoPos }
		    , {lb_dst = array_element_var, lb_src = TupleSelect tuple_cons.glob_object 0 (Var bound_var_for_uselect_result), lb_position = NoPos }
		  	: binds
			]
		  , free_vars, e_state, e_info , cs)

	check_index_expr e_input parsed_index_expr (free_vars, e_state, e_info, cs)
		# (index_expr, free_vars, e_state, e_info, cs) = checkExpression free_vars parsed_index_expr e_input e_state e_info cs
		= (index_expr, (free_vars, e_state, e_info, cs))
		


buildLetExpression :: ![LetBind] ![LetBind] !Expression !Position !*ExpressionHeap  -> (!Expression, !*ExpressionHeap)
buildLetExpression [] [] expr _ expr_heap
	= (expr, expr_heap)
buildLetExpression let_strict_binds let_lazy_binds expr let_expr_position expr_heap
	# (let_expr_ptr, expr_heap) = newPtr EI_Empty expr_heap
	= (Let {let_strict_binds = let_strict_binds, let_lazy_binds = let_lazy_binds, let_expr = expr,
			let_info_ptr = let_expr_ptr, let_expr_position = let_expr_position }, expr_heap)

buildApplication :: !SymbIdent !Int !Int ![Expression] !*ExpressionState !*ErrorAdmin -> (!Expression,!*ExpressionState,!*ErrorAdmin)
buildApplication symbol=:{symb_kind=SK_Constructor _} form_arity act_arity args e_state error
	# app = App { app_symb = symbol , app_args = args, app_info_ptr = nilPtr }
	| act_arity > form_arity
		= (app, e_state, checkError symbol.symb_ident "used with too many arguments" error)
		= (app, e_state, error)
buildApplication symbol=:{symb_kind=SK_OverloadedConstructor cons_index} form_arity act_arity args e_state error
	# (new_info_ptr, es_expr_heap) = newPtr EI_Empty e_state.es_expr_heap
	  e_state = {e_state & es_expr_heap=es_expr_heap}
	  app = App {app_symb = {symbol & symb_kind=SK_Constructor cons_index}, app_args = args, app_info_ptr = new_info_ptr}
	| act_arity > form_arity
		= (app, e_state, checkError symbol.symb_ident "used with too many arguments" error)
		= (app, e_state, error)
buildApplication symbol=:{symb_kind=SK_NewTypeConstructor _} form_arity act_arity args e_state error
	# app = App { app_symb = symbol , app_args = args, app_info_ptr = nilPtr }
	| act_arity == form_arity
		= (app, e_state, error)
	| act_arity > form_arity
		= (app, e_state, checkError symbol.symb_ident "used with too many arguments" error)
		= (app, e_state, checkError symbol.symb_ident "argument missing (for newtype constructor)" error)
buildApplication symbol form_arity act_arity args e_state=:{es_expr_heap} error
	# (new_info_ptr, es_expr_heap) = newPtr EI_Empty es_expr_heap
	| form_arity < act_arity
		# app = { app_symb = symbol , app_args = take form_arity args, app_info_ptr = new_info_ptr }
		= (App app @ drop form_arity args, { e_state & es_expr_heap = es_expr_heap }, error)
		# app = { app_symb = symbol , app_args = take form_arity args, app_info_ptr = new_info_ptr }
		= (App app, { e_state & es_expr_heap = es_expr_heap }, error)

buildApplicationWithoutArguments :: !SymbIdent !*ExpressionState !*ErrorAdmin -> (!Expression,!*ExpressionState,!*ErrorAdmin)
buildApplicationWithoutArguments symbol=:{symb_kind=SK_Constructor _} e_state error
	# app = App { app_symb = symbol , app_args = [], app_info_ptr = nilPtr }
	= (app, e_state, error)
buildApplicationWithoutArguments symbol=:{symb_kind=SK_OverloadedConstructor cons_index} e_state error
	# (new_info_ptr, es_expr_heap) = newPtr EI_Empty e_state.es_expr_heap
	  app = App {app_symb = {symbol & symb_kind=SK_Constructor cons_index}, app_args = [], app_info_ptr = new_info_ptr}
	= (app, {e_state & es_expr_heap = es_expr_heap}, error)
buildApplicationWithoutArguments symbol=:{symb_kind=SK_NewTypeConstructor _} e_state error
	# app = App { app_symb = symbol , app_args = [], app_info_ptr = nilPtr }
	= (app, e_state, checkError symbol.symb_ident "argument missing (for newtype constructor)" error)
buildApplicationWithoutArguments symbol e_state error
	# (new_info_ptr, es_expr_heap) = newPtr EI_Empty e_state.es_expr_heap
	# app = App { app_symb = symbol , app_args = [], app_info_ptr = new_info_ptr }
	= (app, { e_state & es_expr_heap = es_expr_heap }, error)

buildPattern mod_index (APK_Constructor type_index) cons_ident args opt_var ps e_info cs
	= (AP_Algebraic cons_ident type_index args opt_var, ps, e_info, cs)
buildPattern mod_index (APK_NewTypeConstructor type_index) cons_ident [arg] opt_var ps e_info cs
	= (AP_NewType cons_ident type_index arg opt_var, ps, e_info, cs)
buildPattern mod_index (APK_Macro is_dcl_macro) {glob_module,glob_object} args opt_var ps e_info=:{ef_modules,ef_macro_defs,ef_cons_defs} cs=:{cs_error}
	| is_dcl_macro
		# (macro,ef_macro_defs) = ef_macro_defs![glob_module,glob_object.ds_index]
		# (pattern, ps, ef_modules, ef_cons_defs, cs_error)
				= unfoldPatternMacro macro mod_index args opt_var ps ef_modules ef_cons_defs cs_error
		= (pattern, ps, { e_info & ef_modules = ef_modules, ef_macro_defs=ef_macro_defs, ef_cons_defs = ef_cons_defs }, { cs & cs_error = cs_error })
		# (macro,ps) = ps!ps_fun_defs.[glob_object.ds_index]
		# (pattern, ps, ef_modules, ef_cons_defs, cs_error)
				= unfoldPatternMacro macro mod_index args opt_var ps ef_modules ef_cons_defs cs_error
		= (pattern, ps, { e_info & ef_modules = ef_modules, ef_macro_defs=ef_macro_defs, ef_cons_defs = ef_cons_defs }, { cs & cs_error = cs_error })

getPredefinedGlobalSymbol :: !Index !Index !STE_Kind !Int !*CheckState -> (!Global DefinedSymbol, !*CheckState)
getPredefinedGlobalSymbol symb_index module_index req_ste_kind arity cs=:{cs_predef_symbols,cs_symbol_table}
	# mod_id = predefined_idents.[module_index]
	# (mod_entry, cs_symbol_table)		= readPtr mod_id.id_info cs_symbol_table
	| mod_entry.ste_kind == STE_ClosedModule
		# (glob_object, cs) = get_predefined_symbol symb_index req_ste_kind arity mod_entry.ste_index
										{ cs & cs_predef_symbols = cs_predef_symbols, cs_symbol_table = cs_symbol_table}
		= ({ glob_object = glob_object, glob_module = mod_entry.ste_index }, cs)
		= ({ glob_object = { ds_ident = { id_name = "** ERRONEOUS **", id_info = nilPtr }, ds_index = NoIndex, ds_arity = arity }, glob_module = NoIndex},
				  		{ cs & cs_error = checkError mod_id "not imported" cs.cs_error, cs_predef_symbols = cs_predef_symbols, cs_symbol_table = cs_symbol_table })
where
	get_predefined_symbol :: !Index !STE_Kind !Int !Index !*CheckState -> (!DefinedSymbol,!*CheckState)
	get_predefined_symbol symb_index req_ste_kind arity mod_index cs=:{cs_predef_symbols,cs_symbol_table,cs_error}
		# symb_id = predefined_idents.[symb_index]
		  (symb_entry, cs_symbol_table) 	= readPtr symb_id.id_info cs_symbol_table
		  cs = { cs & cs_predef_symbols = cs_predef_symbols, cs_symbol_table = cs_symbol_table }
		| symb_entry.ste_kind == req_ste_kind
			= ({ ds_ident = symb_id, ds_index = symb_entry.ste_index, ds_arity = arity }, cs)
			= case symb_entry.ste_kind of
				STE_Imported kind module_index
					| mod_index == module_index && kind == req_ste_kind
						-> ({ ds_ident = symb_id, ds_index = symb_entry.ste_index, ds_arity = arity }, cs)
				_
					-> ({ ds_ident = symb_id, ds_index = NoIndex, ds_arity = arity }, { cs & cs_error = checkError symb_id "undefined" cs.cs_error })

typeOfBasicValue :: !BasicValue !*CheckState -> (!BasicType, !*CheckState)
typeOfBasicValue (BVI _) cs = (BT_Int, cs)
typeOfBasicValue (BVInt _) cs = (BT_Int, cs)
typeOfBasicValue (BVC _) cs = (BT_Char, cs)
typeOfBasicValue (BVB _) cs = (BT_Bool, cs)
typeOfBasicValue (BVR _) cs = (BT_Real, cs)
typeOfBasicValue (BVS _) cs
	# ({glob_module,glob_object={ds_ident,ds_index,ds_arity}}, cs) = getPredefinedGlobalSymbol PD_StringType PD_PredefinedModule STE_Type 0 cs
	= (BT_String (TA (MakeTypeSymbIdent { glob_object = ds_index, glob_module = glob_module } ds_ident ds_arity) []), cs)

buildTypeCase type_case_dynamic type_case_patterns type_case_default type_case_info_ptr case_explicit :==
	Case {	case_expr = type_case_dynamic, case_guards = DynamicPatterns type_case_patterns, case_default = type_case_default, 
			case_info_ptr = type_case_info_ptr, case_ident = No, case_default_pos = NoPos,
			case_explicit	= case_explicit
 }

determinePatternVariable (Yes bind) var_heap
	= (bind, var_heap)
determinePatternVariable No var_heap
	# (new_info_ptr, var_heap) = newPtr VI_Empty var_heap
	= ({ bind_src = newVarId "_x", bind_dst = new_info_ptr }, var_heap)

pushErrorAdmin2 _ NoPos cs=:{cs_error={ea_loc=[top_of_stack:_]}}
	// there is no position info, push current position to balance pop calls
	= pushErrorAdmin top_of_stack cs
pushErrorAdmin2 string pos=:(LinePos _ _) cs
	= pushErrorAdmin (newPosition {id_name=string, id_info=nilPtr} pos) cs

allocate_bound_var :: !FreeVar !*ExpressionHeap -> (!BoundVar, !.ExpressionHeap)
allocate_bound_var {fv_ident, fv_info_ptr} expr_heap
	# (var_expr_ptr, expr_heap) = newPtr EI_Empty expr_heap
	= ({ var_ident = fv_ident, var_info_ptr = fv_info_ptr, var_expr_ptr = var_expr_ptr }, expr_heap)

allocate_free_var ident var_heap
	# (new_var_info_ptr, var_heap) = newPtr VI_Empty var_heap
	= ({ fv_def_level = NotALevel, fv_ident = ident, fv_info_ptr = new_var_info_ptr,	fv_count = 0 }, var_heap)

newVarId name = { id_name = name, id_info = nilPtr }

retrieveSelectorIndexes :: Int !SymbolTableEntry -> [(Global Int)]
retrieveSelectorIndexes mod_index {ste_kind = STE_Selector selector_list, ste_index, ste_previous }
	= map (adjust_mod_index mod_index) selector_list
where
	adjust_mod_index mod_index selector=:{glob_module}
		| glob_module == NoIndex
			= {selector & glob_module = mod_index}
			= selector
retrieveSelectorIndexes mod_index off_kind
	= []

retrieve_qualified_selector_indices field_name sorted_qualified_imports
	=  [{glob_module=type_mod_index,glob_object=decl_index} \\
		{decl_kind=STE_Imported (STE_Field selector) type_mod_index,decl_index}
		<- search_qualified_imports field_name sorted_qualified_imports FieldNameSpaceN]

instance <<< FieldSymbol
where
	(<<<) file { fs_var } = file <<< fs_var
