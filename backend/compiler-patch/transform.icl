implementation module transform

import syntax, utilities, mergecases

::	LiftState =
	{	ls_var_heap		:: !.VarHeap
	,	ls_x 			:: !.LiftStateX
	,	ls_expr_heap	:: !.ExpressionHeap
	}

::	LiftStateX = {
		x_fun_defs :: !.{#FunDef},
		x_macro_defs :: !.{#.{#FunDef}},
		x_main_dcl_module_n :: !Int
	}

class lift a :: !a !*LiftState -> (!a, !*LiftState)

instance lift [a] | lift a
where
	lift l ls = mapSt lift l ls

instance lift (a,b) | lift a & lift b
where
	lift t ls = app2St (lift,lift) t ls

instance lift (Optional a) | lift a
where
	lift (Yes x) ls
		# (x, ls) = lift x ls
		= (Yes x, ls)
	lift no ls
		= (no, ls)
	
instance lift CheckedAlternative
where
	lift ca=:{ca_rhs} ls
		# (ca_rhs, ls) = lift ca_rhs ls
		= ({ ca & ca_rhs = ca_rhs }, ls)
	
instance lift Expression
where
	lift (FreeVar {fv_ident,fv_info_ptr}) ls=:{ls_var_heap}
		# (var_info, ls_var_heap) = readPtr fv_info_ptr ls_var_heap
		  ls = { ls & ls_var_heap = ls_var_heap }
		= case var_info of
			 VI_LiftedVariable var_info_ptr
			 	# (var_expr_ptr, ls_expr_heap) = newPtr EI_Empty ls.ls_expr_heap
			 	-> (Var { var_ident = fv_ident, var_info_ptr = var_info_ptr, var_expr_ptr = var_expr_ptr }, { ls & ls_expr_heap = ls_expr_heap})
			 _
			 	# (var_expr_ptr, ls_expr_heap) = newPtr EI_Empty ls.ls_expr_heap
			 	-> (Var { var_ident = fv_ident, var_info_ptr = fv_info_ptr, var_expr_ptr = var_expr_ptr }, { ls & ls_expr_heap = ls_expr_heap})
	lift (App app) ls
		# (app, ls) = lift app ls
		= (App app, ls)
	lift (expr @ exprs) ls
		# ((expr,exprs), ls) = lift (expr,exprs) ls
		= (expr @ exprs, ls)
	lift (Let lad=:{let_strict_binds, let_lazy_binds, let_expr}) ls
		# (let_strict_binds, ls) = lift let_strict_binds ls
		  (let_lazy_binds, ls) = lift let_lazy_binds ls
		  (let_expr, ls) = lift let_expr ls
		= (Let {lad & let_strict_binds = let_strict_binds, let_lazy_binds = let_lazy_binds, let_expr = let_expr}, ls)
	lift (Case case_expr) ls
		# (case_expr, ls) = lift case_expr ls
		= (Case case_expr, ls)
	lift (Selection is_unique expr selectors) ls
		# (selectors, ls) = lift selectors ls
		  (expr, ls) = lift expr ls
		= (Selection is_unique expr selectors, ls)
	lift (Update expr1 selectors expr2) ls
		# (selectors, ls) = lift selectors ls
		  (expr1, ls) = lift expr1 ls
		  (expr2, ls) = lift expr2 ls
		= (Update expr1 selectors expr2, ls)
	lift (RecordUpdate cons_symbol expression expressions) ls
		# (expression, ls) = lift expression ls
		  (expressions, ls) = lift expressions ls
		= (RecordUpdate cons_symbol expression expressions, ls)
	lift (TupleSelect symbol argn_nr expr) ls
		# (expr, ls) = lift expr ls
		= (TupleSelect symbol argn_nr expr, ls)
	lift (MatchExpr cons_ident expr) ls
		# (expr, ls) = lift expr ls
		= (MatchExpr cons_ident expr, ls)
	lift (DynamicExpr expr) ls
		# (expr, ls) = lift expr ls
		= (DynamicExpr expr, ls)
	lift (IsConstructor expr cons_symbol cons_arity global_type_index case_ident position) ls
		# (expr, ls) = lift expr ls
		= (IsConstructor expr cons_symbol cons_arity global_type_index case_ident position, ls)
	lift (TypeSignature type_function expr) ls
		# (expr, ls) = lift expr ls
		= (TypeSignature type_function expr, ls)
	lift expr ls
		= (expr, ls)

instance lift Selection
where
	lift (ArraySelection array_select expr_ptr index_expr) ls
		# (index_expr, ls) = lift index_expr ls
		= (ArraySelection array_select expr_ptr index_expr, ls)
	lift record_selection ls
		= (record_selection, ls)

instance lift App
where
	lift app=:{app_symb = app_symbol=:{symb_kind = SK_Function {glob_object,glob_module}}, app_args} ls
		| glob_module == ls.ls_x.LiftStateX.x_main_dcl_module_n
			# (fun_def,ls) = ls!ls_x.x_fun_defs.[glob_object]
			= lift_function_app app fun_def.fun_info.fi_free_vars ls
			# (app_args, ls) = lift app_args ls
			= ({ app & app_args = app_args }, ls)
	lift app=:{app_symb = {symb_kind = SK_LocalMacroFunction glob_object},app_args} ls
		# (fun_def,ls) = ls!ls_x.x_fun_defs.[glob_object]
		= lift_function_app app fun_def.fun_info.fi_free_vars ls
	lift app=:{app_symb = {symb_kind = SK_LocalDclMacroFunction {glob_object,glob_module}}} ls
		# (fun_def,ls) = ls!ls_x.x_macro_defs.[glob_module,glob_object]
		= lift_function_app app fun_def.fun_info.fi_free_vars ls
	lift app=:{app_args} ls
		# (app_args, ls) = lift app_args ls
		= ({ app & app_args = app_args }, ls)

lift_function_app app=:{app_symb=app_symbol,app_args} [] ls
	# (app_args, ls) = lift app_args ls
	= ({ app & app_args = app_args }, ls)
lift_function_app app=:{app_args} fi_free_vars ls
	# (app_args, ls) = lift app_args ls
	# (app_args, ls_var_heap, ls_expr_heap) = add_free_variables_in_app fi_free_vars app_args ls.ls_var_heap ls.ls_expr_heap
	# app = { app & app_args = app_args }
	= (app,	{ ls & ls_var_heap = ls_var_heap, ls_expr_heap = ls_expr_heap })
where
	add_free_variables_in_app :: ![FreeVar] ![Expression] !*VarHeap !*ExpressionHeap -> (![Expression],!*VarHeap,!*ExpressionHeap)
	add_free_variables_in_app [] app_args var_heap expr_heap
		= (app_args, var_heap, expr_heap)
	add_free_variables_in_app [{fv_ident, fv_info_ptr} : free_vars] app_args var_heap expr_heap
		# (var_info,var_heap) = readPtr fv_info_ptr var_heap
		= case var_info of
			VI_LiftedVariable var_info_ptr
			 	# (var_expr_ptr, expr_heap) = newPtr EI_Empty expr_heap
				-> add_free_variables_in_app free_vars [Var { var_ident = fv_ident, var_info_ptr = var_info_ptr, var_expr_ptr = var_expr_ptr } : app_args]
						var_heap expr_heap
			_
			 	# (var_expr_ptr, expr_heap) = newPtr EI_Empty expr_heap
				-> add_free_variables_in_app free_vars [Var { var_ident = fv_ident, var_info_ptr = fv_info_ptr, var_expr_ptr = var_expr_ptr } : app_args]
						var_heap expr_heap

instance lift LetBind
where
	lift bind=:{lb_src} ls
		# (lb_src, ls) = lift lb_src ls
		= ({ bind & lb_src = lb_src }, ls)

instance lift (Bind a b) | lift a
where
	lift bind=:{bind_src} ls
		# (bind_src, ls) = lift bind_src ls
		= ({ bind & bind_src = bind_src }, ls)

instance lift Case
where
	lift kees=:{ case_expr,case_guards,case_default } ls
		# ((case_expr,(case_guards,case_default)), ls) = lift (case_expr,(case_guards,case_default)) ls
		= ({ kees & case_expr = case_expr,case_guards = case_guards, case_default = case_default }, ls)

instance lift CasePatterns
where
	lift (AlgebraicPatterns type patterns) ls
		# (patterns, ls) = lift patterns ls
		= (AlgebraicPatterns type patterns, ls)
	lift (BasicPatterns type patterns) ls
		# (patterns, ls) = lift patterns ls
		= (BasicPatterns type patterns, ls)
	lift (OverloadedListPatterns type decons_expr patterns) ls
		# (patterns, ls) = lift patterns ls
		# (decons_expr, ls) = lift decons_expr ls
		= (OverloadedListPatterns type decons_expr patterns, ls)
	lift (NewTypePatterns type patterns) ls
		# (patterns, ls) = lift patterns ls
		= (NewTypePatterns type patterns, ls)
	lift (DynamicPatterns patterns) ls
		# (patterns, ls) = lift patterns ls
		= (DynamicPatterns patterns, ls)

instance lift AlgebraicPattern
where
	lift pattern=:{ap_expr} ls
		# (ap_expr, ls) = lift ap_expr ls
		= ({ pattern & ap_expr = ap_expr }, ls)

instance lift BasicPattern
where
	lift pattern=:{bp_expr} ls
		# (bp_expr, ls) = lift bp_expr ls
		= ({ pattern & bp_expr = bp_expr }, ls)

instance lift DynamicPattern
where
	lift pattern=:{dp_rhs} ls
		# (dp_rhs, ls) = lift dp_rhs ls
		= ({ pattern & dp_rhs = dp_rhs }, ls)

instance lift DynamicExpr
where
	lift dyn=:{dyn_expr} ls
		# (dyn_expr, ls) = lift dyn_expr ls
		= ({ dyn & dyn_expr = dyn_expr}, ls)

liftFunctions :: [FunctionOrMacroIndex] Int Int *{#FunDef} *{#*{#FunDef}} *VarHeap *ExpressionHeap -> .LiftState;
liftFunctions group group_index main_dcl_module_n fun_defs macro_defs var_heap expr_heap
	# (contains_free_vars, lifted_function_called, fun_defs,macro_defs)
			= foldSt (add_free_vars_of_non_recursive_calls_to_function group_index) group (False, False, fun_defs,macro_defs)
	| contains_free_vars
		# (fun_defs,macro_defs) = iterateSt (add_free_vars_of_recursive_calls_to_functions group_index group) (fun_defs,macro_defs)
		= lift_functions group {ls_x={x_fun_defs=fun_defs,x_macro_defs=macro_defs,x_main_dcl_module_n=main_dcl_module_n},ls_var_heap=var_heap,ls_expr_heap=expr_heap}
	| lifted_function_called
		= lift_functions group {ls_x={x_fun_defs=fun_defs,x_macro_defs=macro_defs,x_main_dcl_module_n=main_dcl_module_n},ls_var_heap=var_heap,ls_expr_heap=expr_heap}
		= {ls_x={x_fun_defs=fun_defs,x_macro_defs=macro_defs,x_main_dcl_module_n=main_dcl_module_n},ls_var_heap=var_heap, ls_expr_heap=expr_heap}
where
	add_free_vars_of_non_recursive_calls_to_function group_index (FunctionOrIclMacroIndex fun) (contains_free_vars, lifted_function_called, fun_defs,macro_defs)
		# (fun_def=:{fun_info}, fun_defs) = fun_defs![fun]
		  { fi_free_vars,fi_def_level,fi_calls } = fun_info
		  (lifted_function_called, fi_free_vars, fun_defs,macro_defs)
				= add_free_vars_of_non_recursive_calls fi_def_level group_index fi_calls lifted_function_called fi_free_vars fun_defs macro_defs
		= (contains_free_vars || not (isEmpty fi_free_vars), lifted_function_called, 
			{ fun_defs & [fun] = { fun_def & fun_info = { fun_info & fi_free_vars = fi_free_vars }}},macro_defs)
	add_free_vars_of_non_recursive_calls_to_function group_index (DclMacroIndex macro_module_index macro_index) (contains_free_vars, lifted_function_called, fun_defs,macro_defs)
		# (fun_def=:{fun_info}, macro_defs) = macro_defs![macro_module_index,macro_index]
		  { fi_free_vars,fi_def_level,fi_calls } = fun_info
		  (lifted_function_called, fi_free_vars, fun_defs,macro_defs)
		  		= add_free_vars_of_non_recursive_calls fi_def_level group_index fi_calls lifted_function_called fi_free_vars fun_defs macro_defs
		= (contains_free_vars || not (isEmpty fi_free_vars), lifted_function_called, 
			fun_defs,{ macro_defs & [macro_module_index,macro_index] = { fun_def & fun_info = { fun_info & fi_free_vars = fi_free_vars }}})

	add_free_vars_of_non_recursive_calls fi_def_level group_index fi_calls lifted_function_called fi_free_vars fun_defs macro_defs
		= foldSt (add_free_vars_of_non_recursive_call fi_def_level group_index) fi_calls (lifted_function_called, fi_free_vars, fun_defs,macro_defs)
	where
		add_free_vars_of_non_recursive_call fun_def_level group_index (FunCall fc_index _) (lifted_function_called, free_vars, fun_defs,macro_defs)
			# ({fun_info = {fi_free_vars,fi_group_index}}, fun_defs) = fun_defs![fc_index]
			| (if (fi_group_index>=NoIndex) (fi_group_index==group_index) (-2-fi_group_index==group_index)) || (isEmpty fi_free_vars)
				= (lifted_function_called, free_vars, fun_defs,macro_defs)
				# (free_vars_added, free_vars) = add_free_variables fun_def_level fi_free_vars (False, free_vars)
				= (True, free_vars, fun_defs,macro_defs)
		add_free_vars_of_non_recursive_call fun_def_level group_index (MacroCall macro_module_index fc_index _) (lifted_function_called, free_vars, fun_defs,macro_defs)
			# ({fun_info = {fi_free_vars,fi_group_index}}, macro_defs) = macro_defs![macro_module_index,fc_index]
			| (if (fi_group_index>=NoIndex) (fi_group_index==group_index) (-2-fi_group_index==group_index)) || (isEmpty fi_free_vars)
				= (lifted_function_called, free_vars, fun_defs,macro_defs)
				# (free_vars_added, free_vars) = add_free_variables fun_def_level fi_free_vars (False, free_vars)
				= (True, free_vars, fun_defs,macro_defs)
		add_free_vars_of_non_recursive_call fun_def_level group_index (DclFunCall _ _) (lifted_function_called, free_vars, fun_defs,macro_defs)
			= (lifted_function_called, free_vars, fun_defs,macro_defs)

	add_free_vars_of_recursive_calls_to_functions group_index group (fun_defs,macro_defs)
		= foldSt (add_free_vars_of_recursive_calls_to_function group_index) group (False, (fun_defs,macro_defs))

	add_free_vars_of_recursive_calls_to_function group_index (FunctionOrIclMacroIndex fun) (free_vars_added, (fun_defs,macro_defs))
		# (fun_def=:{fun_info}, fun_defs) = fun_defs![fun]
		  { fi_free_vars,fi_def_level,fi_calls } = fun_info
		  (free_vars_added, fi_free_vars, fun_defs,macro_defs)
				= foldSt (add_free_vars_of_recursive_call fi_def_level group_index) fi_calls (free_vars_added, fi_free_vars, fun_defs,macro_defs)
		  fun_defs = { fun_defs & [fun] = { fun_def & fun_info = { fun_info & fi_free_vars = fi_free_vars }}}
		= (free_vars_added, (fun_defs,macro_defs))
	add_free_vars_of_recursive_calls_to_function group_index (DclMacroIndex module_index fun) (free_vars_added, (fun_defs,macro_defs))
		# (fun_def=:{fun_info}, macro_defs) = macro_defs![module_index,fun]
		  { fi_free_vars,fi_def_level,fi_calls } = fun_info
		  (free_vars_added, fi_free_vars, fun_defs,macro_defs)
				= foldSt (add_free_vars_of_recursive_call fi_def_level group_index) fi_calls (free_vars_added, fi_free_vars, fun_defs,macro_defs)
		  macro_defs = { macro_defs & [module_index,fun] = { fun_def & fun_info = { fun_info & fi_free_vars = fi_free_vars }}}
		= (free_vars_added, (fun_defs,macro_defs))

	add_free_vars_of_recursive_call fun_def_level group_index (FunCall fc_index _) (free_vars_added, free_vars, fun_defs,macro_defs)
		# ({fun_info = {fi_free_vars,fi_group_index}}, fun_defs) = fun_defs![fc_index]
		| if (fi_group_index>=NoIndex) (fi_group_index==group_index) (-2-fi_group_index==group_index)
			# (free_vars_added, free_vars) = add_free_variables fun_def_level fi_free_vars (free_vars_added, free_vars)
			= (free_vars_added, free_vars, fun_defs,macro_defs)
			= (free_vars_added, free_vars, fun_defs,macro_defs)
	add_free_vars_of_recursive_call fun_def_level group_index (MacroCall module_index fc_index _) (free_vars_added, free_vars, fun_defs,macro_defs)
		# ({fun_info = {fi_free_vars,fi_group_index}}, macro_defs) = macro_defs![module_index,fc_index]
		| if (fi_group_index>=NoIndex) (fi_group_index==group_index) (-2-fi_group_index==group_index)
			# (free_vars_added, free_vars) = add_free_variables fun_def_level fi_free_vars (free_vars_added, free_vars)
			= (free_vars_added, free_vars, fun_defs,macro_defs)
			= (free_vars_added, free_vars, fun_defs,macro_defs)
	add_free_vars_of_recursive_call fun_def_level group_index (DclFunCall _ _) (free_vars_added, free_vars, fun_defs,macro_defs)
		= (free_vars_added, free_vars, fun_defs,macro_defs)

	add_free_variables fun_level new_vars (free_vars_added, free_vars)
		= add_free_global_variables (skip_local_variables fun_level new_vars) (free_vars_added, free_vars)
	where
		skip_local_variables level vars=:[{fv_def_level}:rest_vars]
			| fv_def_level > level
				= skip_local_variables level rest_vars
				= vars
		skip_local_variables _ []
			= []

		add_free_global_variables []  (free_vars_added, free_vars)
			= (free_vars_added, free_vars)
		add_free_global_variables free_vars (free_vars_added, [])
			= (True, free_vars)
		add_free_global_variables [var:vars] (free_vars_added, free_vars)
			# (free_var_added, free_vars) = newFreeVariable var free_vars
			= add_free_global_variables vars (free_var_added || free_vars_added, free_vars)

	lift_functions group lift_state
		= foldSt lift_function group lift_state
	where
		lift_function (FunctionOrIclMacroIndex fun) {ls_x=ls_x=:{x_fun_defs=fun_defs=:{[fun] = fun_def}}, ls_var_heap=var_heap, ls_expr_heap=expr_heap}
			# {fi_free_vars} = fun_def.fun_info
			  fun_lifted = length fi_free_vars
			  (PartitioningFunction {cb_args,cb_rhs} fun_number) = fun_def.fun_body
			  (cb_args, var_heap) = add_lifted_args fi_free_vars cb_args var_heap
			  (cb_rhs, {ls_x,ls_var_heap,ls_expr_heap}) = lift cb_rhs { ls_x={ls_x & x_fun_defs = fun_defs}, ls_var_heap = var_heap, ls_expr_heap = expr_heap }
			  ls_var_heap = remove_lifted_args fi_free_vars ls_var_heap
			  fun_defs = ls_x.x_fun_defs
			  fun_defs = { fun_defs & [fun] = { fun_def & fun_lifted = fun_lifted, fun_body = PartitioningFunction {cb_args = cb_args, cb_rhs = cb_rhs} fun_number}}
			= {ls_x={ls_x & x_fun_defs=fun_defs}, ls_var_heap=ls_var_heap, ls_expr_heap= ls_expr_heap}
		lift_function (DclMacroIndex module_index fun) {ls_x=ls_x=:{x_macro_defs=macro_defs=:{[module_index,fun] = fun_def}}, ls_var_heap=var_heap, ls_expr_heap=expr_heap}
			# {fi_free_vars} = fun_def.fun_info
			  fun_lifted = length fi_free_vars
			  (PartitioningFunction {cb_args,cb_rhs} fun_number) = fun_def.fun_body
			  (cb_args, var_heap) = add_lifted_args fi_free_vars cb_args var_heap
			  (cb_rhs, {ls_x,ls_var_heap,ls_expr_heap}) = lift cb_rhs { ls_x={ls_x & x_macro_defs = macro_defs}, ls_var_heap = var_heap, ls_expr_heap = expr_heap }
			  ls_var_heap = remove_lifted_args fi_free_vars ls_var_heap
			  macro_defs = ls_x.x_macro_defs
			  macro_defs = { macro_defs & [module_index].[fun] = { fun_def & fun_lifted = fun_lifted, fun_body = PartitioningFunction {cb_args = cb_args, cb_rhs = cb_rhs} fun_number}}
			= {ls_x={ls_x & x_macro_defs=macro_defs}, ls_var_heap=ls_var_heap, ls_expr_heap= ls_expr_heap}

		remove_lifted_args vars var_heap
			= foldl (\var_heap {fv_ident,fv_info_ptr} -> writePtr fv_info_ptr VI_Empty var_heap) var_heap vars
	
		add_lifted_args [lifted_arg=:{fv_ident,fv_info_ptr} : lifted_args] args var_heap
			# (new_info_ptr, var_heap) = newPtr VI_Empty var_heap
			  args = [{ lifted_arg & fv_info_ptr = new_info_ptr } : args ]
			= add_lifted_args lifted_args args (writePtr fv_info_ptr (VI_LiftedVariable new_info_ptr) var_heap)
		add_lifted_args [] args var_heap
			= (args, var_heap)

unfoldVariable :: !BoundVar !*UnfoldState -> (!Expression, !*UnfoldState)
unfoldVariable var=:{var_info_ptr} us
	# (var_info, us) = readVarInfo var_info_ptr us
	= case var_info of
		VI_Expression expr
			-> (expr, us)
		VI_Variable var_ident var_info_ptr
		 	# (var_expr_ptr, us_symbol_heap) = newPtr EI_Empty us.us_symbol_heap
			-> (Var {var_ident = var_ident, var_info_ptr = var_info_ptr, var_expr_ptr = var_expr_ptr}, { us & us_symbol_heap = us_symbol_heap})
		_
			-> (Var var, us)

readVarInfo var_info_ptr us
	# (var_info, us_var_heap) = readPtr var_info_ptr us.us_var_heap
	  us = { us & us_var_heap = us_var_heap }
	= case var_info of
		VI_Extended _ original	-> (original, us)
		_						-> (var_info, us)

::	CopiedLocalFunction = { old_function_n :: !FunctionOrMacroIndex, new_function_n :: !Int }

::	CopiedLocalFunctions = {
		copied_local_functions :: [CopiedLocalFunction],
		used_copied_local_functions :: [CopiedLocalFunction],
		new_copied_local_functions :: [CopiedLocalFunction],
		next_local_function_n :: !Int
	}

::	UnfoldState =
	{	us_var_heap				:: !.VarHeap
	,	us_symbol_heap			:: !.ExpressionHeap
	,	us_local_macro_functions :: !Optional CopiedLocalFunctions
	}

class unfold a :: !a !*UnfoldState -> (!a, !*UnfoldState)

instance unfold Expression
where
	unfold (Var var) us
		= unfoldVariable var us
	unfold (App app) us
		# (app, us) = unfold app us
		= (App app, us)
	unfold (expr @ exprs) us
		# ((expr,exprs), us) = unfold (expr,exprs) us
		= (expr @ exprs, us)
	unfold (Let lad) us
		# (lad, us) = unfold lad us
		= (Let lad, us)
	unfold (Case case_expr) us
		# (case_expr, us) = unfold case_expr us
		= (Case case_expr, us)
	unfold (Selection selector_kind expr selectors) us
		# ((expr, selectors), us) = unfold (expr, selectors) us
		= (Selection selector_kind expr selectors, us)
	unfold (Update expr1 selectors expr2) us
		# (((expr1, expr2), selectors), us) = unfold ((expr1, expr2), selectors) us
		= (Update expr1 selectors expr2, us)
	unfold (RecordUpdate cons_symbol expression expressions) us
		# ((expression, expressions), us) = unfold (expression, expressions) us
		= (RecordUpdate cons_symbol expression expressions, us)
	unfold (TupleSelect symbol argn_nr expr) us
		# (expr, us) = unfold expr us
		= (TupleSelect symbol argn_nr expr, us)
	unfold (MatchExpr cons_ident expr) us
		# (expr, us) = unfold expr us
		= (MatchExpr cons_ident expr, us)
	unfold (IsConstructor expr cons_symbol cons_arity global_type_index case_ident position) us
		# (expr, us) = unfold expr us
		= (IsConstructor expr cons_symbol cons_arity global_type_index case_ident position, us)
	unfold (DynamicExpr expr) us
		# (expr, us) = unfold expr us
		= (DynamicExpr expr, us)
	unfold (TypeSignature type_function expr) us
		# (expr, us) = unfold expr us
		= (TypeSignature type_function expr, us)
	unfold expr us
		= (expr, us)

instance unfold DynamicExpr
where
	unfold expr=:{dyn_expr, dyn_info_ptr} us=:{us_symbol_heap}
		# (dyn_info, us_symbol_heap) = readPtr dyn_info_ptr us_symbol_heap
		# (new_dyn_info_ptr, us_symbol_heap) = newPtr dyn_info us_symbol_heap
		# (dyn_expr, us) = unfold dyn_expr {us & us_symbol_heap=us_symbol_heap}
		= ({ expr & dyn_expr = dyn_expr, dyn_info_ptr = new_dyn_info_ptr }, us)

instance unfold Selection
where
	unfold (ArraySelection array_select expr_ptr index_expr) us=:{us_symbol_heap}
		# (new_ptr, us_symbol_heap) = newPtr EI_Empty us_symbol_heap
		  (index_expr, us) = unfold index_expr { us & us_symbol_heap = us_symbol_heap}
		= (ArraySelection array_select new_ptr index_expr, us)
	unfold (DictionarySelection var selectors expr_ptr index_expr) us=:{us_symbol_heap}
		# (new_ptr, us_symbol_heap) = newPtr EI_Empty us_symbol_heap
		  (index_expr, us) = unfold index_expr { us & us_symbol_heap = us_symbol_heap}
		  (var_expr, us) = unfoldVariable var us
		= case var_expr of 
			App {app_symb={symb_kind= SK_Constructor _ }, app_args}
				# [RecordSelection _ field_index:_] = selectors
				  (App { app_symb = {symb_ident, symb_kind = SK_Function array_select}}) =  app_args !! field_index
				-> (ArraySelection { array_select & glob_object = { ds_ident = symb_ident, ds_arity = 2, ds_index = array_select.glob_object}}
							new_ptr index_expr, us)
			Var var
				-> (DictionarySelection var selectors new_ptr index_expr, us)
	unfold record_selection us
		= (record_selection, us)

instance unfold FreeVar
where
	unfold fv=:{fv_info_ptr,fv_ident} us=:{us_var_heap}
		# (new_info_ptr, us_var_heap) = newPtr VI_Empty us_var_heap
		= ({ fv & fv_info_ptr = new_info_ptr }, { us & us_var_heap = writePtr fv_info_ptr (VI_Variable fv_ident new_info_ptr) us_var_heap })

instance unfold App
where
	unfold app=:{app_symb={symb_kind}, app_args, app_info_ptr} us
		= case symb_kind of
			SK_Function {glob_module,glob_object}
				-> unfold_function_app app us
			SK_IclMacro macro_index
				-> unfold_function_app app us
			SK_DclMacro {glob_module,glob_object}
				-> unfold_function_app app us
			SK_OverloadedFunction {glob_module,glob_object}
				-> unfold_function_app app us
			SK_Generic {glob_module,glob_object} kind
				-> unfold_function_app app us
			SK_LocalMacroFunction local_macro_function_n
				-> unfold_local_macro_function (FunctionOrIclMacroIndex local_macro_function_n)
			SK_LocalDclMacroFunction {glob_module,glob_object}
				-> unfold_local_macro_function (DclMacroIndex glob_module glob_object)
			SK_Constructor _
				| not (isNilPtr app_info_ptr)
					# (app_info, us_symbol_heap) = readPtr app_info_ptr us.us_symbol_heap
					  new_app_info = app_info
					  (new_info_ptr, us_symbol_heap) = newPtr new_app_info us_symbol_heap
					  us={ us & us_symbol_heap = us_symbol_heap }
					  (app_args, us) = unfold app_args us
					-> ({ app & app_args = app_args, app_info_ptr = new_info_ptr}, us) 
					# (app_args, us) = unfold app_args us
					-> ({ app & app_args = app_args}, us)
			_
				# (app_args, us) = unfold app_args us
				-> ({ app & app_args = app_args, app_info_ptr = nilPtr}, us) 
	where
		unfold_function_app app=:{app_args, app_info_ptr} us
			# (new_info_ptr, us_symbol_heap) = newPtr EI_Empty us.us_symbol_heap
			# us={ us & us_symbol_heap = us_symbol_heap }
			# (app_args, us) = unfold app_args us
			= ({ app & app_args = app_args, app_info_ptr = new_info_ptr}, us) 

		unfold_local_macro_function local_macro_function_n
			# (us_local_macro_functions,us) = us!us_local_macro_functions
			= case us_local_macro_functions of
				No
					-> unfold_function_app app us
				uslocal_macro_functions=:(Yes local_macro_functions)
					# (new_local_macro_function_n,us_local_macro_functions) = determine_new_local_macro_function_n local_macro_function_n local_macro_functions
						with
							determine_new_local_macro_function_n local_macro_function_n local_macro_functions=:{copied_local_functions,used_copied_local_functions,new_copied_local_functions,next_local_function_n}
								# new_local_macro_function_n = search_new_local_macro_function_n used_copied_local_functions
								| new_local_macro_function_n>=0
									= (new_local_macro_function_n,us_local_macro_functions)
								# (new_local_macro_function_n,used_copied_local_functions) = search_new_local_macro_function_n_and_add_to_used_functions copied_local_functions used_copied_local_functions
								| new_local_macro_function_n>=0
									= (new_local_macro_function_n,Yes {local_macro_functions & used_copied_local_functions=used_copied_local_functions})
								# (new_local_macro_function_n,used_copied_local_functions) = search_new_local_macro_function_n_and_add_to_used_functions new_copied_local_functions used_copied_local_functions
								| new_local_macro_function_n>=0
									= (new_local_macro_function_n,Yes {local_macro_functions & used_copied_local_functions=used_copied_local_functions})
									# new_local_function = {old_function_n=local_macro_function_n,new_function_n=next_local_function_n}
									# new_copied_local_functions=new_copied_local_functions++[new_local_function]
									# us_local_macro_functions=Yes {copied_local_functions=copied_local_functions,
																	new_copied_local_functions=new_copied_local_functions,
																	used_copied_local_functions=[new_local_function:used_copied_local_functions],
																	next_local_function_n=next_local_function_n+1}
									= (next_local_function_n,us_local_macro_functions)
								where
									search_new_local_macro_function_n [{old_function_n,new_function_n}:local_functions]
										| local_macro_function_n==old_function_n
											= new_function_n
										 	= search_new_local_macro_function_n local_functions
									search_new_local_macro_function_n []
										= -1

									search_new_local_macro_function_n_and_add_to_used_functions [copied_local_function=:{old_function_n,new_function_n}:local_functions] used_copied_local_functions
										| local_macro_function_n==old_function_n
											= (new_function_n,[copied_local_function:used_copied_local_functions])
										 	= search_new_local_macro_function_n_and_add_to_used_functions local_functions used_copied_local_functions
									search_new_local_macro_function_n_and_add_to_used_functions [] used_copied_local_functions
										= (-1,used_copied_local_functions)
					# us={us & us_local_macro_functions=us_local_macro_functions}
					# app={app & app_symb.symb_kind=SK_LocalMacroFunction new_local_macro_function_n}
					-> unfold_function_app app us

instance unfold LetBind
where
	unfold bind=:{lb_src} us
		# (lb_src, us) = unfold lb_src us
		= ({ bind & lb_src = lb_src }, us)

instance unfold (Bind a b) | unfold a
where
	unfold bind=:{bind_src} us
		# (bind_src, us) = unfold bind_src us
		= ({ bind & bind_src = bind_src }, us)

instance unfold Case
where
	unfold kees=:{case_expr,case_guards,case_default,case_info_ptr} us
		# (old_case_info, us_symbol_heap) = readPtr case_info_ptr us.us_symbol_heap
		  new_case_info = old_case_info
		  (new_info_ptr, us_symbol_heap) = newPtr new_case_info us_symbol_heap
		  us = { us & us_symbol_heap = us_symbol_heap }
		  ((case_guards,case_default), us) = unfold (case_guards,case_default) us
		  (case_expr, us) = unfold case_expr us
		= ({ kees & case_expr = case_expr,case_guards = case_guards, case_default = case_default, case_info_ptr =  new_info_ptr}, us)

instance unfold Let
where
	unfold lad=:{let_strict_binds, let_lazy_binds, let_expr, let_info_ptr} us
		# (let_strict_binds, us) = copy_bound_vars let_strict_binds us
		# (let_lazy_binds, us) = copy_bound_vars let_lazy_binds us
		# (let_strict_binds, us) = unfold let_strict_binds us
		# (let_lazy_binds, us) = unfold let_lazy_binds us
		# (let_expr, us) = unfold let_expr us
		  (old_let_info, us_symbol_heap) = readPtr let_info_ptr us.us_symbol_heap
		  new_let_info = old_let_info
		  (new_info_ptr, us_symbol_heap) = newPtr new_let_info us_symbol_heap
		= ({lad & let_strict_binds = let_strict_binds, let_lazy_binds = let_lazy_binds, let_expr = let_expr, let_info_ptr = new_info_ptr},
			{ us & us_symbol_heap = us_symbol_heap })
		where
			copy_bound_vars [bind=:{lb_dst} : binds] us
				# (lb_dst, us) = unfold lb_dst us
				  (binds, us) = copy_bound_vars binds us
				= ([ {bind & lb_dst = lb_dst} : binds ], us)
			copy_bound_vars [] us
				= ([], us)

instance unfold CasePatterns
where
	unfold (AlgebraicPatterns type patterns) us
		# (patterns, us) = unfold patterns us
		= (AlgebraicPatterns type patterns, us)
	unfold (BasicPatterns type patterns) us
		# (patterns, us) = unfold patterns us
		= (BasicPatterns type patterns, us)
	unfold (OverloadedListPatterns type decons_expr patterns) us
		# (patterns, us) = unfold patterns us
		# (decons_expr, us) = unfold decons_expr us
		= (OverloadedListPatterns type decons_expr patterns, us)
	unfold (NewTypePatterns type patterns) us
		# (patterns, us) = unfold patterns us
		= (NewTypePatterns type patterns, us)
	unfold (DynamicPatterns patterns) us
		# (patterns, us) = unfold patterns us
		= (DynamicPatterns patterns, us)

instance unfold AlgebraicPattern
where
	unfold guard=:{ap_vars,ap_expr} us
		# (ap_vars, us) = unfold ap_vars us
		  (ap_expr, us) = unfold ap_expr us
		= ({ guard & ap_vars = ap_vars, ap_expr = ap_expr }, us)

instance unfold BasicPattern
where
	unfold guard=:{bp_expr} us
		# (bp_expr, us) = unfold bp_expr us
		= ({ guard & bp_expr = bp_expr }, us)

instance unfold DynamicPattern
where
	unfold guard=:{dp_var,dp_rhs} us
		# (dp_var, us) = unfold dp_var us
		  (dp_rhs, us) = unfold dp_rhs us
		= ({ guard & dp_var = dp_var, dp_rhs = dp_rhs }, us)

instance unfold [a] | unfold a
where
	unfold l us
		= map_st l us
		where
			map_st [x : xs] s
			 	# (x, s) = unfold x s
				  (xs, s) = map_st xs s
				#! s = s
				= ([x : xs], s)
			map_st [] s
			 	= ([], s)

instance unfold (a,b) | unfold a & unfold b
where
	unfold (a,b) us
		# (a,us) = unfold a us
		# (b,us) = unfold b us
		= ((a,b),us)

instance unfold (Optional a) | unfold a
where
	unfold (Yes x) us
		# (x, us) = unfold x us
		= (Yes x, us)
	unfold no us
		= (no, us)

updateFunctionCalls :: ![FunCall] ![FunCall] !*{# FunDef} !*SymbolTable
	-> (![FunCall], !*{# FunDef}, !*SymbolTable)
updateFunctionCalls calls collected_calls fun_defs symbol_table
	= foldSt add_function_call calls (collected_calls, fun_defs, symbol_table)
where
	add_function_call fc=:(FunCall fc_index _) (collected_calls, fun_defs, symbol_table)
//		# fc_index = trace_n ("add_function_call: "+++toString fc_index+++" ") fc_index
		# ({fun_ident}, fun_defs) = fun_defs![fc_index]
		  (collected_calls, symbol_table) = examineFunctionCall fun_ident fc (collected_calls, symbol_table) 
		= (collected_calls, fun_defs, symbol_table)

examineFunctionCall {id_info} fc=:(FunCall fc_index _) (calls, symbol_table)
	# (entry, symbol_table) = readPtr id_info symbol_table
	= case entry.ste_kind of
		STE_Called indexes
			| is_member fc_index indexes
				-> (calls, symbol_table)
				-> ([ fc : calls ], symbol_table <:= (id_info, { entry & ste_kind = STE_Called [ FunctionOrIclMacroIndex fc_index : indexes ]}))
		_
			-> ( [ fc : calls ], symbol_table <:=
					(id_info, { ste_kind = STE_Called [FunctionOrIclMacroIndex fc_index], ste_index = NoIndex, ste_def_level = NotALevel, ste_previous = entry, ste_doc = No }))
	where
		is_member fc_index [FunctionOrIclMacroIndex index:indexes]
			| fc_index==index
				= True
				= is_member fc_index indexes
		is_member fc_index [_:indexes]
			= is_member fc_index indexes
		is_member _ []
			= False
examineFunctionCall {id_info} fc=:(MacroCall macro_module_index fc_index _) (calls, symbol_table)
	# (entry, symbol_table) = readPtr id_info symbol_table
	= case entry.ste_kind of
		STE_Called indexes
			| is_member macro_module_index fc_index indexes
				-> (calls, symbol_table)
				-> ([ fc : calls ], symbol_table <:= (id_info, { entry & ste_kind = STE_Called [ DclMacroIndex macro_module_index fc_index : indexes ]}))
		_
			-> ( [ fc : calls ], symbol_table <:=
					(id_info, { ste_kind = STE_Called [DclMacroIndex macro_module_index fc_index], ste_index = NoIndex, ste_def_level = NotALevel, ste_previous = entry, ste_doc = No }))
	where
		is_member macro_module_index fc_index [DclMacroIndex module_index index:indexes]
			| fc_index==index && module_index==macro_module_index
				= True
				= is_member macro_module_index fc_index indexes
		is_member macro_module_index fc_index [_:indexes]
			= is_member macro_module_index fc_index indexes
		is_member _ _ []
			= False

::	ExpandState = {
		es_symbol_table		:: !.SymbolTable,
		es_var_heap			:: !.VarHeap,
		es_expression_heap 	:: !.ExpressionHeap,
		es_error 			:: !.ErrorAdmin,
		es_fun_defs			:: !.{#FunDef},
		es_macro_defs		:: !.{#.{#FunDef}},
		es_new_fun_def_numbers :: ![Int]
	}

copy_macro_and_local_functions ::		!FunDef !Int !*{#FunDef} !*{#*{#FunDef}} !*VarHeap !*ExpressionHeap
	-> (!FunDef,![(CopiedLocalFunction,FunDef)],!Int,!*{#FunDef},!*{#*{#FunDef}},!*VarHeap,!*ExpressionHeap)
copy_macro_and_local_functions macro new_function_index fun_defs macro_defs var_heap expr_heap
	# local_macro_functions = Yes {copied_local_functions=[],used_copied_local_functions=[],new_copied_local_functions=[],next_local_function_n=new_function_index+1}
	  (macro,local_macro_functions,var_heap,expr_heap)
		= copy_macro_or_local_macro_function macro local_macro_functions var_heap expr_heap
	  (new_functions,Yes {next_local_function_n},fun_defs,macro_defs,var_heap,expr_heap)
		= copy_local_functions_of_macro local_macro_functions [] fun_defs macro_defs var_heap expr_heap
	= (macro,new_functions,next_local_function_n,fun_defs,macro_defs,var_heap,expr_heap)

copy_local_functions_of_macro :: (Optional CopiedLocalFunctions) [CopiedLocalFunction] !*{#FunDef} !*{#*{#FunDef}} !*VarHeap !*ExpressionHeap
					-> (![(CopiedLocalFunction,FunDef)],!Optional CopiedLocalFunctions,!*{#FunDef},!*{#*{#FunDef}},!*VarHeap,!*ExpressionHeap)
copy_local_functions_of_macro local_macro_functions local_functions_to_be_copied fun_defs macro_defs var_heap expr_heap
	# (local_functions_to_be_copied,local_macro_functions) = add_new_local_functions_to_be_copied local_functions_to_be_copied local_macro_functions
		with
			add_new_local_functions_to_be_copied local_functions_to_be_copied local_macro_functions=:(Yes copied_local_macro_functions=:{new_copied_local_functions=[]})
				= (local_functions_to_be_copied,Yes {copied_local_macro_functions & used_copied_local_functions=[]})
			add_new_local_functions_to_be_copied local_functions_to_be_copied (Yes {copied_local_functions,new_copied_local_functions,next_local_function_n})
				# local_macro_functions=Yes {copied_local_functions=copied_local_functions++new_copied_local_functions,
											new_copied_local_functions=[],used_copied_local_functions=[],next_local_function_n=next_local_function_n}
				= (local_functions_to_be_copied++new_copied_local_functions,local_macro_functions)
	= case local_functions_to_be_copied of
		[]
			-> ([],local_macro_functions,fun_defs,macro_defs,var_heap,expr_heap)
		[(old_and_new_function_n=:{old_function_n,new_function_n}):local_functions_to_be_copied]
			# (function,fun_defs,macro_defs)
				= case old_function_n of
					FunctionOrIclMacroIndex old_function_index
						# (function,fun_defs)=fun_defs![old_function_index]			
						#! function_group_index=function.fun_info.fi_group_index
						# fun_defs & [old_function_index].fun_info.fi_group_index= if (function_group_index>NoIndex) (-2-function_group_index) function_group_index
						# function = {function & fun_info.fi_group_index=if (function_group_index<NoIndex) (-2-function_group_index) function_group_index}
						-> (function,fun_defs,macro_defs)
					DclMacroIndex old_function_module_index old_function_index
						# (function,macro_defs)=macro_defs![old_function_module_index,old_function_index]			
						#! function_group_index=function.fun_info.fi_group_index
						# macro_defs & [old_function_module_index].[old_function_index].fun_info.fi_group_index= if (function_group_index>NoIndex) (-2-function_group_index) function_group_index
						# function = {function & fun_info.fi_group_index=if (function_group_index<NoIndex) (-2-function_group_index) function_group_index}
						-> (function,fun_defs,macro_defs)
			# (function,local_macro_functions,var_heap,expr_heap) = copy_macro_or_local_macro_function function local_macro_functions var_heap expr_heap
			# (new_functions,local_macro_functions,fun_defs,macro_defs,var_heap,expr_heap)
				= copy_local_functions_of_macro local_macro_functions local_functions_to_be_copied fun_defs macro_defs var_heap expr_heap
			-> ([(old_and_new_function_n,function):new_functions],local_macro_functions,fun_defs,macro_defs,var_heap,expr_heap)

update_calls calls No
	= calls
update_calls calls (Yes {used_copied_local_functions=[]})
	= calls
update_calls calls (Yes {used_copied_local_functions})
	# calls = remove_old_calls calls
	= add_new_calls used_copied_local_functions calls
where
	remove_old_calls [call=:(FunCall fc_index _):calls]
		| contains_old_function_n used_copied_local_functions
//			# calls = trace ("remove_old_calls1: "+++toString fc_index) calls
			= remove_old_calls calls
//			# calls = trace ("remove_old_calls2: "+++toString fc_index) calls
			= [call:remove_old_calls calls]
	where
		contains_old_function_n [{old_function_n=FunctionOrIclMacroIndex old_function_index }:local_functions]
			= fc_index==old_function_index || contains_old_function_n local_functions
		contains_old_function_n [_:local_functions]
			= contains_old_function_n local_functions
		contains_old_function_n []
			= False
	remove_old_calls [call=:(MacroCall macro_module_index fc_index _):calls]
		| contains_old_function_n used_copied_local_functions
			= remove_old_calls calls
			= [call:remove_old_calls calls]
	where
		contains_old_function_n [{old_function_n=DclMacroIndex old_macro_module_index old_function_index }:local_functions]
			= fc_index==old_function_index && macro_module_index==old_macro_module_index || contains_old_function_n local_functions
		contains_old_function_n [_:local_functions]
			= contains_old_function_n local_functions
		contains_old_function_n []
			= False
	remove_old_calls [call=:(DclFunCall _ _):calls]
			= [call:remove_old_calls calls]
	remove_old_calls []
		= []
	
	add_new_calls [{new_function_n}:local_functions] calls
//		# local_functions = trace ("add_new_calls: "+++toString new_function_n) local_functions
		= add_new_calls local_functions [FunCall new_function_n NotALevel:calls]
	add_new_calls [] calls
		= calls

copy_macro_or_local_macro_function :: !FunDef !(Optional CopiedLocalFunctions) !*VarHeap !*ExpressionHeap -> (!FunDef,!Optional CopiedLocalFunctions,!*VarHeap,!*ExpressionHeap);
copy_macro_or_local_macro_function macro=:{fun_body = TransformedBody {tb_args,tb_rhs},fun_kind,fun_info={fi_local_vars,fi_calls}} local_macro_functions var_heap expr_heap
	# (tb_args,var_heap) = create_new_arguments tb_args var_heap
		with
			create_new_arguments [var=:{fv_ident,fv_info_ptr} : vars] var_heap
				# (new_vars,var_heap) = create_new_arguments vars var_heap
				# (new_info, var_heap) = newPtr VI_Empty var_heap
				# new_var = { fv_ident = fv_ident, fv_def_level = NotALevel, fv_info_ptr = new_info, fv_count = 0 }
				= ([new_var : new_vars], writePtr fv_info_ptr (VI_Variable fv_ident new_info) var_heap)
			create_new_arguments [] var_heap
				= ([],var_heap)
	# us = { us_symbol_heap = expr_heap, us_var_heap = var_heap, us_local_macro_functions = local_macro_functions }
	# (result_expr,{us_local_macro_functions,us_symbol_heap,us_var_heap}) = unfold tb_rhs us
	# (fi_local_vars,us_var_heap) = update_local_vars fi_local_vars us_var_heap
		with
			update_local_vars :: ![FreeVar] !*VarHeap -> (![FreeVar],!*VarHeap);
			update_local_vars [fv=:{fv_info_ptr}:fvs] var_heap
				# (fvs,var_heap)=update_local_vars fvs var_heap
				# (fv_info,var_heap) = readPtr fv_info_ptr var_heap
				# fv = {fv & fv_info_ptr=case fv_info of 
											(VI_Variable _ info_ptr) -> info_ptr
						}
				= ([fv:fvs],var_heap)
			update_local_vars [] var_heap
				= ([],var_heap)
	# fi_calls = update_calls fi_calls us_local_macro_functions
	= ({macro & fun_body = TransformedBody {tb_args=tb_args,tb_rhs=result_expr},fun_info.fi_local_vars=fi_local_vars,fun_info.fi_calls=fi_calls},us_local_macro_functions,
	   us_var_heap, us_symbol_heap)

unfoldMacro :: !FunDef ![Expression] !*ExpandInfo -> (!Expression, !*ExpandInfo)
unfoldMacro {fun_body =fun_body=: TransformedBody {tb_args,tb_rhs}, fun_info = {fi_calls},fun_kind,fun_ident} args (calls, es=:{es_var_heap,es_expression_heap,es_fun_defs})
	# (let_binds, var_heap) = bind_expressions tb_args args [] es_var_heap
	#! size_fun_defs = size es_fun_defs
	# copied_local_functions = Yes { copied_local_functions=[],used_copied_local_functions=[],new_copied_local_functions=[],next_local_function_n=size_fun_defs}
	# us = { us_symbol_heap = es_expression_heap, us_var_heap = var_heap, us_local_macro_functions = copied_local_functions }
	# (result_expr,{us_local_macro_functions,us_symbol_heap,us_var_heap}) = unfold tb_rhs us
	# es = {es & es_var_heap = us_var_heap, es_expression_heap = us_symbol_heap}
	# fi_calls = update_calls fi_calls us_local_macro_functions
	# {es_fun_defs,es_macro_defs,es_var_heap,es_expression_heap,es_symbol_table,es_new_fun_def_numbers} = es
	  (new_functions,us_local_macro_functions,es_fun_defs,es_macro_defs,es_var_heap,es_expression_heap)
		= copy_local_functions_of_macro us_local_macro_functions [] es_fun_defs es_macro_defs es_var_heap es_expression_heap
	# (es_fun_defs,es_new_fun_def_numbers) = case new_functions of
		[]
			-> (es_fun_defs,es_new_fun_def_numbers)
		_
			# last_function_index = case us_local_macro_functions of (Yes {next_local_function_n}) -> next_local_function_n-1
			# new_fun_defs = new_fun_defs
				with
					new_fun_defs :: *{!FunDef}
					new_fun_defs => {fun_def \\ (_,fun_def)<-new_functions}
//			-> ({if (i<size_fun_defs) es_fun_defs.[i] new_fun_defs.[i-size_fun_defs] \\ i<-[0..last_function_index]} // inefficient
//				,[size_fun_defs:es_new_fun_def_numbers])
//			#! new_fun_defs = arrayConcat es_fun_defs new_fun_defs	// leads to backend crash!
			# new_fun_defs = arrayConcat es_fun_defs new_fun_defs
			-> (new_fun_defs, [size_fun_defs:es_new_fun_def_numbers])
	# (calls, fun_defs, es_symbol_table) = updateFunctionCalls fi_calls calls es_fun_defs es_symbol_table
	| isEmpty let_binds
		# es & es_macro_defs=es_macro_defs, es_var_heap=es_var_heap, es_symbol_table = es_symbol_table, es_expression_heap=es_expression_heap, es_fun_defs=fun_defs, es_new_fun_def_numbers=es_new_fun_def_numbers
		= (result_expr, (calls, es))
		# (new_info_ptr, es_expression_heap) = newPtr EI_Empty es_expression_heap
		# es & es_macro_defs=es_macro_defs, es_var_heap=es_var_heap, es_symbol_table = es_symbol_table, es_expression_heap=es_expression_heap, es_fun_defs=fun_defs, es_new_fun_def_numbers=es_new_fun_def_numbers
		# result_expr=Let { let_strict_binds = [], let_lazy_binds = let_binds, let_expr = result_expr, let_info_ptr = new_info_ptr, let_expr_position = NoPos }
		= (result_expr, (calls, es))
where
	bind_expressions [var : vars] [expr : exprs] binds var_heap
		# (binds, var_heap) = bind_expressions vars exprs binds var_heap
		= bind_expression var expr binds var_heap
	bind_expressions _ _ binds var_heap
		= (binds, var_heap)

	bind_expression :: FreeVar Expression [LetBind] *VarHeap -> (![LetBind],!*VarHeap);
	bind_expression {fv_count} expr binds var_heap
		| fv_count == 0
			= (binds, var_heap)
	bind_expression {fv_info_ptr} (Var {var_ident,var_info_ptr}) binds var_heap
		= (binds, writePtr fv_info_ptr (VI_Variable var_ident var_info_ptr) var_heap)
	bind_expression {fv_ident,fv_info_ptr,fv_count} expr binds var_heap
		| fv_count == 1
			= (binds, writePtr fv_info_ptr (VI_Expression expr) var_heap)
		# (new_info, var_heap) = newPtr VI_Empty var_heap
		  new_var = { fv_ident = fv_ident, fv_def_level = NotALevel, fv_info_ptr = new_info, fv_count = 0 }
		= ([{ lb_src = expr, lb_dst = new_var, lb_position = NoPos} : binds], writePtr fv_info_ptr (VI_Variable fv_ident new_info) var_heap)

:: UnexpandedDclMacros:==[(Int,Int,FunDef)]

::	PartitioningState =
	{	ps_symbol_table	:: !.SymbolTable
	,	ps_var_heap		:: !.VarHeap
	,	ps_symbol_heap	:: !.ExpressionHeap
	,	ps_error		:: !.ErrorAdmin
	,	ps_fun_defs		:: !.{#FunDef}
	,	ps_macro_defs	:: !.{#.{#FunDef}}
	,	ps_next_num		:: !Int
	,	ps_next_group	:: !Int
	,	ps_groups		:: ![[FunctionOrMacroIndex]]
	,	ps_deps			:: ![FunctionOrMacroIndex]
	,	ps_unexpanded_dcl_macros :: !UnexpandedDclMacros
	}

:: PartitioningInfo = ! {
	pi_predef_symbols_for_transform :: !PredefSymbolsForTransform,
	pi_main_dcl_module_n :: !Int,
	pi_reset_body_of_rhs_macros :: !Bool
   }

NotChecked :== -1	

:: PredefSymbolsForTransform = { predef_alias_dummy :: !PredefinedSymbol, predef_and :: !PredefinedSymbol, predef_or :: !PredefinedSymbol };

reset_body_of_rhs_macros ps_deps fun_defs macro_defs
	= foldSt reset_body_of_rhs_macro ps_deps (fun_defs,macro_defs)
	where
		reset_body_of_rhs_macro (FunctionOrIclMacroIndex macro_index) (fun_defs,macro_defs)
			# (macro_def,fun_defs) = fun_defs![macro_index]
			= case macro_def.fun_body of
				RhsMacroBody body
					-> ({ fun_defs & [macro_index] = { macro_def & fun_body = CheckedBody body }},macro_defs)
				_
					-> (fun_defs,macro_defs)
		reset_body_of_rhs_macro (DclMacroIndex module_index macro_index) (fun_defs,macro_defs)
			# (macro_def,macro_defs) = macro_defs![module_index,macro_index]
			= case macro_def.fun_body of
				RhsMacroBody body
					-> (fun_defs,{ macro_defs & [module_index,macro_index] = { macro_def & fun_body = CheckedBody body }})
				_
					-> (fun_defs,macro_defs)

expand_simple_macro mod_index macro=:{fun_body = CheckedBody body, fun_info, fun_ident, fun_pos,fun_kind}
		predef_symbols_for_transform ps=:{ps_symbol_table,ps_symbol_heap,ps_var_heap,ps_fun_defs,ps_macro_defs,ps_error}
  	# identPos = newPosition fun_ident fun_pos
	# es = { es_symbol_table = ps_symbol_table, es_var_heap = ps_var_heap,
			 es_expression_heap = ps_symbol_heap, es_error = setErrorAdmin identPos ps_error,
			 es_fun_defs=ps_fun_defs, es_macro_defs=ps_macro_defs, es_new_fun_def_numbers=[]
		   }
	# (tb_args, tb_rhs, local_vars, fi_calls, fi_dynamics,{es_symbol_table, es_var_heap, es_expression_heap, es_error,es_fun_defs,es_macro_defs})
			= expandMacrosInBody [] body fun_info.fi_dynamics predef_symbols_for_transform False es
	# macro = { macro & fun_body = TransformedBody { tb_args = tb_args, tb_rhs = tb_rhs},
	  			fun_info = { fun_info & fi_calls = fi_calls, fi_local_vars = local_vars, fi_dynamics=fi_dynamics }}
	= ( macro, { ps & ps_symbol_table = es_symbol_table, ps_symbol_heap = es_expression_heap, ps_var_heap = es_var_heap, ps_fun_defs = es_fun_defs,ps_macro_defs=es_macro_defs,ps_error = es_error })

expand_dcl_macro_if_simple mod_index macro_index macro=:{fun_body = CheckedBody body, fun_info}
		predef_symbols_for_transform ps=:{ps_symbol_table,ps_symbol_heap,ps_var_heap,ps_fun_defs,ps_macro_defs,ps_error}
	| macros_are_simple fun_info.fi_calls mod_index ps_fun_defs ps_macro_defs && has_no_curried_macro body.cb_rhs ps_fun_defs ps_macro_defs
		# (macro,ps) = expand_simple_macro mod_index macro predef_symbols_for_transform ps
		= { ps & ps_macro_defs.[mod_index,macro_index] = macro }
		= { ps & ps_deps = [DclMacroIndex mod_index macro_index:ps.ps_deps], ps_macro_defs.[mod_index,macro_index] = { macro & fun_body = RhsMacroBody body }}

expand_icl_macro_if_simple mod_index macro_index macro=:{fun_body = CheckedBody body, fun_info}
		predef_symbols_for_transform ps=:{ps_symbol_table,ps_symbol_heap,ps_var_heap,ps_fun_defs,ps_macro_defs,ps_error}
	| macros_are_simple fun_info.fi_calls mod_index ps_fun_defs ps_macro_defs && has_no_curried_macro body.cb_rhs ps_fun_defs ps_macro_defs
		# (macro,ps) = expand_simple_macro mod_index macro predef_symbols_for_transform ps
		= { ps & ps_fun_defs.[macro_index] = macro }
		= { ps & ps_deps = [FunctionOrIclMacroIndex macro_index:ps.ps_deps], ps_fun_defs.[macro_index] = { macro & fun_body = RhsMacroBody body }}

macros_are_simple :: [FunCall] Int {#FunDef} {#{#FunDef}} -> Bool;
macros_are_simple [] mod_index fun_defs macro_defs
	= True
macros_are_simple [FunCall fc_index _ : calls ] mod_index fun_defs macro_defs
	# {fun_kind,fun_body, fun_ident} = fun_defs.[fc_index]
	= is_a_pattern_macro fun_kind fun_body && macros_are_simple calls mod_index fun_defs macro_defs
macros_are_simple [MacroCall module_index fc_index _ : calls ] mod_index fun_defs macro_defs
	# {fun_kind,fun_body, fun_ident} = macro_defs.[module_index,fc_index]
	= is_a_pattern_macro fun_kind fun_body && macros_are_simple calls mod_index fun_defs macro_defs
macros_are_simple [DclFunCall dcl_fun_index _ : calls ] mod_index fun_defs macro_defs
	= dcl_fun_index<>mod_index && macros_are_simple calls mod_index fun_defs macro_defs

is_a_pattern_macro FK_Macro (TransformedBody {tb_args})
	= True
is_a_pattern_macro _ _
	= False

visit_macro mod_index max_fun_nr predef_symbols_for_transform (FunCall fc_index _) ps
	= partitionate_icl_macro mod_index max_fun_nr predef_symbols_for_transform fc_index ps
visit_macro mod_index max_fun_nr predef_symbols_for_transform (MacroCall macro_module_index fc_index _) ps
	= partitionate_dcl_macro macro_module_index max_fun_nr predef_symbols_for_transform fc_index ps
visit_macro mod_index max_fun_nr predef_symbols_for_transform (DclFunCall _ _) ps
	= ps

partitionate_dcl_macro mod_index max_fun_nr predef_symbols_for_transform macro_index ps
	# (macro_def, ps) = ps!ps_macro_defs.[mod_index,macro_index]
	| case macro_def.fun_kind of FK_Macro->True ; _ -> False
	 	= case macro_def.fun_body of
			CheckedBody body
	 			# ps={ ps & ps_macro_defs.[mod_index,macro_index] = { macro_def & fun_body = PartitioningMacro }}
		  		# macros_pi = foldSt (visit_macro mod_index max_fun_nr predef_symbols_for_transform) macro_def.fun_info.fi_calls ps
				-> expand_dcl_macro_if_simple mod_index macro_index macro_def predef_symbols_for_transform macros_pi
			PartitioningMacro
	  			# identPos = newPosition macro_def.fun_ident macro_def.fun_pos
	 			-> { ps &  ps_error = checkError macro_def.fun_ident "recursive macro definition" (setErrorAdmin identPos ps.ps_error)  }
	 		_
	 			-> ps
		= ps

partitionate_icl_macro mod_index max_fun_nr predef_symbols_for_transform macro_index ps
	# (macro_def, ps) = ps!ps_fun_defs.[macro_index]
	| case macro_def.fun_kind of FK_Macro->True; _ -> False
	 	= case macro_def.fun_body of
	 		CheckedBody body
	 			# ps={ ps & ps_fun_defs.[macro_index] = { macro_def & fun_body = PartitioningMacro }}
		  		# macros_pi = foldSt (visit_macro mod_index max_fun_nr predef_symbols_for_transform) macro_def.fun_info.fi_calls ps
				-> expand_icl_macro_if_simple mod_index macro_index macro_def predef_symbols_for_transform macros_pi
	 		PartitioningMacro
	  			# identPos = newPosition macro_def.fun_ident macro_def.fun_pos
	 			-> { ps &  ps_error = checkError macro_def.fun_ident "recursive macro definition" (setErrorAdmin identPos ps.ps_error)  }
	 		_
	 			-> ps
		= ps

partitionateDclMacros :: !IndexRange !Index !PredefSymbolsForTransform !*{#*{#FunDef}} !*VarHeap !*ExpressionHeap !*SymbolTable !*ErrorAdmin
																   -> (!*{#*{#FunDef}},!*VarHeap,!*ExpressionHeap,!*SymbolTable,!*ErrorAdmin )
partitionateDclMacros {ir_from,ir_to} mod_index predef_symbols_for_transform macro_defs var_heap symbol_heap symbol_table error
	#! max_fun_nr = cMAXINT
	# partitioning_info = { ps_var_heap = var_heap, ps_symbol_heap = symbol_heap,
							ps_symbol_table = symbol_table, ps_fun_defs={}, ps_macro_defs=macro_defs,
							ps_error = error, ps_deps = [], ps_next_num = 0, ps_next_group = 0, ps_groups = [],
							ps_unexpanded_dcl_macros=[] }
	  {ps_symbol_table, ps_var_heap, ps_symbol_heap, ps_fun_defs, ps_macro_defs, ps_error, ps_next_group, ps_groups, ps_deps}
	  		= iFoldSt (partitionate_dcl_macro mod_index max_fun_nr predef_symbols_for_transform) ir_from ir_to partitioning_info
	  (_,macro_defs) = reset_body_of_rhs_macros ps_deps ps_fun_defs ps_macro_defs
	= (macro_defs, ps_var_heap, ps_symbol_heap, ps_symbol_table, ps_error)

partitionateIclMacros :: !IndexRange !Index !PredefSymbolsForTransform !*{#FunDef} !*{#*{#FunDef}} !*VarHeap !*ExpressionHeap !*SymbolTable !*ErrorAdmin
																   -> (!*{#FunDef},!*{#*{#FunDef}},!*VarHeap,!*ExpressionHeap,!*SymbolTable,!*ErrorAdmin )
partitionateIclMacros {ir_from,ir_to} mod_index predef_symbols_for_transform fun_defs macro_defs var_heap symbol_heap symbol_table error
	#! max_fun_nr = cMAXINT
	# partitioning_info = { ps_var_heap = var_heap, ps_symbol_heap = symbol_heap,
							ps_symbol_table = symbol_table, ps_fun_defs=fun_defs, ps_macro_defs=macro_defs,
							ps_error = error, ps_deps = [], ps_next_num = 0, ps_next_group = 0, ps_groups = [],
							ps_unexpanded_dcl_macros=[] }
	  {ps_symbol_table, ps_var_heap, ps_symbol_heap, ps_fun_defs, ps_macro_defs, ps_error, ps_next_group, ps_groups, ps_deps}
	  		= iFoldSt (partitionate_icl_macro mod_index max_fun_nr predef_symbols_for_transform) ir_from ir_to partitioning_info
	  (fun_defs,macro_defs) = reset_body_of_rhs_macros ps_deps ps_fun_defs ps_macro_defs
	= (fun_defs,macro_defs, ps_var_heap, ps_symbol_heap, ps_symbol_table, ps_error)

add_new_macros_to_groups :: ![Int] !Int Int  *{#FunDef}  [FunctionOrMacroIndex]  [[FunctionOrMacroIndex]]
								   -> (!Int,!*{#FunDef},![FunctionOrMacroIndex],![[FunctionOrMacroIndex]]);
add_new_macros_to_groups [new_macro_fun_def_index] n_fun_defs_after_expanding_macros ps_next_group es_fun_defs functions_in_group ps_groups
	= add_new_macro_and_local_functions_to_groups new_macro_fun_def_index n_fun_defs_after_expanding_macros ps_next_group es_fun_defs functions_in_group ps_groups
add_new_macros_to_groups [new_macro_fun_def_index:macro_fun_def_numbers=:[next_macro_fun_def_index:_]] n_fun_defs_after_expanding_macros ps_next_group es_fun_defs functions_in_group ps_groups
	# (ps_next_group,es_fun_defs,functions_in_group,ps_groups)
		= add_new_macro_and_local_functions_to_groups new_macro_fun_def_index next_macro_fun_def_index ps_next_group es_fun_defs functions_in_group ps_groups
	= add_new_macros_to_groups macro_fun_def_numbers n_fun_defs_after_expanding_macros ps_next_group es_fun_defs functions_in_group ps_groups
add_new_macros_to_groups [] n_fun_defs_after_expanding_macros ps_next_group es_fun_defs functions_in_group ps_groups
	= (ps_next_group,es_fun_defs,functions_in_group,ps_groups)

add_new_macro_and_local_functions_to_groups :: !Int !Int Int  *{#FunDef}  [FunctionOrMacroIndex]  [[FunctionOrMacroIndex]]
													-> (!Int,!*{#FunDef},![FunctionOrMacroIndex],![[FunctionOrMacroIndex]]);
add_new_macro_and_local_functions_to_groups new_macro_fun_def_index n_fun_defs_after_expanding_macros ps_next_group es_fun_defs functions_in_group ps_groups
	# (ps_next_group,es_fun_defs,functions_in_group,macros)
		= add_macros_to_current_group new_macro_fun_def_index n_fun_defs_after_expanding_macros ps_next_group es_fun_defs functions_in_group []
	# (macros_with_group_numbers,es_fun_defs) = add_group_numbers macros es_fun_defs
		with
			add_group_numbers [fun_def_index:l] es_fun_defs
				# (group_number,es_fun_defs) = es_fun_defs![fun_def_index].fun_info.fi_group_index
//				# group_number=trace ("add_group_numbers: "+++toString fun_def_index+++" "+++toString group_number+++"\n") group_number;
				# (l,es_fun_defs) = add_group_numbers l es_fun_defs
				= ([(fun_def_index,group_number):l],es_fun_defs)
			add_group_numbers [] es_fun_defs
				= ([],es_fun_defs)
	# sorted_macros_with_group_numbers = sortBy (\(_,group_number1) (_,group_number2) -> group_number1<group_number2) macros_with_group_numbers
	# (ps_next_group,ps_groups) = partition_macros_in_groups sorted_macros_with_group_numbers [] (-1) ps_next_group ps_groups
		with
			partition_macros_in_groups [(fun_def_index,fun_def_group_number):l] [] group_number ps_next_group ps_groups
				= partition_macros_in_groups l [FunctionOrIclMacroIndex fun_def_index] fun_def_group_number ps_next_group ps_groups
			partition_macros_in_groups [(fun_def_index,fun_def_group_number):l] group group_number ps_next_group ps_groups
				| fun_def_group_number==group_number
					= partition_macros_in_groups l [FunctionOrIclMacroIndex fun_def_index:group] group_number ps_next_group ps_groups
					# ps_groups=[group:ps_groups]
					# ps_next_group=ps_next_group+1
					= partition_macros_in_groups l [FunctionOrIclMacroIndex fun_def_index] fun_def_group_number ps_next_group ps_groups			
			partition_macros_in_groups [] [] group_number ps_next_group ps_groups
				= (ps_next_group,ps_groups)
			partition_macros_in_groups [] last_group group_number ps_next_group ps_groups
				= (ps_next_group+1,[last_group:ps_groups])
	= (ps_next_group,es_fun_defs,functions_in_group,ps_groups)

add_macros_to_current_group :: !Int !Int Int *{#FunDef} [FunctionOrMacroIndex] [Int] -> (!Int,!*{#FunDef},![FunctionOrMacroIndex],![Int]);
add_macros_to_current_group new_macro_fun_def_index n_fun_defs_after_expanding_macros ps_next_group es_fun_defs functions_in_group macros
	| new_macro_fun_def_index>=n_fun_defs_after_expanding_macros
		= (ps_next_group,es_fun_defs,functions_in_group,macros)
	| es_fun_defs.[new_macro_fun_def_index].fun_info.fi_group_index<=NoIndex
		= abort ("add_macros_to_current_group: "+++toString new_macro_fun_def_index)
//				+++" "+++toString es_fun_defs.[new_macro_fun_def_index].fun_info.fi_group_index)

	| es_fun_defs.[new_macro_fun_def_index].fun_info.fi_group_index==ps_next_group
//		# new_macro_fun_def_index=trace ("add_macros_to_current_group1: "+++toString new_macro_fun_def_index+++"\n") new_macro_fun_def_index;
		# functions_in_group=[FunctionOrIclMacroIndex new_macro_fun_def_index:functions_in_group]
		= add_macros_to_current_group (new_macro_fun_def_index+1) n_fun_defs_after_expanding_macros ps_next_group es_fun_defs functions_in_group macros

//		# new_macro_fun_def_index=trace ("add_macros_to_current_group2: "+++toString new_macro_fun_def_index+++"\n") new_macro_fun_def_index;
//		# ps_groups=[[new_macro_fun_def_index]:ps_groups]
//		# ps_next_group=ps_next_group+1
		= add_macros_to_current_group (new_macro_fun_def_index+1) n_fun_defs_after_expanding_macros ps_next_group es_fun_defs functions_in_group [new_macro_fun_def_index:macros]

has_no_curried_macro cb_rhs fun_defs macro_defs
	= has_no_curried_macro_CheckedAlternative cb_rhs
where
	has_no_curried_macro_CheckedAlternative [{ca_rhs}:cas]
		= has_no_curried_macro_Expression ca_rhs && has_no_curried_macro_CheckedAlternative cas
	has_no_curried_macro_CheckedAlternative []
		= True

	has_no_curried_macro_Expression (App {app_symb={symb_kind = SK_DclMacro {glob_object,glob_module}}, app_args})
		| macro_defs.[glob_module,glob_object].fun_arity<>length app_args
			= False;
			= has_no_curried_macro_Expressions app_args
	has_no_curried_macro_Expression (App {app_symb={symb_kind = SK_IclMacro glob_object}, app_args})
		| fun_defs.[glob_object].fun_arity<>length app_args
			= False;
			= has_no_curried_macro_Expressions app_args
	has_no_curried_macro_Expression (App {app_args})
		= has_no_curried_macro_Expressions app_args
	has_no_curried_macro_Expression (expr @ exprs)
		= has_no_curried_macro_Expression expr && has_no_curried_macro_Expressions exprs
	has_no_curried_macro_Expression (Let {let_strict_binds, let_lazy_binds, let_expr})
		= has_no_curried_macro_LetBinds let_strict_binds && has_no_curried_macro_LetBinds let_lazy_binds && has_no_curried_macro_Expression let_expr
		where
				has_no_curried_macro_LetBinds [{lb_src}:xs]
					= has_no_curried_macro_Expression lb_src && has_no_curried_macro_LetBinds xs
				has_no_curried_macro_LetBinds []
					= True
	has_no_curried_macro_Expression (Case {case_expr,case_guards,case_default})
		=	has_no_curried_macro_Expression case_expr && has_no_curried_macro_CasePatterns case_guards && has_no_curried_macro_OptionalExpression case_default
		where
			has_no_curried_macro_CasePatterns (AlgebraicPatterns type patterns)
				= has_no_curried_macro_AlgebraicPatterns patterns
			has_no_curried_macro_CasePatterns (BasicPatterns type patterns)
				= has_no_curried_macro_BasicPatterns patterns
			where
				has_no_curried_macro_BasicPatterns [{bp_expr}:patterns]
					= has_no_curried_macro_Expression bp_expr && has_no_curried_macro_BasicPatterns patterns
				has_no_curried_macro_BasicPatterns []
					= True
			has_no_curried_macro_CasePatterns (NewTypePatterns type patterns)
				= has_no_curried_macro_AlgebraicPatterns patterns
			has_no_curried_macro_CasePatterns (DynamicPatterns patterns)
				= has_no_curried_macro_DynamicPatterns patterns
			where
				has_no_curried_macro_DynamicPatterns [{dp_rhs}:patterns]
					= has_no_curried_macro_Expression dp_rhs && has_no_curried_macro_DynamicPatterns patterns
				has_no_curried_macro_DynamicPatterns []
					= True

			has_no_curried_macro_AlgebraicPatterns [{ap_expr}:patterns]
				= has_no_curried_macro_Expression ap_expr && has_no_curried_macro_AlgebraicPatterns patterns
			has_no_curried_macro_AlgebraicPatterns []
				= True

			has_no_curried_macro_OptionalExpression (Yes expr)
				= has_no_curried_macro_Expression expr
			has_no_curried_macro_OptionalExpression No
				= True
	has_no_curried_macro_Expression (Selection is_unique expr selectors)
		= has_no_curried_macro_Expression expr && has_no_curried_macro_Selections selectors
	has_no_curried_macro_Expression (Update expr1 selectors expr2)
		= has_no_curried_macro_Expression expr1 && has_no_curried_macro_Expression expr2 && has_no_curried_macro_Selections selectors
	has_no_curried_macro_Expression (RecordUpdate cons_symbol expression expressions)
		= has_no_curried_macro_Expression expression && has_no_curried_macro_Binds expressions
		where
			has_no_curried_macro_Binds [{bind_src}:binds]
				= has_no_curried_macro_Expression bind_src && has_no_curried_macro_Binds binds
			has_no_curried_macro_Binds []
				= True
	has_no_curried_macro_Expression (TupleSelect symbol argn_nr expr)
		= has_no_curried_macro_Expression expr
	has_no_curried_macro_Expression (MatchExpr cons_ident expr)
		= has_no_curried_macro_Expression expr
	has_no_curried_macro_Expression (IsConstructor expr cons_symbol cons_arity global_type_index case_ident position)
		= has_no_curried_macro_Expression expr
	has_no_curried_macro_Expression (TypeSignature _ expr)
		= has_no_curried_macro_Expression expr
	has_no_curried_macro_Expression expr
		= True

	has_no_curried_macro_Expressions [x:xs]
		= has_no_curried_macro_Expression x && has_no_curried_macro_Expressions xs
	has_no_curried_macro_Expressions []
		= True

	has_no_curried_macro_Selections [ArraySelection array_select expr_ptr index_expr:selections]
		= has_no_curried_macro_Expression index_expr && has_no_curried_macro_Selections selections
	has_no_curried_macro_Selections [record_selection:selections]
		= has_no_curried_macro_Selections selections
	has_no_curried_macro_Selections []
		= True

partitionateAndLiftFunctions :: ![IndexRange] !Index !PredefSymbolsForTransform !*{#FunDef} !*{#*{#FunDef}} !*VarHeap !*ExpressionHeap !*SymbolTable !*ErrorAdmin
																-> (!*{!Group}, !*{#FunDef},!*{#*{#FunDef}},!*VarHeap,!*ExpressionHeap,!*SymbolTable,!*ErrorAdmin)
partitionateAndLiftFunctions ranges main_dcl_module_n predef_symbols_for_transform fun_defs macro_defs var_heap symbol_heap symbol_table error
	#! max_fun_nr = cMAXINT
	# partitioning_info = {	ps_var_heap = var_heap, ps_symbol_heap = symbol_heap, ps_symbol_table = symbol_table, ps_fun_defs=fun_defs, ps_macro_defs=macro_defs,
							ps_error = error, ps_deps = [], ps_next_num = 0, ps_next_group = 0, ps_groups = [],
							ps_unexpanded_dcl_macros=[] }
	  {ps_groups, ps_symbol_table, ps_var_heap, ps_symbol_heap, ps_fun_defs, ps_macro_defs, ps_error,ps_unexpanded_dcl_macros}
	  		= foldSt (partitionate_functions main_dcl_module_n max_fun_nr) ranges partitioning_info
	# (reversed_ps_groups,fun_defs) = remove_macros_from_groups_and_reverse ps_groups ps_fun_defs []
	# groups = { {group_members = group} \\ group <- reversed_ps_groups }
	# macro_defs = restore_unexpanded_dcl_macros ps_unexpanded_dcl_macros ps_macro_defs
	= (groups, fun_defs, macro_defs, ps_var_heap, ps_symbol_heap, ps_symbol_table, ps_error)
where
	partitionate_functions mod_index max_fun_nr {ir_from,ir_to} ps
		= iFoldSt (partitionate_global_function mod_index max_fun_nr) ir_from ir_to ps

	partitionate_global_function mod_index max_fun_nr fun_index ps
		# pi = {pi_predef_symbols_for_transform=predef_symbols_for_transform,pi_main_dcl_module_n=main_dcl_module_n,pi_reset_body_of_rhs_macros=False}
		# (_,ps) = partitionate_function mod_index max_fun_nr fun_index pi ps
		= ps

get_predef_symbols_for_transform :: !PredefinedSymbols -> PredefSymbolsForTransform
get_predef_symbols_for_transform predef_symbols
	= ({predef_alias_dummy=predef_symbols.[PD_DummyForStrictAliasFun],predef_and=predef_symbols.[PD_AndOp],predef_or=predef_symbols.[PD_OrOp]})

partitionateAndLiftMacro :: !Int !Int !Index !PredefinedSymbols !Int !*{#FunDef} !*{#*{#FunDef}} !*VarHeap !*ExpressionHeap !*SymbolTable !*ErrorAdmin
								   -> (![[Int]],!UnexpandedDclMacros,!*{#FunDef},!*{#*{#FunDef}},!*VarHeap,!*ExpressionHeap,!*SymbolTable,!*ErrorAdmin)
partitionateAndLiftMacro macro_module_index macro_index main_dcl_module_n predef_symbols next_group_n fun_defs macro_defs var_heap symbol_heap symbol_table error
	# predef_symbols_for_transform = get_predef_symbols_for_transform predef_symbols
	#! max_fun_nr = cMAXINT
	# partitioning_state = {ps_var_heap = var_heap, ps_symbol_heap = symbol_heap, ps_symbol_table = symbol_table, ps_fun_defs=fun_defs, ps_macro_defs=macro_defs,
							ps_error = error, ps_deps = [], ps_next_num = 0, ps_next_group = next_group_n, ps_groups = [],
							ps_unexpanded_dcl_macros=[] }
	  pi = {pi_predef_symbols_for_transform=predef_symbols_for_transform,pi_main_dcl_module_n=main_dcl_module_n,pi_reset_body_of_rhs_macros=True}
	  (_, {ps_groups, ps_symbol_table, ps_var_heap, ps_symbol_heap, ps_fun_defs, ps_macro_defs, ps_error,ps_unexpanded_dcl_macros})
	  		= partitionate_macro main_dcl_module_n max_fun_nr macro_module_index macro_index pi partitioning_state
	# (reversed_ps_groups,fun_defs) = remove_macros_from_groups_and_reverse ps_groups ps_fun_defs []
	= (reversed_ps_groups, ps_unexpanded_dcl_macros, fun_defs, ps_macro_defs, ps_var_heap, ps_symbol_heap, ps_symbol_table, ps_error)

restore_unexpanded_dcl_macros :: !UnexpandedDclMacros !*{#*{#FunDef}} -> *{#*{#FunDef}}
restore_unexpanded_dcl_macros [(macro_module_index,macro_index,macro_def):unexpanded_dcl_macros] macro_defs
	# macro_defs & [macro_module_index,macro_index] = macro_def
	= restore_unexpanded_dcl_macros unexpanded_dcl_macros macro_defs
restore_unexpanded_dcl_macros [] macro_defs
	= macro_defs

partitionate_function :: Int Int !Int PartitioningInfo !*PartitioningState -> (!Int,!*PartitioningState)
partitionate_function mod_index max_fun_nr fun_index pi ps
	# (fun_def, ps) = ps!ps_fun_defs.[fun_index]
	= case fun_def.fun_body of
		CheckedBody body
			# fun_number = ps.ps_next_num
			# (min_dep, ps) = visit_functions mod_index max_fun_nr fun_def.fun_info.fi_calls pi
					(max_fun_nr,
						{ ps & ps_fun_defs={ ps.ps_fun_defs & [fun_index] = { fun_def & fun_body = PartitioningFunction body fun_number }},
							   ps_next_num = inc fun_number, ps_deps = [FunctionOrIclMacroIndex fun_index : ps.ps_deps] })
			-> try_to_close_group max_fun_nr (-1) fun_index fun_number min_dep pi ps
		PartitioningFunction _ fun_number
			-> (fun_number, ps)
		TransformedBody _
			| fun_def.fun_info.fi_group_index == NoIndex
				# ps =  add_called_macros fun_def.fun_info.fi_calls ps
				-> (max_fun_nr,
//					-> (max_fun_nr, ({ fun_defs & [fun_index] = {fun_def & fun_info.fi_group_index = -2-ps.ps_next_group }},
						{ps & ps_fun_defs.[fun_index] = {fun_def & fun_info.fi_group_index = ps.ps_next_group },
							  ps_next_group = inc ps.ps_next_group, ps_groups = [ [FunctionOrIclMacroIndex fun_index] : ps.ps_groups]}
//							{ps & ps_next_group = ps.ps_next_group}
						)
				-> (max_fun_nr, ps)
		GeneratedBody
			// do not allocate a group, it will be allocated during generic phase
			-> (max_fun_nr, ps)

partitionate_macro :: Int Int !Int !Int PartitioningInfo !*PartitioningState -> (!Int,!*PartitioningState)
partitionate_macro mod_index max_fun_nr macro_module_index macro_index pi ps
	# (fun_def, ps) = ps!ps_macro_defs.[macro_module_index,macro_index]
	= case fun_def.fun_body of
		CheckedBody body
			# fun_number = ps.ps_next_num			
			# ps={ps & ps_unexpanded_dcl_macros=[(macro_module_index,macro_index,fun_def):ps.ps_unexpanded_dcl_macros]}
			# (min_dep, ps) = visit_functions mod_index max_fun_nr fun_def.fun_info.fi_calls pi
					(max_fun_nr,
						{ ps & ps_macro_defs.[macro_module_index,macro_index] = { fun_def & fun_body = PartitioningFunction body fun_number },
							   ps_next_num = inc fun_number, ps_deps = [DclMacroIndex macro_module_index macro_index : ps.ps_deps] })
			-> try_to_close_group max_fun_nr macro_module_index macro_index fun_number min_dep pi ps
		PartitioningFunction _ fun_number
			-> (fun_number, ps)
		TransformedBody _
			| fun_def.fun_info.fi_group_index == NoIndex
				# ps =  add_called_macros fun_def.fun_info.fi_calls ps
				-> (max_fun_nr,
						{ps & ps_macro_defs.[macro_module_index,macro_index] = {fun_def & fun_info.fi_group_index = ps.ps_next_group },
							  ps_next_group = inc ps.ps_next_group, ps_groups = [ [DclMacroIndex macro_module_index macro_index] : ps.ps_groups]}
						)
				-> (max_fun_nr, ps)

visit_functions :: Int Int ![FunCall] PartitioningInfo !*(Int,*PartitioningState) -> *(Int,*PartitioningState)
visit_functions mod_index max_fun_nr calls pi min_dep_ps
	= foldSt (visit_function mod_index max_fun_nr) calls min_dep_ps
where
	visit_function mod_index max_fun_nr (FunCall fc_index _) (min_dep, ps)
		# (next_min, ps) = partitionate_function mod_index max_fun_nr fc_index pi ps
		= (min next_min min_dep, ps)
	visit_function mod_index max_fun_nr (MacroCall macro_module_index fc_index _) (min_dep, ps)
		# (next_min, ps) = partitionate_macro mod_index max_fun_nr macro_module_index fc_index pi ps
		= (min next_min min_dep, ps)
	visit_function mod_index max_fun_nr (DclFunCall dcl_fun_module_index dcl_fun_index) (min_dep, ps)
		| mod_index==dcl_fun_module_index
			# (next_min, ps) = partitionate_function mod_index max_fun_nr dcl_fun_index pi ps
			= (min next_min min_dep, ps)
			= (min_dep, ps)

try_to_close_group :: Int Int Int Int Int PartitioningInfo !*PartitioningState -> (!Int,!*PartitioningState)
try_to_close_group max_fun_nr macro_module_index fun_index fun_number min_dep pi
		ps=:{ps_symbol_table, ps_var_heap, ps_symbol_heap, ps_fun_defs,ps_macro_defs,ps_deps, ps_groups, ps_next_group, ps_error,ps_unexpanded_dcl_macros}
	| fun_number <= min_dep
		# (ps_deps, functions_in_group, macros_in_group, fun_defs,ps_macro_defs)
				= close_group macro_module_index fun_index ps_deps [] [] max_fun_nr ps_next_group ps_fun_defs ps_macro_defs
		  {ls_x={x_fun_defs=fun_defs,x_macro_defs}, ls_var_heap=ps_var_heap, ls_expr_heap=ps_symbol_heap}
		  		= liftFunctions (functions_in_group ++ macros_in_group) ps_next_group pi.pi_main_dcl_module_n fun_defs ps_macro_defs ps_var_heap ps_symbol_heap
		# es = expand_macros_in_group macros_in_group
		  			{	es_symbol_table = ps_symbol_table, es_var_heap = ps_var_heap, es_expression_heap = ps_symbol_heap,
		  				es_fun_defs=fun_defs, es_macro_defs=x_macro_defs, es_new_fun_def_numbers=[],
			  			es_error = ps_error }
		# {es_symbol_table, es_var_heap, es_expression_heap, es_error,es_fun_defs,es_macro_defs,es_new_fun_def_numbers}
		  		= expand_macros_in_group functions_in_group es
		# (n_fun_defs_after_expanding_macros,es_fun_defs) = usize es_fun_defs
		# (ps_next_group,es_fun_defs,functions_in_group,ps_groups)
			= add_new_macros_to_groups (reverse es_new_fun_def_numbers) n_fun_defs_after_expanding_macros ps_next_group es_fun_defs functions_in_group ps_groups
		= (max_fun_nr, { ps & ps_deps = ps_deps, ps_var_heap = es_var_heap,
					ps_symbol_table = es_symbol_table, ps_fun_defs=es_fun_defs, ps_macro_defs=es_macro_defs,
					ps_error = es_error, ps_symbol_heap = es_expression_heap, 
					ps_next_group = inc ps_next_group, 
					ps_groups = [ functions_in_group ++ macros_in_group : ps_groups ],ps_unexpanded_dcl_macros=ps_unexpanded_dcl_macros })
		= (min_dep, ps)
where
	close_group macro_module_index fun_index [index=:FunctionOrIclMacroIndex d:ds] functions_in_group macros_in_group nr_of_fun_defs group_number fun_defs macro_defs
		# (fun_def, fun_defs) = fun_defs![d]
		| case fun_def.fun_kind of FK_Macro->True; _ -> False
			# fun_defs = { fun_defs & [d] = { fun_def & fun_info.fi_group_index = -2-group_number }}
//			# fun_defs = { fun_defs & [d] = { fun_def & fun_info.fi_group_index = group_number }}
			# macros_in_group = [index : macros_in_group]
			| d == fun_index && macro_module_index==(-1)
				= (ds, functions_in_group, macros_in_group, fun_defs,macro_defs)
				= close_group macro_module_index fun_index ds functions_in_group macros_in_group nr_of_fun_defs group_number fun_defs macro_defs
			# fun_defs = { fun_defs & [d] = { fun_def & fun_info.fi_group_index = group_number }}
			# functions_in_group = [index : functions_in_group]
			| d == fun_index && macro_module_index==(-1)
				= (ds, functions_in_group, macros_in_group, fun_defs,macro_defs)
				= close_group macro_module_index fun_index ds functions_in_group macros_in_group nr_of_fun_defs group_number fun_defs macro_defs
	close_group macro_module_index fun_index [index=:DclMacroIndex module_index d:ds] functions_in_group macros_in_group nr_of_fun_defs group_number fun_defs macro_defs
		# (fun_def, macro_defs) = macro_defs![module_index,d]
		| case fun_def.fun_kind of FK_Macro->True; _ -> False
			# macro_defs = { macro_defs & [module_index,d] = { fun_def & fun_info.fi_group_index = -2-group_number }}
			# macros_in_group = [index : macros_in_group]
			| d == fun_index && macro_module_index==module_index
				= (ds, functions_in_group, macros_in_group, fun_defs,macro_defs)
				= close_group macro_module_index fun_index ds functions_in_group macros_in_group nr_of_fun_defs group_number fun_defs macro_defs
			# macro_defs = { macro_defs & [module_index,d] = { fun_def & fun_info.fi_group_index = group_number }}
			# functions_in_group = [index : functions_in_group]
			| d == fun_index && macro_module_index==module_index
				= (ds, functions_in_group, macros_in_group, fun_defs,macro_defs)
				= close_group macro_module_index fun_index ds functions_in_group macros_in_group nr_of_fun_defs group_number fun_defs macro_defs

	expand_macros_in_group group es
		= foldSt expand_macros group es
	where
		expand_macros (FunctionOrIclMacroIndex fun_index) es
			# (fun_def,es) = es!es_fun_defs.[fun_index]
			  {fun_ident,fun_body = PartitioningFunction body _, fun_info, fun_pos,fun_kind} = fun_def
			  identPos = newPosition fun_ident fun_pos
			# es={ es & es_error = setErrorAdmin identPos es.es_error }
			# (tb_args, tb_rhs, fi_local_vars, fi_calls,fi_dynamics, es)
					= expandMacrosInBody fun_info.fi_calls body fun_info.fi_dynamics pi.pi_predef_symbols_for_transform pi.pi_reset_body_of_rhs_macros es
			  fun_def = { fun_def & fun_body = TransformedBody { tb_args = tb_args, tb_rhs = tb_rhs},
			  			fun_info = { fun_info & fi_calls = fi_calls, fi_local_vars = fi_local_vars,fi_dynamics=fi_dynamics }}
			= {es & es_fun_defs.[fun_index] = fun_def }
		expand_macros (DclMacroIndex macro_module_index fun_index) es
			# (old_fun_def,es) = es!es_macro_defs.[macro_module_index,fun_index]
			  {fun_ident,fun_body = PartitioningFunction body _, fun_info, fun_pos,fun_kind} = old_fun_def
		  	  identPos = newPosition fun_ident fun_pos
			#  es={ es & es_error = setErrorAdmin identPos es.es_error }
			# (tb_args, tb_rhs, fi_local_vars, fi_calls,fi_dynamics, es)
					= expandMacrosInBody fun_info.fi_calls body fun_info.fi_dynamics pi.pi_predef_symbols_for_transform pi.pi_reset_body_of_rhs_macros es
			  fun_def = { old_fun_def & fun_body = TransformedBody { tb_args = tb_args, tb_rhs = tb_rhs},
			  			fun_info = { fun_info & fi_calls = fi_calls, fi_local_vars = fi_local_vars,fi_dynamics=fi_dynamics }}
			= {es & es_macro_defs.[macro_module_index,fun_index] = fun_def }

add_called_macros :: ![FunCall] !*PartitioningState -> *PartitioningState
add_called_macros calls ps
	= foldSt add_called_macro calls ps
where
	add_called_macro (FunCall fc_index _) ps
//		# fc_index = trace_n ("add_called_macro: "+++toString fc_index+++" ") fc_index
		# (macro_def, ps) = ps!ps_fun_defs.[fc_index]
		= case macro_def.fun_body of
			TransformedBody _
				| macro_def.fun_info.fi_group_index == NoIndex
					# ps = add_called_macros macro_def.fun_info.fi_calls ps
//						->	({ macro_defs & [fc_index] = {macro_def & fun_info.fi_group_index = ps.ps_next_group }},
//						# fc_index = trace ("add_called_macro2: "+++toString fc_index+++" ") fc_index
//						->	({ macro_defs & [fc_index] = {macro_def & fun_info.fi_group_index = -2-ps.ps_next_group }},
					->	{ps & ps_fun_defs.[fc_index] = {macro_def & fun_info.fi_group_index = ps.ps_next_group },
								ps_next_group = inc ps.ps_next_group, ps_groups = [ [FunctionOrIclMacroIndex fc_index] : ps.ps_groups]}
//								{ps & ps_next_group = ps.ps_next_group}
					-> ps

remove_macros_from_groups_and_reverse :: ![[FunctionOrMacroIndex]] !*{#FunDef} [[Int]] -> (![[Int]],!*{#FunDef})
remove_macros_from_groups_and_reverse [group:groups] fun_defs result_groups
	# (group,fun_defs) = remove_macros_from_group group fun_defs
	= case group of
		[]	-> remove_macros_from_groups_and_reverse groups fun_defs result_groups
		_	-> remove_macros_from_groups_and_reverse groups fun_defs [group:result_groups]
where
	remove_macros_from_group [FunctionOrIclMacroIndex fun:funs] fun_defs
		# (funs,fun_defs)=remove_macros_from_group funs fun_defs
		| fun_defs.[fun].fun_info.fi_group_index<NoIndex
			= (funs,fun_defs)
			= ([fun:funs],fun_defs)
	remove_macros_from_group [DclMacroIndex macro_module_index macro_index:funs] fun_defs		
		= remove_macros_from_group funs fun_defs
	remove_macros_from_group [] fun_defs
		= ([],fun_defs);
remove_macros_from_groups_and_reverse [] fun_defs result_groups
	= (result_groups,fun_defs);

addFunctionCallsToSymbolTable calls fun_defs macro_defs symbol_table
	= foldSt add_function_call_to_symbol_table calls ([], fun_defs,macro_defs, symbol_table)
where
	add_function_call_to_symbol_table fc=:(FunCall fc_index _) (collected_calls, fun_defs,macro_defs, symbol_table)
		# ({fun_ident = { id_info }, fun_kind}, fun_defs) = fun_defs![fc_index]
		= case fun_kind of
			FK_Macro
				-> (collected_calls, fun_defs,macro_defs,symbol_table)
			_
				# (entry, symbol_table) = readPtr id_info symbol_table
				-> ([fc : collected_calls], fun_defs,macro_defs,
					symbol_table <:= (id_info, { ste_kind = STE_Called [FunctionOrIclMacroIndex fc_index], ste_index = NoIndex, ste_def_level = NotALevel, ste_previous = entry, ste_doc = No }))
	add_function_call_to_symbol_table (MacroCall _ _ _) (collected_calls, fun_defs,macro_defs, symbol_table)
		= (collected_calls, fun_defs,macro_defs,symbol_table)
	add_function_call_to_symbol_table (DclFunCall _ _) (collected_calls, fun_defs,macro_defs, symbol_table)
		= (collected_calls, fun_defs,macro_defs,symbol_table)

removeFunctionCallsFromSymbolTable calls fun_defs symbol_table
	= foldSt remove_function_call_from_symbol_table calls (fun_defs, symbol_table)
where
	remove_function_call_from_symbol_table (FunCall fc_index _) (fun_defs, symbol_table)
		# ({fun_ident = { id_info }}, fun_defs) = fun_defs![fc_index]
		  (entry, symbol_table) = readPtr id_info symbol_table
		= case entry.ste_kind of
			STE_Called indexes
				-> (fun_defs, symbol_table <:= (id_info, entry.ste_previous))
			_
				-> (fun_defs, symbol_table)

expandMacrosInBody :: ![.FunCall] !CheckedBody ![ExprInfoPtr] !PredefSymbolsForTransform !Bool !*ExpandState
	-> (![FreeVar],!Expression,![FreeVar],![FunCall],![ExprInfoPtr],!*ExpandState)
expandMacrosInBody fi_calls {cb_args,cb_rhs} fi_dynamics predef_symbols_for_transform reset_body_of_rhs_macros
		es=:{es_symbol_table,es_expression_heap,es_fun_defs,es_macro_defs,es_var_heap}
	# (prev_calls, fun_defs, macro_defs,es_symbol_table)
			= addFunctionCallsToSymbolTable fi_calls es_fun_defs es_macro_defs es_symbol_table
	  es_var_heap = if reset_body_of_rhs_macros
					(reset_free_var_heap_pointers cb_rhs (reset_free_var_heap_pointers cb_args es_var_heap))
					es_var_heap
	  es & es_fun_defs=fun_defs, es_macro_defs=macro_defs, es_symbol_table=es_symbol_table, es_expression_heap=es_expression_heap, es_var_heap=es_var_heap

	  ([rhs:rhss], (all_calls, es) )
	  		= mapSt expandCheckedAlternative cb_rhs (prev_calls, es)
	  (fun_defs, symbol_table)
	  		= removeFunctionCallsFromSymbolTable all_calls es.es_fun_defs es.es_symbol_table

	  ((merged_rhs, _), es_var_heap, es_expression_heap, es_error)
	  		= mergeCases rhs rhss es.es_var_heap es.es_expression_heap es.es_error

	  (new_rhs, new_args, local_vars, fi_dynamics, {cos_error, cos_var_heap, cos_expression_heap})
	  		= determineVariablesAndRefCounts cb_args merged_rhs
	  				{ cos_error = es_error, cos_var_heap = es_var_heap, cos_expression_heap = es_expression_heap,
	  					cos_predef_symbols_for_transform = predef_symbols_for_transform }
	= (new_args, new_rhs, local_vars, all_calls, fi_dynamics,
		{ es & es_error = cos_error, es_var_heap = cos_var_heap, es_expression_heap = cos_expression_heap, es_fun_defs=fun_defs, es_symbol_table = symbol_table })

expandCheckedAlternative {ca_rhs, ca_position} ei
	# (ca_rhs, ei) = expand ca_rhs ei
	= ((ca_rhs, ca_position), ei)

::	ExpandInfo :== (![FunCall], !.ExpandState)

add_new_fun_defs new_functions new_function_index last_function_index es=:{es_fun_defs,es_new_fun_def_numbers}
	# new_fun_defs = new_fun_defs
		with
			new_fun_defs :: *{!FunDef}
			new_fun_defs => {fun_def \\ (_,fun_def)<-new_functions}
	# es_fun_defs = {if (i<new_function_index) es_fun_defs.[i] new_fun_defs.[i-new_function_index] \\ i<-[0..last_function_index]} // inefficient
	= {es & es_fun_defs=es_fun_defs,es_new_fun_def_numbers=[new_function_index:es_new_fun_def_numbers]}

class expand a :: !a !*ExpandInfo -> (!a, !*ExpandInfo)

instance expand Expression
where
	expand (App app=:{app_symb = symb=:{symb_kind = SK_DclMacro {glob_object,glob_module}}, app_args}) ei
		# (app_args, (calls, es)) = expand app_args ei
		# (macro, es) = es!es_macro_defs.[glob_module,glob_object]
		#! macro_group_index=macro.fun_info.fi_group_index
		# es = {es & es_macro_defs.[glob_module,glob_object].fun_info.fi_group_index= if (macro_group_index>NoIndex) (-2-macro_group_index) macro_group_index}
		| macro.fun_arity == length app_args
			= unfoldMacro macro app_args (calls, es)

			# macro = {macro & fun_info.fi_group_index=if (macro_group_index<NoIndex) (-2-macro_group_index) macro_group_index}
			#! new_function_index = size es.es_fun_defs
			# {es_fun_defs,es_macro_defs,es_var_heap,es_expression_heap} = es
			  (macro,new_functions,next_local_function_n,es_fun_defs,es_macro_defs,es_var_heap,es_expression_heap)
				= copy_macro_and_local_functions macro new_function_index es_fun_defs es_macro_defs es_var_heap es_expression_heap			
			  es & es_fun_defs=es_fun_defs, es_macro_defs=es_macro_defs, es_var_heap=es_var_heap, es_expression_heap=es_expression_heap
			  last_function_index = next_local_function_n-1

			# es = add_new_fun_defs [({old_function_n=DclMacroIndex glob_module glob_object,new_function_n=new_function_index},macro):new_functions] new_function_index last_function_index es
			# (calls, es_symbol_table) = examineFunctionCall macro.fun_ident (FunCall new_function_index NotALevel) (calls, es.es_symbol_table)
			# app = App { app & app_symb = { symb & symb_kind = SK_LocalMacroFunction new_function_index }, app_args = app_args }

/*			| macro.fun_info.fi_group_index>NoIndex
				# macro = {macro & fun_info.fi_group_index= -2-macro.fun_info.fi_group_index}
				# es= {es & es_fun_defs.[new_function_index]=macro}
				= (app, (calls, { es & es_symbol_table = es_symbol_table }))
*/
			= (app, (calls, { es & es_symbol_table = es_symbol_table }))

/*
			# (calls, es_symbol_table) = examineFunctionCall macro.fun_ident {fc_index = glob_object, fc_level = NotALevel} (calls, es.es_symbol_table)
			# app = App { app & app_symb = { symb & symb_kind = SK_Function {glob_object = glob_object, glob_module = glob_module} }, app_args = app_args }
			| macro.fun_info.fi_group_index<NoIndex
				# macro = {macro & fun_info.fi_group_index= -2-macro.fun_info.fi_group_index}
				# es= {es & es_fun_defs.[glob_object]=macro}
				= (app, (calls, { es & es_symbol_table = es_symbol_table }))
				= (app, (calls, { es & es_symbol_table = es_symbol_table }))
*/
	expand (App app=:{app_symb = symb=:{symb_kind = SK_IclMacro glob_object}, app_args}) ei
		# (app_args, (calls, es)) = expand app_args ei
		# (macro, es) = es!es_fun_defs.[glob_object]
		#! macro_group_index=macro.fun_info.fi_group_index
		# es = {es & es_fun_defs.[glob_object].fun_info.fi_group_index= if (macro_group_index>NoIndex) (-2-macro_group_index) macro_group_index}
		| macro.fun_arity == length app_args
			= unfoldMacro macro app_args (calls, es)

			# macro = {macro & fun_info.fi_group_index=if (macro_group_index<NoIndex) (-2-macro_group_index) macro_group_index}
			#! new_function_index = size es.es_fun_defs
			# {es_fun_defs,es_macro_defs,es_var_heap,es_expression_heap} = es
			  (macro,new_functions,next_local_function_n,es_fun_defs,es_macro_defs,es_var_heap,es_expression_heap)
				= copy_macro_and_local_functions macro new_function_index es_fun_defs es_macro_defs es_var_heap es_expression_heap			
			  es & es_fun_defs=es_fun_defs, es_macro_defs=es_macro_defs, es_var_heap=es_var_heap, es_expression_heap=es_expression_heap
			  last_function_index = next_local_function_n-1

			# es = add_new_fun_defs [({old_function_n=FunctionOrIclMacroIndex glob_object,new_function_n=new_function_index},macro):new_functions] new_function_index last_function_index es
			# (calls, es_symbol_table) = examineFunctionCall macro.fun_ident (FunCall new_function_index NotALevel) (calls, es.es_symbol_table)
			# app = App { app & app_symb = { symb & symb_kind = SK_LocalMacroFunction new_function_index }, app_args = app_args }
			= (app, (calls, { es & es_symbol_table = es_symbol_table }))

	expand (App app=:{app_args}) ei
		# (app_args, ei) = expand app_args ei
		= (App { app & app_args = app_args }, ei)
	expand (expr @ exprs) ei
		# ((expr,exprs), ei) = expand (expr,exprs) ei
		= (expr @ exprs, ei)
	expand (Let lad=:{let_strict_binds, let_lazy_binds, let_expr}) ei
		# (let_strict_binds, ei) = expand let_strict_binds ei
		# (let_lazy_binds, ei) = expand let_lazy_binds ei
		# (let_expr, ei) = expand let_expr ei
		= (Let {lad & let_expr = let_expr, let_strict_binds = let_strict_binds, let_lazy_binds = let_lazy_binds}, ei)
	expand (Case case_expr) ei
		# (case_expr, ei) = expand case_expr ei
		= (Case case_expr, ei)
	expand (Selection is_unique expr selectors) ei
		# ((expr, selectors), ei) = expand (expr, selectors) ei
		= (Selection is_unique expr selectors, ei)
	expand (Update expr1 selectors expr2) ei
		# (((expr1, expr2), selectors), ei) = expand ((expr1, expr2), selectors) ei
		= (Update expr1 selectors expr2, ei)
	expand (RecordUpdate cons_symbol expression expressions) ei
		# ((expression, expressions), ei) = expand (expression, expressions) ei
		= (RecordUpdate cons_symbol expression expressions, ei)
	expand (TupleSelect symbol argn_nr expr) ei
		# (expr, ei) = expand expr ei
		= (TupleSelect symbol argn_nr expr, ei)
	expand (MatchExpr cons_ident expr) ei
		# (expr, ei) = expand expr ei
		= (MatchExpr cons_ident expr, ei)
	expand (IsConstructor expr cons_symbol cons_arity global_type_index case_ident position) ei
		# (expr, ei) = expand expr ei
		= (IsConstructor expr cons_symbol cons_arity global_type_index case_ident position, ei)
	expand (DynamicExpr dyn) ei
		# (dyn, ei) = expand dyn ei
		= (DynamicExpr dyn, ei)
	expand (TypeSignature type_function expr) ei
		# (expr, ei) = expand expr ei
		= (TypeSignature type_function expr, ei)
	expand expr ei
		= (expr, ei)

instance expand Selection
where
	expand (ArraySelection array_select expr_ptr index_expr) ei
		# (index_expr, ei) = expand index_expr ei
		= (ArraySelection array_select expr_ptr index_expr, ei)
	expand record_selection ei
		= (record_selection, ei)

instance expand LetBind
where
	expand bind=:{lb_src} ei
		# (lb_src, ei) = expand lb_src ei
		= ({ bind & lb_src = lb_src }, ei)

instance expand (Bind a b) | expand a
where
	expand bind=:{bind_src} ei
		# (bind_src, ei) = expand bind_src ei
		= ({ bind & bind_src = bind_src }, ei)

instance expand Case
where
 	expand kees (fundefs, es=:{es_var_heap, es_expression_heap, es_error})
		# (kees=:{case_expr,case_guards,case_default}, es_var_heap, es_expression_heap, es_error)
			= merge_if_explicit_case kees es_var_heap es_expression_heap es_error
		# ei = (fundefs, {es & es_var_heap=es_var_heap, es_expression_heap=es_expression_heap, es_error=es_error})
		# ((case_expr,(case_guards,case_default)), ei) = expand (case_expr,(case_guards,case_default)) ei
		= ({ kees & case_expr = case_expr,case_guards = case_guards, case_default = case_default }, ei)
		where
			merge_if_explicit_case kees=:{ case_explicit } var_heap expr_heap error_admin
				| case_explicit
					# cases	= map (make_case kees.case_expr) (split_patterns kees.case_guards)
					  cases = init cases ++ [{last cases & case_default = kees.case_default}]
					  [firstCase : otherCases] = [(Case kees, NoPos) \\ kees <- cases]
					  ((Case {case_guards},_), var_heap, expr_heap, error_admin)
					  		=  mergeCases firstCase otherCases var_heap expr_heap error_admin
					  kees = {kees & case_guards = case_guards}
					=	(kees, var_heap, expr_heap, error_admin)
					with
						split_patterns :: CasePatterns -> [CasePatterns]
						split_patterns (AlgebraicPatterns index patterns)
							=	[AlgebraicPatterns index [pattern] \\ pattern <- patterns]
						split_patterns (BasicPatterns basicType patterns)
							=	[BasicPatterns basicType [pattern] \\ pattern <- patterns]
						split_patterns (OverloadedListPatterns  overloaded_list_type decons_expr patterns)
							=	[OverloadedListPatterns overloaded_list_type decons_expr [pattern] \\ pattern <- patterns]
						split_patterns (NewTypePatterns index patterns)
							=	[NewTypePatterns index [pattern] \\ pattern <- patterns]
						split_patterns (DynamicPatterns patterns)
							=	[DynamicPatterns [pattern] \\ pattern <- patterns]
						split_patterns NoPattern
							=	[NoPattern]

						make_case :: Expression CasePatterns -> Case
						make_case expr guard
							=
							{	case_expr		= expr
							,	case_guards		= guard
							,	case_default	= No
							,	case_ident		= No
							,	case_info_ptr	= nilPtr
							,	case_default_pos= NoPos
							,	case_explicit	= False
							}
				// otherwise // not case_explicit
					=	(kees, var_heap, expr_heap, error_admin)

instance expand CasePatterns
where
	expand (AlgebraicPatterns type patterns) ei
		# (patterns, ei) = expand patterns ei
		= (AlgebraicPatterns type patterns, ei) 
	expand (BasicPatterns type patterns) ei
		# (patterns, ei) = expand patterns ei
		= (BasicPatterns type patterns, ei) 
	expand (OverloadedListPatterns type decons_expr patterns) ei
		# (patterns, ei) = expand patterns ei
		= (OverloadedListPatterns type decons_expr patterns, ei) 
	expand (NewTypePatterns type patterns) ei
		# (patterns, ei) = expand patterns ei
		= (NewTypePatterns type patterns, ei) 
	expand (DynamicPatterns patterns) ei
		# (patterns, ei) = expand patterns ei
		= (DynamicPatterns patterns, ei) 

instance expand AlgebraicPattern
where
	expand alg_pattern=:{ap_expr} ei
		# (ap_expr, ei) = expand ap_expr ei
		= ({ alg_pattern & ap_expr = ap_expr }, ei)

instance expand BasicPattern
where
	expand bas_pattern=:{bp_expr} ei
		# (bp_expr, ei) = expand bp_expr ei
		= ({ bas_pattern & bp_expr = bp_expr }, ei)

instance expand DynamicPattern
where
	expand dyn_pattern=:{dp_rhs} ei
		# (dp_rhs, ei) = expand dp_rhs ei
		= ({ dyn_pattern & dp_rhs = dp_rhs }, ei)

instance expand DynamicExpr
where
	expand (dyn=:{dyn_expr}) ei
		# (dyn_expr, ei) = expand dyn_expr ei
		= ({dyn & dyn_expr = dyn_expr}, ei)

instance expand [a] | expand a
where
	expand [x:xs] ei
		# (x, ei) = expand x ei
		  (xs, ei) = expand xs ei
		= ([x:xs], ei)
	expand [] ei
		= ([], ei)

instance expand (a,b) | expand a & expand b
where
	expand (x,y) ei
		# (x, ei) = expand x ei
		  (y, ei) = expand y ei
		= ((x,y), ei)

instance expand (Optional a) | expand a
where
	expand (Yes x) ei
		# (x, ei) = expand x ei
		= (Yes x, ei)
	expand no ei
		= (no, ei)

::	CollectState =
	{	cos_var_heap		:: !.VarHeap
	,	cos_expression_heap :: !.ExpressionHeap
	,	cos_error			:: !.ErrorAdmin
	,	cos_predef_symbols_for_transform :: !PredefSymbolsForTransform
	}

determineVariablesAndRefCounts :: ![FreeVar] !Expression !*CollectState -> (!Expression , ![FreeVar], ![FreeVar], ![DynamicPtr], !*CollectState)
determineVariablesAndRefCounts free_vars expr cos=:{cos_var_heap}
	# cos = {cos & cos_var_heap = clearCount free_vars cIsAGlobalVar cos_var_heap}
	  (expr, local_vars, dynamics, cos) = collectVariables expr [] [] cos
	  (free_vars, cos_var_heap) = retrieveRefCounts free_vars cos.cos_var_heap
	  (local_vars, cos_var_heap) = retrieveRefCounts local_vars cos_var_heap
	= (expr, free_vars, local_vars, dynamics, { cos & cos_var_heap = cos_var_heap })

retrieveRefCounts free_vars var_heap
	= mapSt retrieveRefCount free_vars var_heap

retrieveRefCount :: FreeVar *VarHeap -> (!FreeVar,!.VarHeap)
retrieveRefCount fv=:{fv_info_ptr} var_heap
	# (info, var_heap) = readPtr fv_info_ptr var_heap
	= case info of
		VI_Count count _
			-> ({ fv & fv_count = count }, var_heap)
		VI_RefFromTupleSel0 count
			-> ({ fv & fv_count = count }, var_heap)		
		VI_RefFromArrayUpdate count _
			-> ({ fv & fv_count = count }, var_heap)
		VI_RefFromArrayUpdateOfTupleElem2 count _
			-> ({ fv & fv_count = count }, var_heap)
		VI_RefFromArrayUpdateToTupleSelector2 count _ _
			-> ({ fv & fv_count = count }, var_heap)

/*
	'clearCount' initialises the 'fv_info_ptr' field of each 'FreeVar'
*/

class clearCount a :: !a !Bool !*VarHeap -> *VarHeap

instance clearCount [a] | clearCount a
where
	clearCount [x:xs] locality var_heap
		= clearCount x locality (clearCount xs locality var_heap)
	clearCount [] locality var_heap
		= var_heap

instance clearCount LetBind
where
	clearCount bind=:{lb_dst} locality var_heap
		= clearCount lb_dst locality var_heap

instance clearCount FreeVar
where
	clearCount {fv_info_ptr} locality var_heap
		= var_heap <:= (fv_info_ptr, VI_Count 0 locality)

instance clearCount (FreeVar,a)
where
	clearCount ({fv_info_ptr},_) locality var_heap
		= var_heap <:= (fv_info_ptr, VI_Count 0 locality)

/*
	In 'collectVariables' all local variables are collected. Moreover the reference counts
	of the local as well as of the global variables are determined. Aliases and unreachable 
	bindings introduced in a 'let' are removed.
	Dynamic administration is rebuilt.
*/	

class collectVariables a :: !a ![FreeVar] ![DynamicPtr] !*CollectState -> (!a, ![FreeVar],![DynamicPtr],!*CollectState)

cContainsACycle		:== True
cContainsNoCycle	:== False

instance collectVariables Expression
where
	collectVariables (Var var) free_vars dynamics cos
		# (var, free_vars, dynamics, cos) = collectVariables var free_vars dynamics cos
		= (Var var, free_vars, dynamics, cos)
	/* optimize && and || */
	collectVariables (App app=:{app_symb={symb_kind=SK_Function {glob_object,glob_module}},app_args}) free_vars dynamics cos=:{cos_predef_symbols_for_transform={predef_and,predef_or}}
		# ([e1,e2:_], free_vars, dynamics, cos) = collectVariables app_args free_vars dynamics cos
		| glob_object==predef_and.pds_def && glob_module==predef_and.pds_module && two_args app_args
			# (kase,cos) = if_expression e1 e2 (BasicExpr (BVB False)) cos
			= (kase, free_vars, dynamics, cos)
		| glob_object==predef_or.pds_def && glob_module==predef_or.pds_module && two_args app_args
			# (kase,cos) = if_expression e1 (BasicExpr (BVB True)) e2 cos
			= (kase, free_vars, dynamics, cos)
		where
			if_expression :: !Expression !Expression !Expression !*CollectState -> (!Expression,!.CollectState);
			if_expression e1 e2 e3 cos
//				# (new_info_ptr,symbol_heap) = newPtr EI_Empty cos.cos_expression_heap
				# case_type =
					{	ct_pattern_type	= MakeAttributedType (TB BT_Bool)
					,	ct_result_type	= MakeAttributedType (TB BT_Bool)
					,	ct_cons_types 	= [[MakeAttributedType (TB BT_Bool)]]
					}
				# (new_info_ptr,symbol_heap) = newPtr (EI_CaseType case_type) cos.cos_expression_heap
				# kase = Case {	case_expr=e1, case_guards=BasicPatterns BT_Bool [{bp_value=BVB True,bp_expr=e2,bp_position=NoPos}],
								case_default=Yes e3, case_ident=No, case_info_ptr=new_info_ptr, case_default_pos = NoPos,
								case_explicit = False }
				= (kase,{cos & cos_expression_heap=symbol_heap});
			
			two_args [_,_]
				= True;
			two_args app_args
				= False;
	collectVariables (App app=:{app_args}) free_vars dynamics cos
		# (app_args, free_vars, dynamics, cos) = collectVariables app_args free_vars dynamics cos
		= (App { app & app_args = app_args}, free_vars, dynamics, cos)
	collectVariables (expr @ exprs) free_vars dynamics cos
		# ((expr, exprs), free_vars, dynamics, cos) = collectVariables (expr, exprs) free_vars dynamics cos
		= (expr @ exprs, free_vars, dynamics, cos)
	collectVariables (Let lad=:{let_strict_binds, let_lazy_binds, let_expr, let_info_ptr}) free_vars dynamics cos=:{cos_var_heap}
		# (let_info,cos_expression_heap)	= readPtr let_info_ptr cos.cos_expression_heap
		  let_types = case let_info of
		  				EI_LetType let_types	-> let_types
		  				_						-> repeat undef
		  cos = {cos & cos_expression_heap = cos_expression_heap}
		  cos_var_heap = cos.cos_var_heap

		# cos_var_heap = determine_aliases let_strict_binds cos_var_heap
		  cos_var_heap = determine_aliases let_lazy_binds cos_var_heap

		  (let_strict_binds, let_types)	= combine let_strict_binds let_types
			  	with
			  		combine [] let_types
			  			= ([],let_types)
			  		combine [lb:let_binds] [tp:let_types]
			  			# (let_binds,let_types)	= combine let_binds let_types
			  			= ([(tp, lb) : let_binds], let_types)
		  let_lazy_binds = zip2 let_types let_lazy_binds

		  (is_cyclic_s, let_strict_binds, cos) 
		  		= detect_cycles_and_handle_alias_binds True let_strict_binds
		  											{ cos & cos_var_heap = cos_var_heap }
		  (is_cyclic_l, let_lazy_binds, cos) 
		  		= detect_cycles_and_handle_alias_binds False let_lazy_binds cos
		| is_cyclic_s || is_cyclic_l
			# let_info = case let_info of
				EI_LetType _	-> EI_LetType (map fst (let_strict_binds ++ let_lazy_binds))
				_				-> let_info
			  let_strict_binds = map snd let_strict_binds
			  let_lazy_binds = map snd let_lazy_binds
			  cos_expression_heap = writePtr let_info_ptr let_info cos.cos_expression_heap
			  cos = {cos & cos_expression_heap = cos_expression_heap}
			= (Let {lad & let_strict_binds = let_strict_binds, let_lazy_binds = let_lazy_binds }, free_vars, dynamics,
					{ cos & cos_error = checkError "" "cyclic let definition" cos.cos_error})
//		| otherwise
			# (let_expr, free_vars, dynamics, cos) = collectVariables let_expr free_vars dynamics cos
			  all_binds = combine let_strict_binds let_lazy_binds
						  	with
						  		combine [] let_lazy_binds
						  			= [(False, tp, lb) \\ (tp,lb)<-let_lazy_binds]
						  		combine [(tp,lb):let_strict_binds] let_lazy_binds
						  			= [(True, tp, lb) : combine let_strict_binds let_lazy_binds]
			  (collected_binds, free_vars, dynamics, cos) = collect_variables_in_binds all_binds [] free_vars dynamics cos
			| isEmpty collected_binds
				= (let_expr, free_vars, dynamics, cos)
			 	# (let_strict_bind_types,let_lazy_bind_types,let_strict_binds,let_lazy_binds,cos_var_heap) = split_binds collected_binds cos.cos_var_heap
				  	with
		  				split_binds :: ![(Bool, AType, LetBind)] !*VarHeap -> (!*[AType],!*[AType],!*[LetBind],!*[LetBind],!*VarHeap)
						split_binds [] var_heap
							= ([],[],[],[],var_heap)
						split_binds [(strict, t, b=:{lb_dst={fv_info_ptr},lb_src=Selection UniqueSelector expr selections}) : xs] var_heap
							| unique_result_selection selections fv_info_ptr var_heap
								# (st,lt,sb,lb,var_heap) = split_binds xs var_heap
								# b = {b & lb_src = Selection UniqueSelectorUniqueElementResult expr selections}
								| strict
									= ([t:st],lt,[b:sb],lb,var_heap)
									= (st,[t:lt],sb,[b:lb],var_heap)
						split_binds [(strict, t, b=:{lb_dst={fv_info_ptr},lb_src=Selection UniqueSingleArraySelector expr selections}) : xs] var_heap
							| unique_result_selection selections fv_info_ptr var_heap
								# (st,lt,sb,lb,var_heap) = split_binds xs var_heap
								# b = {b & lb_src = Selection UniqueSingleArraySelectorUniqueElementResult expr selections}
								| strict
									= ([t:st],lt,[b:sb],lb,var_heap)
									= (st,[t:lt],sb,[b:lb],var_heap)
						split_binds [(strict, t, b):xs] var_heap
							# (st,lt,sb,lb,var_heap) = split_binds xs var_heap
							| strict
								= ([t:st],lt,[b:sb],lb,var_heap)
								= (st,[t:lt],sb,[b:lb],var_heap)

						unique_result_selection selections fv_info_ptr var_heap
							= case sreadPtr fv_info_ptr var_heap of
								VI_RefFromArrayUpdateOfTupleElem2 _ update_selections
									-> same_selections selections update_selections
								_
									-> False
				# cos = {cos & cos_var_heap=cos_var_heap}
				# let_info = case let_info of
					EI_LetType _	-> EI_LetType (let_strict_bind_types ++ let_lazy_bind_types)
					_				-> let_info
				  cos_expression_heap = writePtr let_info_ptr let_info cos.cos_expression_heap
				  cos = {cos & cos_expression_heap = cos_expression_heap}
				= (Let {lad & let_expr = let_expr, let_strict_binds = let_strict_binds, let_lazy_binds = let_lazy_binds}, free_vars, dynamics, cos)
		where
		/*	Set the 'var_info_field' of each  bound variable to either 'VI_Alias var' (if
			this variable is an alias for 'var') or to 'VI_Count 0 cIsALocalVar' to initialise
		   	the reference count info.
		*/

			determine_aliases [{lb_dst={fv_info_ptr}, lb_src = Var var} : binds] var_heap
				= determine_aliases binds (writePtr fv_info_ptr (VI_Alias var) var_heap)
			determine_aliases [bind : binds] var_heap
				= determine_aliases binds (clearCount bind cIsALocalVar var_heap)
			determine_aliases [] var_heap
				= var_heap

			
		/*	Remove all aliases from the list of lazy 'let'-binds. Add a _dummyForStrictAlias
			function call for the strict aliases. Be careful with cycles! */
		
			detect_cycles_and_handle_alias_binds :: !.Bool !u:[v:(.a,w:LetBind)] !*CollectState -> (!.Bool,!x:[y:(.a,z:LetBind)],!.CollectState), [u <= x,v <= y,w <= z]
			detect_cycles_and_handle_alias_binds is_strict [] cos
				= (cContainsNoCycle, [], cos)
//			detect_cycles_and_handle_alias_binds is_strict [bind=:{bind_dst={fv_info_ptr}} : binds] cos
			detect_cycles_and_handle_alias_binds is_strict [(type,bind=:{lb_dst={fv_info_ptr}}) : binds] cos
				# (var_info, cos_var_heap) = readPtr fv_info_ptr cos.cos_var_heap
				  cos = { cos & cos_var_heap = cos_var_heap }
				= case var_info of
					VI_Alias {var_info_ptr}
						| is_cyclic fv_info_ptr var_info_ptr cos.cos_var_heap
							-> (cContainsACycle, binds, cos)
						| is_strict
							# cos_var_heap = writePtr fv_info_ptr (VI_Count 0 cIsALocalVar) cos.cos_var_heap
							  (new_bind_src, cos) = add_dummy_id_for_strict_alias bind.lb_src 
							  								{ cos & cos_var_heap = cos_var_heap }
							  (is_cyclic, binds, cos) 
							  		= detect_cycles_and_handle_alias_binds is_strict binds cos
							-> (is_cyclic, [(type,{ bind & lb_src = new_bind_src }) : binds], cos)
						-> detect_cycles_and_handle_alias_binds is_strict binds cos
					_
						# (is_cyclic, binds, cos) = detect_cycles_and_handle_alias_binds is_strict binds cos
						-> (is_cyclic, [(type,bind) : binds], cos)
			where
				is_cyclic :: !.(Ptr VarInfo) !(Ptr VarInfo) !VarHeap -> .Bool
				is_cyclic orig_info_ptr info_ptr var_heap
					| orig_info_ptr == info_ptr
						= True
						#! var_info = sreadPtr info_ptr var_heap
						= case var_info of
							VI_Alias {var_info_ptr}
								-> is_cyclic orig_info_ptr var_info_ptr var_heap
							_
								-> False
				
				add_dummy_id_for_strict_alias :: !.Expression !*CollectState -> (!.Expression,!.CollectState)
				add_dummy_id_for_strict_alias bind_src cos=:{cos_expression_heap, cos_predef_symbols_for_transform}
					# (new_app_info_ptr, cos_expression_heap) = newPtr EI_Empty cos_expression_heap
					  {pds_module, pds_def} = cos_predef_symbols_for_transform.predef_alias_dummy
					  pds_ident = predefined_idents.[PD_DummyForStrictAliasFun]
			  		  app_symb = { symb_ident = pds_ident, symb_kind = SK_Function {glob_module = pds_module, glob_object = pds_def} }
					= (App { app_symb = app_symb, app_args = [bind_src], app_info_ptr = new_app_info_ptr },
						{ cos & cos_expression_heap = cos_expression_heap } )
								
		/*	Apply 'collectVariables' to the bound expressions (the 'bind_src' field of 'let'-bind) if
		    the corresponding bound variable (the 'bind_dst' field) has been used. This can be determined
		    by examining the reference count.
		*/

			collect_variables_in_binds :: ![(Bool,.b,.LetBind)] !u:[v:(Bool,.b,w:LetBind)] ![FreeVar] ![(Ptr ExprInfo)] !*CollectState -> (!x:[y:(Bool,.b,z:LetBind)],![FreeVar],![(Ptr ExprInfo)],!.CollectState), [u <= x,v <= y,w <= z]
			collect_variables_in_binds binds collected_binds free_vars dynamics cos
				# (continue, binds, collected_binds, free_vars, dynamics, cos) = examine_reachable_binds False binds collected_binds free_vars dynamics cos
				| continue
					= collect_variables_in_binds binds collected_binds free_vars dynamics cos
					# cos = {cos & cos_error=report_unused_strict_binds binds cos.cos_error}
					= (collected_binds, free_vars, dynamics, cos)

			examine_reachable_binds :: !Bool ![v:(.a,.b,w:LetBind)] !x:[y:(.a,.b,z:LetBind)] ![.FreeVar] ![.(Ptr ExprInfo)] !*CollectState -> *(!Bool,![v0:(.a,.b,w0:LetBind)],!x0:[y0:(.a,.b,z0:LetBind)],![FreeVar],![(Ptr ExprInfo)],!*CollectState), [v <= v0,w <= w0,x <= x0,y <= y0,z <= z0]
			examine_reachable_binds bind_found [bind=:(is_strict, type, letb=:{lb_dst=fv=:{fv_info_ptr},lb_src}) : binds] collected_binds free_vars dynamics cos
				# (bind_found, binds, collected_binds, free_vars, dynamics, cos) = examine_reachable_binds bind_found binds collected_binds free_vars dynamics cos
				# (info, cos_var_heap) = readPtr fv_info_ptr cos.cos_var_heap
				# cos = { cos & cos_var_heap = cos_var_heap }
				= case info of
					VI_Count count _
						| count > 0
							#  (lb_src, free_vars, dynamics, cos) = collectVariables lb_src free_vars dynamics cos
							-> (True, binds, [ (is_strict, type, { letb & lb_dst = { fv & fv_count = count }, lb_src = lb_src }) : collected_binds ], free_vars, dynamics, cos)
							-> (bind_found, [bind : binds], collected_binds, free_vars, dynamics, cos)
					VI_RefFromTupleSel0 count
						#  (lb_src, free_vars, dynamics, cos) = collectVariables lb_src free_vars dynamics cos
						-> (True, binds, [ (is_strict, type, { letb & lb_dst = { fv & fv_count = count }, lb_src = lb_src }) : collected_binds ], free_vars, dynamics, cos)
					VI_RefFromArrayUpdate count selectors
						-> case lb_src of
							TupleSelect tuple_symbol 1 (Var var)
								# (var, free_vars, dynamics, cos) = collectUpdateVarTupleSelect2Var var fv_info_ptr count selectors free_vars dynamics cos
								#  lb_src = TupleSelect tuple_symbol 1 (Var var)
								-> (True, binds, [ (is_strict, type, { letb & lb_dst = { fv & fv_count = count }, lb_src = lb_src }) : collected_binds ], free_vars, dynamics, cos)
							_
								#  (lb_src, free_vars, dynamics, cos) = collectVariables lb_src free_vars dynamics cos
								-> (True, binds, [ (is_strict, type, { letb & lb_dst = { fv & fv_count = count }, lb_src = lb_src }) : collected_binds ], free_vars, dynamics, cos)
					VI_RefFromArrayUpdateOfTupleElem2 count _
						#  (lb_src, free_vars, dynamics, cos) = collectVariables lb_src free_vars dynamics cos
						-> (True, binds, [ (is_strict, type, { letb & lb_dst = { fv & fv_count = count }, lb_src = lb_src }) : collected_binds ], free_vars, dynamics, cos)
					VI_RefFromArrayUpdateToTupleSelector2 count selectors array_var_info_ptr
						-> abort "examine_reachable_binds VI_RefFromArrayUpdateToTupleSelector2"

			examine_reachable_binds bind_found [] collected_binds free_vars dynamics cos
				= (bind_found, [], collected_binds, free_vars, dynamics, cos)

			report_unused_strict_binds [(is_strict,type,{lb_dst={fv_ident},lb_position}):binds] errors
				| not is_strict
					= report_unused_strict_binds binds errors
					= report_unused_strict_binds binds (checkWarningWithPosition fv_ident lb_position "not used, ! ignored" errors)
			report_unused_strict_binds [] errors
				= errors

	collectVariables (Case case_expr) free_vars dynamics cos
		# (case_expr, free_vars, dynamics, cos) = collectVariables case_expr free_vars dynamics cos
		= (Case case_expr, free_vars, dynamics, cos)
	collectVariables (Selection is_unique expr selectors) free_vars dynamics cos
		# ((expr, selectors), free_vars, dynamics, cos) = collectVariables (expr, selectors) free_vars dynamics cos
		= (Selection is_unique expr selectors, free_vars, dynamics, cos)
	collectVariables (Update (Var var) selectors expr2) free_vars dynamics cos
		# (var, free_vars, dynamics, cos) = collectUpdateVar var selectors free_vars dynamics cos
		# ((expr2, selectors), free_vars, dynamics, cos) = collectVariables (expr2, selectors) free_vars dynamics cos
		= (Update (Var var) selectors expr2, free_vars, dynamics, cos)
	collectVariables (Update (TupleSelect tuple_symbol 1 (Var var)) selectors expr2) free_vars dynamics cos
		# (var, free_vars, dynamics, cos) = collectUpdateTupleSelect2Var var selectors free_vars dynamics cos
		# ((expr2, selectors), free_vars, dynamics, cos) = collectVariables (expr2, selectors) free_vars dynamics cos
		= (Update (TupleSelect tuple_symbol 1 (Var var)) selectors expr2, free_vars, dynamics, cos)
	collectVariables (Update expr1 selectors expr2) free_vars dynamics cos
		# (((expr1, expr2), selectors), free_vars, dynamics, cos) = collectVariables ((expr1, expr2), selectors) free_vars dynamics cos
		= (Update expr1 selectors expr2, free_vars, dynamics, cos)
	collectVariables (RecordUpdate cons_symbol expression expressions) free_vars dynamics cos
		# ((expression, expressions), free_vars, dynamics, cos) = collectVariables (expression, expressions) free_vars dynamics cos
		= (RecordUpdate cons_symbol expression expressions, free_vars, dynamics, cos)
	collectVariables (TupleSelect symbol 0 (Var var)) free_vars dynamics cos
		# (var, free_vars, dynamics, cos) = collectTupleSelect0Var var free_vars dynamics cos
		= (TupleSelect symbol 0 (Var var), free_vars, dynamics, cos)
	collectVariables (TupleSelect symbol argn_nr expr) free_vars dynamics cos
		# (expr, free_vars, dynamics, cos) = collectVariables expr free_vars dynamics cos
		= (TupleSelect symbol argn_nr expr, free_vars, dynamics, cos)
	collectVariables (MatchExpr cons_ident expr) free_vars dynamics cos
		# (expr, free_vars, dynamics, cos) = collectVariables expr free_vars dynamics cos
		= (MatchExpr cons_ident expr, free_vars, dynamics, cos)
	collectVariables (IsConstructor expr cons_symbol cons_arity global_type_index case_ident position) free_vars dynamics cos
		# (expr, free_vars, dynamics, cos) = collectVariables expr free_vars dynamics cos
		= (IsConstructor expr cons_symbol cons_arity global_type_index case_ident position, free_vars, dynamics, cos)
	collectVariables (DynamicExpr dynamic_expr) free_vars dynamics cos
		# (dynamic_expr, free_vars, dynamics, cos) = collectVariables dynamic_expr free_vars dynamics cos
		= (DynamicExpr dynamic_expr, free_vars, dynamics, cos)
	collectVariables (TypeSignature type_function expr) free_vars dynamics cos
		# (expr, free_vars, dynamics, cos) = collectVariables expr free_vars dynamics cos
		= (TypeSignature type_function expr, free_vars, dynamics, cos);
	collectVariables (DictionariesFunction dictionaries expr expr_type) free_vars dynamics cos
		# cos = {cos & cos_var_heap = clearCount dictionaries cIsALocalVar cos.cos_var_heap}
		  (expr, free_vars, dynamics, cos) = collectVariables expr free_vars dynamics cos
		  (dictionaries, var_heap) = mapSt retrieve_ref_count dictionaries cos.cos_var_heap
		  cos = {cos & cos_var_heap = var_heap}
		= (DictionariesFunction dictionaries expr expr_type, free_vars, dynamics, cos)
	where
		retrieve_ref_count (fv,a_type) var_heap
			# (fv,var_heap) = retrieveRefCount fv var_heap
			= ((fv,a_type),var_heap)
	collectVariables expr free_vars dynamics cos
		= (expr, free_vars, dynamics, cos)

instance collectVariables Selection
where
	collectVariables (ArraySelection array_select expr_ptr index_expr) free_vars dynamics cos
		# (index_expr, free_vars, dynamics, cos) = collectVariables index_expr free_vars dynamics cos
		= (ArraySelection array_select expr_ptr index_expr, free_vars, dynamics, cos)
	collectVariables (DictionarySelection dictionary_select selectors expr_ptr index_expr) free_vars dynamics cos
		# ((index_expr,selectors), free_vars, dynamics, cos) = collectVariables (index_expr,selectors) free_vars dynamics cos
		= (DictionarySelection dictionary_select selectors expr_ptr index_expr, free_vars, dynamics, cos)
	collectVariables record_selection free_vars dynamics cos
		= (record_selection, free_vars, dynamics, cos)

instance collectVariables [a] | collectVariables a
where
	collectVariables [x:xs] free_vars dynamics cos
		# (x, free_vars, dynamics, cos) = collectVariables x free_vars dynamics cos
		# (xs, free_vars, dynamics, cos) = collectVariables xs free_vars dynamics cos
		= ([x:xs], free_vars, dynamics, cos)
	collectVariables [] free_vars dynamics cos
		= ([], free_vars, dynamics, cos)

instance collectVariables (!a,!b) | collectVariables a & collectVariables b
where
	collectVariables (x,y) free_vars dynamics cos
		# (x, free_vars, dynamics, cos) = collectVariables x free_vars dynamics cos
		# (y, free_vars, dynamics, cos) = collectVariables y free_vars dynamics cos
		= ((x,y), free_vars, dynamics, cos)

instance collectVariables (Optional a) | collectVariables a
where
	collectVariables (Yes x) free_vars dynamics cos
		# (x, free_vars, dynamics, cos) = collectVariables x free_vars dynamics cos
		= (Yes x, free_vars, dynamics, cos)
	collectVariables no free_vars dynamics cos
		= (no, free_vars, dynamics, cos)

instance collectVariables (Bind a b) | collectVariables a where
	collectVariables bind=:{bind_src} free_vars dynamics cos
		# (bind_src, free_vars, dynamics, cos) = collectVariables bind_src free_vars dynamics cos
		= ({bind & bind_src = bind_src}, free_vars, dynamics, cos)

instance collectVariables Case
where
	collectVariables kees=:{ case_expr, case_guards, case_default } free_vars dynamics cos
		# (case_expr, free_vars, dynamics, cos) = collectVariables case_expr free_vars dynamics cos
		# (case_guards, free_vars, dynamics, cos) = collectVariables case_guards free_vars dynamics cos
		# (case_default, free_vars, dynamics, cos) = collectVariables case_default free_vars dynamics cos
		=  ({ kees & case_expr = case_expr, case_guards = case_guards, case_default = case_default }, free_vars, dynamics, cos)

instance collectVariables CasePatterns
where
	collectVariables (AlgebraicPatterns type patterns) free_vars dynamics cos
		# (patterns, free_vars, dynamics, cos) = collectVariables patterns free_vars dynamics cos
		= (AlgebraicPatterns type patterns, free_vars, dynamics, cos)
	collectVariables (BasicPatterns type patterns) free_vars dynamics cos
		# (patterns, free_vars, dynamics, cos) = collectVariables patterns free_vars dynamics cos
		= (BasicPatterns type patterns, free_vars, dynamics, cos)
	collectVariables (OverloadedListPatterns type decons_expr patterns) free_vars dynamics cos
		# (patterns, free_vars, dynamics, cos) = collectVariables patterns free_vars dynamics cos
		= (OverloadedListPatterns type decons_expr patterns, free_vars, dynamics, cos)
	collectVariables (NewTypePatterns type patterns) free_vars dynamics cos
		# (patterns, free_vars, dynamics, cos) = collectVariables patterns free_vars dynamics cos
		= (NewTypePatterns type patterns, free_vars, dynamics, cos)
	collectVariables (DynamicPatterns patterns) free_vars dynamics cos
		# (patterns, free_vars, dynamics, cos) = collectVariables patterns free_vars dynamics cos
		= (DynamicPatterns patterns, free_vars, dynamics, cos)
	collectVariables NoPattern free_vars dynamics cos
		= (NoPattern, free_vars, dynamics, cos)

instance collectVariables AlgebraicPattern
where
	collectVariables pattern=:{ap_vars,ap_expr} free_vars dynamics cos
		# cos = {cos & cos_var_heap = clearCount ap_vars cIsALocalVar cos.cos_var_heap}
		  (ap_expr, free_vars, dynamics, cos) = collectVariables ap_expr free_vars dynamics cos
		  (ap_vars, cos_var_heap) = retrieveRefCounts ap_vars cos.cos_var_heap
		= ({ pattern & ap_expr = ap_expr, ap_vars = ap_vars }, free_vars, dynamics, { cos & cos_var_heap = cos_var_heap })
	
instance collectVariables BasicPattern
where
	collectVariables pattern=:{bp_expr} free_vars dynamics cos
		# (bp_expr, free_vars, dynamics, cos) = collectVariables bp_expr free_vars dynamics cos
		= ({ pattern & bp_expr = bp_expr }, free_vars, dynamics, cos)

instance collectVariables DynamicPattern
where
	collectVariables pattern=:{dp_var,dp_rhs,dp_type} free_vars dynamics cos=:{cos_var_heap,cos_expression_heap}
		# cos_var_heap = clearCount dp_var cIsALocalVar cos_var_heap
		  (EI_DynamicTypeWithVars vars type _, cos_expression_heap) = readPtr dp_type cos_expression_heap
		  cos = { cos & cos_var_heap = cos_var_heap, cos_expression_heap = cos_expression_heap }
		  (dp_rhs, free_vars, local_dynamics, cos) = collectVariables dp_rhs free_vars [] cos
		  cos_expression_heap = cos.cos_expression_heap <:= (dp_type, EI_DynamicTypeWithVars vars type local_dynamics)
		  (dp_var, cos_var_heap) = retrieveRefCount dp_var cos.cos_var_heap
		  cos = { cos & cos_var_heap = cos_var_heap, cos_expression_heap = cos_expression_heap }
		= ({ pattern & dp_rhs = dp_rhs, dp_var = dp_var }, free_vars, [dp_type:dynamics], cos)

instance collectVariables DynamicExpr
where
	collectVariables dynamic_expr=:{dyn_expr, dyn_info_ptr} free_vars dynamics cos
		# (dyn_expr, free_vars, local_dynamics, cos=:{cos_expression_heap}) = collectVariables dyn_expr free_vars [] cos
		  cos_expression_heap = mark_used_dynamic dyn_info_ptr local_dynamics (readPtr dyn_info_ptr cos_expression_heap)
		= ({dynamic_expr & dyn_expr = dyn_expr}, free_vars, [dyn_info_ptr:dynamics], { cos & cos_expression_heap = cos_expression_heap });
	where
		mark_used_dynamic dyn_info_ptr local_dynamics (EI_UnmarkedDynamic opt_type _, symbol_heap) 
			= symbol_heap <:= (dyn_info_ptr, EI_Dynamic opt_type local_dynamics)
		mark_used_dynamic dyn_info_ptr local_dynamics (EI_Dynamic opt_type _, symbol_heap) 
			= symbol_heap <:= (dyn_info_ptr, EI_Dynamic opt_type local_dynamics)

instance collectVariables BoundVar
where
	collectVariables var=:{var_ident,var_info_ptr,var_expr_ptr} free_vars dynamics cos=:{cos_var_heap}
		# (var_info, cos_var_heap) = readPtr var_info_ptr cos_var_heap
		  cos = { cos & cos_var_heap = cos_var_heap }
		= case var_info of
			VI_Count count is_global
				| count > 0 || is_global
					-> (var, free_vars, dynamics, { cos & cos_var_heap = writePtr var_info_ptr (VI_Count (inc count) is_global) cos.cos_var_heap })
					-> (var, [{fv_ident = var_ident, fv_info_ptr = var_info_ptr, fv_def_level = NotALevel, fv_count = 0} : free_vars ], dynamics,
								{ cos & cos_var_heap = writePtr var_info_ptr (VI_Count 1 is_global) cos.cos_var_heap })
			VI_Alias alias
				#  (original, free_vars, dynamics, cos) = collectVariables alias free_vars dynamics cos
				-> ({ original & var_expr_ptr = var_expr_ptr }, free_vars, dynamics, cos)
			VI_RefFromTupleSel0 count
				-> (var, free_vars, dynamics, { cos & cos_var_heap = writePtr var_info_ptr (VI_Count (inc count) False) cos.cos_var_heap })
			VI_RefFromArrayUpdate count _
				-> (var, free_vars, dynamics, { cos & cos_var_heap = writePtr var_info_ptr (VI_Count (inc count) False) cos.cos_var_heap })
			VI_RefFromArrayUpdateToTupleSelector2 count _ array_var_info_ptr
				# cos_var_heap = remove_VI_RefFromArrayUpdateOfTupleElem2 array_var_info_ptr cos_var_heap
				# cos_var_heap = writePtr var_info_ptr (VI_Count (inc count) False) cos.cos_var_heap
				-> (var, free_vars, dynamics, { cos & cos_var_heap = cos_var_heap })
			VI_RefFromArrayUpdateOfTupleElem2 count _
				-> (var, free_vars, dynamics, { cos & cos_var_heap = writePtr var_info_ptr (VI_Count (inc count) False) cos.cos_var_heap })
			_
				-> abort "collectVariables [BoundVar] (transform)"  //---> (var_info ,var_ident, ptrToInt var_info_ptr)

collectTupleSelect0Var var=:{var_ident,var_info_ptr,var_expr_ptr} free_vars dynamics cos=:{cos_var_heap}
	# (var_info, cos_var_heap) = readPtr var_info_ptr cos_var_heap
	  cos = { cos & cos_var_heap = cos_var_heap }
	= case var_info of
		VI_Count count is_global
			| count > 0 || is_global
				-> (var, free_vars, dynamics, { cos & cos_var_heap = writePtr var_info_ptr (VI_Count (inc count) is_global) cos.cos_var_heap })
				-> (var, [{fv_ident = var_ident, fv_info_ptr = var_info_ptr, fv_def_level = NotALevel, fv_count = 0} : free_vars ], dynamics,
							{ cos & cos_var_heap = writePtr var_info_ptr (VI_RefFromTupleSel0 1) cos.cos_var_heap })
		VI_Alias alias
			#  (original, free_vars, dynamics, cos) = collectVariables alias free_vars dynamics cos
			-> ({ original & var_expr_ptr = var_expr_ptr }, free_vars, dynamics, cos)
		VI_RefFromTupleSel0 count
			-> (var, free_vars, dynamics, { cos & cos_var_heap = writePtr var_info_ptr (VI_RefFromTupleSel0 (inc count)) cos.cos_var_heap })			
		VI_RefFromArrayUpdate count _
			-> (var, free_vars, dynamics, { cos & cos_var_heap = writePtr var_info_ptr (VI_Count (inc count) False) cos.cos_var_heap })
		VI_RefFromArrayUpdateToTupleSelector2 count _ array_var_info_ptr
			# cos_var_heap = remove_VI_RefFromArrayUpdateOfTupleElem2 array_var_info_ptr cos_var_heap
			# cos_var_heap = writePtr var_info_ptr (VI_Count (inc count) False) cos.cos_var_heap
			-> (var, free_vars, dynamics, { cos & cos_var_heap = cos_var_heap })
		VI_RefFromArrayUpdateOfTupleElem2 count selectors
			-> (var, free_vars, dynamics, { cos & cos_var_heap = writePtr var_info_ptr (VI_RefFromArrayUpdateOfTupleElem2 (inc count) selectors) cos.cos_var_heap })

remove_VI_RefFromArrayUpdateOfTupleElem2 array_var_info_ptr var_heap
	# (array_var_info, var_heap) = readPtr array_var_info_ptr var_heap
	= case array_var_info of
		VI_RefFromArrayUpdateOfTupleElem2 count _
			-> writePtr array_var_info_ptr (VI_Count count False) var_heap
		_
			-> var_heap

collectUpdateVar :: !BoundVar ![Selection] ![FreeVar] ![DynamicPtr] !*CollectState -> (!BoundVar, ![FreeVar],![DynamicPtr],!*CollectState)
collectUpdateVar var=:{var_ident,var_info_ptr,var_expr_ptr} update_selectors free_vars dynamics cos=:{cos_var_heap}
	# (var_info, cos_var_heap) = readPtr var_info_ptr cos_var_heap
	= case var_info of
		VI_Count count is_global
			| count > 0 || is_global
				-> (var, free_vars, dynamics, { cos & cos_var_heap = writePtr var_info_ptr (VI_Count (inc count) is_global) cos_var_heap })
				-> (var, [{fv_ident = var_ident, fv_info_ptr = var_info_ptr, fv_def_level = NotALevel, fv_count = 0} : free_vars ], dynamics,
							{ cos & cos_var_heap = writePtr var_info_ptr (VI_RefFromArrayUpdate 1 update_selectors) cos_var_heap })
		VI_Alias alias
			# (original, free_vars, dynamics, cos) = collectUpdateVar alias update_selectors free_vars dynamics { cos & cos_var_heap = cos_var_heap }
			-> ({ original & var_expr_ptr = var_expr_ptr }, free_vars, dynamics, cos)
		VI_RefFromTupleSel0 count
			-> (var, free_vars, dynamics, { cos & cos_var_heap = writePtr var_info_ptr (VI_Count (inc count) False) cos_var_heap })
		VI_RefFromArrayUpdate count selectors
			| same_selections selectors update_selectors
				-> (var, free_vars, dynamics, { cos & cos_var_heap = writePtr var_info_ptr (VI_RefFromArrayUpdate (inc count) update_selectors) cos_var_heap })
				-> (var, free_vars, dynamics, { cos & cos_var_heap = writePtr var_info_ptr (VI_Count (inc count) False) cos_var_heap })
		VI_RefFromArrayUpdateToTupleSelector2 count selectors array_var_info_ptr
			# cos_var_heap = remove_VI_RefFromArrayUpdateOfTupleElem2 array_var_info_ptr cos_var_heap
			# cos_var_heap = writePtr var_info_ptr (VI_Count (inc count) False) cos_var_heap
			-> (var, free_vars, dynamics, { cos & cos_var_heap = cos_var_heap })			
		VI_RefFromArrayUpdateOfTupleElem2 count _
			-> (var, free_vars, dynamics, { cos & cos_var_heap = writePtr var_info_ptr (VI_Count (inc count) False) cos_var_heap })

collectUpdateTupleSelect2Var :: !BoundVar ![Selection] ![FreeVar] ![DynamicPtr] !*CollectState -> (!BoundVar, ![FreeVar],![DynamicPtr],!*CollectState)
collectUpdateTupleSelect2Var var=:{var_ident,var_info_ptr,var_expr_ptr} update_selectors free_vars dynamics cos=:{cos_var_heap}
	# (var_info, cos_var_heap) = readPtr var_info_ptr cos_var_heap
	= case var_info of
		VI_Count count is_global
			| count > 0 || is_global
				-> (var, free_vars, dynamics, { cos & cos_var_heap = writePtr var_info_ptr (VI_Count (inc count) is_global) cos_var_heap })
				-> (var, [{fv_ident = var_ident, fv_info_ptr = var_info_ptr, fv_def_level = NotALevel, fv_count = 0} : free_vars ], dynamics,
							{ cos & cos_var_heap = writePtr var_info_ptr (VI_RefFromArrayUpdateOfTupleElem2 1 update_selectors) cos_var_heap })
		VI_Alias alias
			# (original, free_vars, dynamics, cos) = collectUpdateTupleSelect2Var alias update_selectors free_vars dynamics { cos & cos_var_heap = cos_var_heap }
			-> ({ original & var_expr_ptr = var_expr_ptr }, free_vars, dynamics, cos)
		VI_RefFromTupleSel0 count
			-> (var, free_vars, dynamics, { cos & cos_var_heap = writePtr var_info_ptr (VI_RefFromArrayUpdateOfTupleElem2 (inc count) update_selectors) cos_var_heap })
		VI_RefFromArrayUpdate count _
			-> (var, free_vars, dynamics, { cos & cos_var_heap = writePtr var_info_ptr (VI_Count (inc count) False) cos_var_heap })
		VI_RefFromArrayUpdateToTupleSelector2 count selectors array_var_info_ptr
			# cos_var_heap = remove_VI_RefFromArrayUpdateOfTupleElem2 array_var_info_ptr cos_var_heap
			# cos_var_heap = writePtr var_info_ptr (VI_Count (inc count) False) cos_var_heap			
			-> (var, free_vars, dynamics, { cos & cos_var_heap = cos_var_heap })			
		VI_RefFromArrayUpdateOfTupleElem2 count selectors
			| same_selections selectors update_selectors
				-> (var, free_vars, dynamics, { cos & cos_var_heap = writePtr var_info_ptr (VI_RefFromArrayUpdateOfTupleElem2 (inc count) update_selectors) cos_var_heap })
				-> (var, free_vars, dynamics, { cos & cos_var_heap = writePtr var_info_ptr (VI_Count (inc count) False) cos_var_heap })

collectUpdateVarTupleSelect2Var :: !BoundVar !VarInfoPtr !Int ![Selection] ![FreeVar] ![DynamicPtr] !*CollectState -> (!BoundVar, ![FreeVar],![DynamicPtr],!*CollectState)
collectUpdateVarTupleSelect2Var var=:{var_ident,var_info_ptr,var_expr_ptr} array_var_info_ptr count update_selectors free_vars dynamics cos=:{cos_var_heap}
	# (var_info, cos_var_heap) = readPtr var_info_ptr cos_var_heap
	= case var_info of
		VI_Count count is_global
			| count > 0 || is_global
				-> (var, free_vars, dynamics, { cos & cos_var_heap = writePtr var_info_ptr (VI_Count (inc count) is_global) cos_var_heap })
				# cos_var_heap = writePtr var_info_ptr (VI_RefFromArrayUpdateOfTupleElem2 1 update_selectors) cos_var_heap
				# cos_var_heap = writePtr array_var_info_ptr (VI_RefFromArrayUpdateToTupleSelector2 count update_selectors var_info_ptr) cos_var_heap
				# cos = { cos & cos_var_heap = cos_var_heap}
				-> (var, [{fv_ident = var_ident, fv_info_ptr = var_info_ptr, fv_def_level = NotALevel, fv_count = 0} : free_vars ], dynamics, cos)
		VI_Alias alias
			# (original, free_vars, dynamics, cos) = collectUpdateVarTupleSelect2Var alias array_var_info_ptr count update_selectors free_vars dynamics { cos & cos_var_heap = cos_var_heap }
			-> ({ original & var_expr_ptr = var_expr_ptr }, free_vars, dynamics, cos)
		VI_RefFromTupleSel0 count
			# cos_var_heap = writePtr var_info_ptr (VI_RefFromArrayUpdateOfTupleElem2 (inc count) update_selectors) cos_var_heap
			# cos_var_heap = writePtr array_var_info_ptr (VI_RefFromArrayUpdateToTupleSelector2 count update_selectors var_info_ptr) cos_var_heap
			-> (var, free_vars, dynamics, { cos & cos_var_heap = cos_var_heap})
		VI_RefFromArrayUpdate count _
			-> (var, free_vars, dynamics, { cos & cos_var_heap = writePtr var_info_ptr (VI_Count (inc count) False) cos_var_heap })
		VI_RefFromArrayUpdateToTupleSelector2 count selectors array_var_info_ptr
			# cos_var_heap = remove_VI_RefFromArrayUpdateOfTupleElem2 array_var_info_ptr cos_var_heap
			# cos_var_heap = writePtr var_info_ptr (VI_Count (inc count) False) cos_var_heap
			-> (var, free_vars, dynamics, { cos & cos_var_heap = cos_var_heap })
		VI_RefFromArrayUpdateOfTupleElem2 count selectors
			| same_selections selectors update_selectors
				# cos_var_heap = writePtr var_info_ptr (VI_RefFromArrayUpdateOfTupleElem2 (inc count) update_selectors) cos_var_heap
				# cos_var_heap = writePtr array_var_info_ptr (VI_RefFromArrayUpdateToTupleSelector2 count update_selectors var_info_ptr) cos_var_heap
				-> (var, free_vars, dynamics, { cos & cos_var_heap = cos_var_heap})
				-> (var, free_vars, dynamics, { cos & cos_var_heap = writePtr var_info_ptr (VI_Count (inc count) False) cos_var_heap })

same_selections [ArraySelection array_select1 _ index_expr1:selections1] [ArraySelection array_select2 _ index_expr2:selections2]
	= equal_index index_expr1 index_expr2 && same_selections selections1 selections2
where
	equal_index (Var {var_info_ptr=var_info_ptr1}) (Var {var_info_ptr=var_info_ptr2})
		= var_info_ptr1==var_info_ptr2
	equal_index (BasicExpr (BVInt i1)) (BasicExpr (BVInt i2))
		= i1==i2
	equal_index _ _
		= False
same_selections [RecordSelection {glob_module=m1,glob_object={ds_index=i1}} f1:selections1] [RecordSelection {glob_module=m2,glob_object={ds_index=i2}} f2:selections2]
	= f1==f2 && m1==m2 && i1==i2 && same_selections selections1 selections2
same_selections [] []
	= True
same_selections selections update_selections
	= False

instance <<< (Ptr a)
where
	(<<<) file p = file <<< ptrToInt p

instance <<< VarInfo
  where
	(<<<) file (VI_Expression expr) = file <<< expr
	(<<<) file vi					= file <<< "VI??"

class reset_free_var_heap_pointers a :: !a !*VarHeap -> *VarHeap

instance reset_free_var_heap_pointers Expression
where
	reset_free_var_heap_pointers (App {app_args}) var_heap
		= reset_free_var_heap_pointers app_args var_heap
	reset_free_var_heap_pointers (expr @ exprs) var_heap
		= reset_free_var_heap_pointers expr (reset_free_var_heap_pointers exprs var_heap)
	reset_free_var_heap_pointers (Let {let_strict_binds,let_lazy_binds,let_expr}) var_heap
		= reset_free_var_heap_pointers let_expr (reset_bound_vars let_lazy_binds (reset_bound_vars let_strict_binds var_heap))
	reset_free_var_heap_pointers (Case {case_expr,case_guards,case_default}) var_heap
		= reset_free_var_heap_pointers case_default (reset_free_var_heap_pointers case_guards (reset_free_var_heap_pointers case_expr var_heap))
	reset_free_var_heap_pointers (Selection selector_kind expr selectors) var_heap
		= reset_free_var_heap_pointers expr (reset_free_var_heap_pointers selectors var_heap)
	reset_free_var_heap_pointers (Update expr1 selectors expr2) var_heap
		= reset_free_var_heap_pointers expr1 (reset_free_var_heap_pointers expr2 (reset_free_var_heap_pointers selectors var_heap))
	reset_free_var_heap_pointers (RecordUpdate cons_symbol expression bind_expressions) var_heap
		= reset_free_var_heap_pointers expression (reset_var_heap_pointers_of_bind_srcs bind_expressions var_heap)
	reset_free_var_heap_pointers (TupleSelect symbol argn_nr expr) var_heap
		= reset_free_var_heap_pointers expr var_heap
	reset_free_var_heap_pointers (MatchExpr cons_ident expr) var_heap
		= reset_free_var_heap_pointers expr var_heap
	reset_free_var_heap_pointers (DynamicExpr {dyn_expr}) var_heap
		= reset_free_var_heap_pointers dyn_expr var_heap
	reset_free_var_heap_pointers (TypeSignature type_function expr) var_heap
		= reset_free_var_heap_pointers expr var_heap
	reset_free_var_heap_pointers expr var_heap
		= var_heap

instance reset_free_var_heap_pointers Selection
where
	reset_free_var_heap_pointers (ArraySelection array_select expr_ptr index_expr) var_heap
		= reset_free_var_heap_pointers index_expr var_heap
	reset_free_var_heap_pointers (DictionarySelection var selectors expr_ptr index_expr) var_heap
		= reset_free_var_heap_pointers index_expr (reset_free_var_heap_pointers selectors var_heap)
	reset_free_var_heap_pointers record_selection var_heap
		= var_heap

instance reset_free_var_heap_pointers FreeVar
where
	reset_free_var_heap_pointers {fv_info_ptr} var_heap
		= writePtr fv_info_ptr VI_Empty var_heap

reset_var_heap_pointers_of_bind_srcs [{bind_src}:binds] var_heap
	= reset_var_heap_pointers_of_bind_srcs binds (reset_free_var_heap_pointers bind_src var_heap)
reset_var_heap_pointers_of_bind_srcs [] var_heap
	= var_heap

reset_bound_vars [{lb_dst={fv_info_ptr},lb_src} : binds] var_heap
	= reset_bound_vars binds (reset_free_var_heap_pointers lb_src (writePtr fv_info_ptr VI_Empty var_heap))
reset_bound_vars [] var_heap
	= var_heap

instance reset_free_var_heap_pointers CasePatterns
where
	reset_free_var_heap_pointers (AlgebraicPatterns type patterns) var_heap
		= reset_free_var_heap_pointers patterns var_heap
	reset_free_var_heap_pointers (BasicPatterns type patterns) var_heap
		= reset_free_var_heap_pointers patterns var_heap
	reset_free_var_heap_pointers (OverloadedListPatterns type decons_expr patterns) var_heap
		= reset_free_var_heap_pointers patterns (reset_free_var_heap_pointers decons_expr var_heap)
	reset_free_var_heap_pointers (NewTypePatterns type patterns) var_heap
		= reset_free_var_heap_pointers patterns var_heap
	reset_free_var_heap_pointers (DynamicPatterns patterns) var_heap
		= reset_free_var_heap_pointers patterns var_heap

instance reset_free_var_heap_pointers AlgebraicPattern
where
	reset_free_var_heap_pointers {ap_vars,ap_expr} var_heap
		= reset_free_var_heap_pointers ap_expr (reset_free_var_heap_pointers ap_vars var_heap)

instance reset_free_var_heap_pointers BasicPattern
where
	reset_free_var_heap_pointers {bp_expr} var_heap
		= reset_free_var_heap_pointers bp_expr var_heap

instance reset_free_var_heap_pointers DynamicPattern
where
	reset_free_var_heap_pointers {dp_var,dp_rhs} var_heap
		= reset_free_var_heap_pointers dp_rhs (reset_free_var_heap_pointers dp_var var_heap)

instance reset_free_var_heap_pointers [a] | reset_free_var_heap_pointers a
where
	reset_free_var_heap_pointers [x : xs] s
		= reset_free_var_heap_pointers xs (reset_free_var_heap_pointers x s)
	reset_free_var_heap_pointers [] s
	 	= s

instance reset_free_var_heap_pointers (Optional a) | reset_free_var_heap_pointers a
where
	reset_free_var_heap_pointers (Yes x) var_heap
		= reset_free_var_heap_pointers x var_heap
	reset_free_var_heap_pointers no var_heap
		= var_heap

instance reset_free_var_heap_pointers CheckedAlternative
where
	reset_free_var_heap_pointers {ca_rhs} var_heap
		=  reset_free_var_heap_pointers ca_rhs var_heap
