implementation module checktypes

import StdEnv, compare_types
import syntax, checksupport, typesupport, utilities
import genericsupport
from explicitimports import search_qualified_ident,::NameSpaceN,TypeNameSpaceN,ClassNameSpaceN

::	TypeSymbols = 
	{	ts_type_defs		:: !.{# CheckedTypeDef}
	,	ts_cons_defs 		:: !.{# ConsDef}
	,	ts_selector_defs	:: !.{# SelectorDef}
	,	ts_modules			:: !.{# DclModule}
	}

::	TypeInfo =
	{	ti_var_heap			:: !.VarHeap
	,	ti_type_heaps		:: !.TypeHeaps
	,	ti_used_types		:: ![SymbolPtr]
	}

::	CurrentTypeInfo =
	{	cti_module_index	:: !Index
	,	cti_type_index		:: !Index
	,	cti_lhs_attribute	:: !TypeAttribute
	}

bindArgAType :: !CurrentTypeInfo !AType !v:{#ClassDef}  !(!*TypeSymbols, !*TypeInfo, !*CheckState)
			-> (!AType, !TypeAttribute, !v:{#ClassDef}, !(!*TypeSymbols, !*TypeInfo, !*CheckState))
bindArgAType cti {at_attribute,at_type=TFA vars type} class_defs (ts, ti=:{ti_type_heaps}, cs)
	# (type_vars, (_, ti_type_heaps, cs)) = addTypeVariablesToSymbolTable cRankTwoScope vars [] ti_type_heaps cs
	  (type, _, (ts, ti, cs)) = bindTypes cti type (ts, {ti & ti_type_heaps = ti_type_heaps}, cs)
	  cs & cs_symbol_table = removeAttributedTypeVarsFromSymbolTable cRankTwoScope type_vars cs.cs_symbol_table
	  at_type = TFA type_vars type
	  (attype,combined_attribute,ts_ti_cs) = bindAttributes TA_Multi cti at_attribute at_type (ts, ti, cs)
	= (attype,combined_attribute,class_defs,ts_ti_cs)
bindArgAType cti {at_attribute,at_type=TFAC vars type contexts} class_defs (ts, ti=:{ti_type_heaps}, cs)
	# (type_vars, (_, ti_type_heaps, cs)) = addTypeVariablesToSymbolTable cRankTwoScope vars [] ti_type_heaps cs
	  (type, _, (ts, ti, cs)) = bindTypes cti type (ts, {ti & ti_type_heaps = ti_type_heaps}, cs)
	  (contexts,class_defs,ts,ti,cs) = bind_rank2_context_of_cons contexts cti class_defs ts ti cs
	  cs & cs_symbol_table = removeAttributedTypeVarsFromSymbolTable cRankTwoScope type_vars cs.cs_symbol_table
	  at_type = TFAC type_vars type contexts
	  (attype,combined_attribute,ts_ti_cs) = bindAttributes TA_Multi cti at_attribute at_type (ts, ti, cs)
	= (attype,combined_attribute,class_defs,ts_ti_cs)
bindArgAType cti {at_attribute,at_type} class_defs ts_ti_cs
	# (at_type, type_attr, ts_ti_cs) = bindTypes cti at_type ts_ti_cs
	  (attype,combined_attribute,ts_ti_cs) = bindAttributes type_attr cti at_attribute at_type ts_ti_cs
	= (attype,combined_attribute,class_defs,ts_ti_cs)

bind_rank2_context_of_cons [context=:{tc_class,tc_types}:contexts] cti class_defs ts ti cs
	# (ok,tc_class,fun_dep_vars,class_defs,modules,cs=:{cs_error}) = check_context_class tc_class tc_types cti.cti_module_index class_defs ts.ts_modules cs
	// to do: use ok and fun_dep_vars
	  ts = {ts & ts_modules=modules}
	| cs_error.ea_ok
	 	# (tc_types, _, (ts,ti,cs)) = bindTypes cti tc_types (ts,ti,cs)
		  cs = check_context_types tc_class tc_types cs
		  (contexts,class_defs,ts,ti,cs) = bind_rank2_context_of_cons contexts cti class_defs ts ti cs
		#! contexts = [{context & tc_class=tc_class, tc_types=tc_types}:contexts]
		| cs_error.ea_ok
			# cs = foldSt check_rank2_vars_in_type tc_types cs
			= (contexts,class_defs,ts,ti,cs)
			= (contexts,class_defs,ts,ti,cs)
		# (contexts,class_defs,ts,ti,cs) = bind_rank2_context_of_cons contexts cti class_defs ts ti cs
		= ([{context & tc_types = []}:contexts],class_defs,ts,ti,cs)
where
	check_rank2_vars_in_atypes [{at_type}:tc_types] cs
		= check_rank2_vars_in_atypes tc_types (check_rank2_vars_in_type at_type cs)
	check_rank2_vars_in_atypes [] cs
		= cs
	
	check_rank2_vars_in_type (TV {tv_ident}) cs=:{cs_symbol_table}
		| (sreadPtr tv_ident.id_info cs_symbol_table).ste_def_level==cRankTwoScope
			= cs
			= {cs & cs_error = checkError tv_ident "universally quantified type variable expected" cs.cs_error}
	check_rank2_vars_in_type (TA _ atypes) cs
		= check_rank2_vars_in_atypes atypes cs
	check_rank2_vars_in_type (TAS _ atypes _) cs
		= check_rank2_vars_in_atypes atypes cs
	check_rank2_vars_in_type (arg_type --> res_type) cs
		= check_rank2_vars_in_type res_type.at_type (check_rank2_vars_in_type arg_type.at_type cs)
	check_rank2_vars_in_type (TArrow1 {at_type}) cs
		= check_rank2_vars_in_type at_type cs
	check_rank2_vars_in_type (CV {tv_ident} :@: types) cs=:{cs_symbol_table}
		| (sreadPtr tv_ident.id_info cs_symbol_table).ste_def_level==cRankTwoScope
			= check_rank2_vars_in_atypes types cs
			# cs & cs_error = checkError tv_ident "universally quantified type variable expected" cs.cs_error
			= check_rank2_vars_in_atypes types cs
	check_rank2_vars_in_type _ cs
		= cs
bind_rank2_context_of_cons [] cti class_defs ts ti cs
	= ([],class_defs,ts,ti,cs)

class bindTypes type :: !CurrentTypeInfo !type !(!*TypeSymbols, !*TypeInfo, !*CheckState)
					-> (!type, !TypeAttribute, !(!*TypeSymbols, !*TypeInfo, !*CheckState))

instance bindTypes AType
where
	bindTypes cti {at_attribute,at_type} ts_ti_cs
		# (at_type, type_attr, ts_ti_cs) = bindTypes cti at_type ts_ti_cs
		= bindAttributes type_attr cti at_attribute at_type ts_ti_cs

bindAttributes :: !TypeAttribute !CurrentTypeInfo !TypeAttribute !Type !(!*TypeSymbols, !*TypeInfo, !*CheckState)
										   -> (!AType, !TypeAttribute, !(!*TypeSymbols, !*TypeInfo, !*CheckState))
bindAttributes type_attr cti at_attribute at_type (ts, ti, cs)
	# cs_error = check_attr_of_type_var at_attribute at_type cs.cs_error
	  (combined_attribute, cs_error) = check_type_attribute at_attribute type_attr cti.cti_lhs_attribute cs_error
	= ({ at_attribute = combined_attribute, at_type = at_type }, combined_attribute, (ts, ti, { cs & cs_error = cs_error }))
where
	check_type_attribute :: !TypeAttribute !TypeAttribute !TypeAttribute !*ErrorAdmin -> (!TypeAttribute,!*ErrorAdmin)
	check_type_attribute TA_Anonymous type_attr root_attr error
		| try_to_combine_attributes type_attr root_attr
			= (to_root_attr root_attr, error)
			= (TA_Multi, checkError "conflicting attribution of type definition" "" error)
	where
		to_root_attr (TA_Var var)
			= TA_RootVar var
		to_root_attr attr
			= attr
	check_type_attribute TA_Unique type_attr root_attr error
		| try_to_combine_attributes TA_Unique type_attr || try_to_combine_attributes TA_Unique root_attr
			= (TA_Unique, error)
			= (TA_Multi, checkError "conflicting attribution of type definition" "" error)
	check_type_attribute (TA_Var var) _ _ error
		= (TA_Multi, checkError var "attribute variable not allowed" error)
	check_type_attribute (TA_RootVar var) _ _ error
		= (TA_Multi, checkError var "attribute variable not allowed" error)
	check_type_attribute _ type_attr root_attr error
		= (type_attr, error)

	try_to_combine_attributes :: !TypeAttribute !TypeAttribute -> Bool
	try_to_combine_attributes TA_Multi _
		= True
	try_to_combine_attributes (TA_Var attr_var1) (TA_Var attr_var2)
		= attr_var1.av_ident == attr_var2.av_ident
	try_to_combine_attributes TA_Unique TA_Unique
		= True
	try_to_combine_attributes TA_Unique TA_Multi
		= True
	try_to_combine_attributes _ _
		= False

	check_attr_of_type_var :: !TypeAttribute !Type !*ErrorAdmin -> .ErrorAdmin 
	check_attr_of_type_var TA_Unique (TV var) error
		// the case "TA_Var" is catched by check_type_attribute
		= checkError var "uniqueness attribute not allowed" error
	check_attr_of_type_var TA_Anonymous (CV tv :@: types) error
		= checkError tv "attribute variable not allowed" error
	check_attr_of_type_var attr _ error
		= error
				
instance bindTypes TypeVar
where
	bindTypes cti tv=:{tv_ident=var_id=:{id_info}} (ts, ti, cs=:{cs_symbol_table})
		# (var_def, cs_symbol_table) = readPtr id_info cs_symbol_table
		  cs = { cs & cs_symbol_table = cs_symbol_table }
		= case var_def.ste_kind of
			STE_BoundTypeVariable {stv_info_ptr,stv_attribute}
				-> ({ tv & tv_info_ptr = stv_info_ptr}, stv_attribute, (ts, ti, cs))
			_
				-> (tv, TA_Multi, (ts, ti, {cs & cs_error = checkError var_id "type variable undefined" cs.cs_error}))

instance bindTypes [a] | bindTypes a
where
	bindTypes cti [] ts_ti_cs
		= ([], TA_Multi, ts_ti_cs)
	bindTypes cti [x : xs] ts_ti_cs
		# (x, _, ts_ti_cs) = bindTypes cti x ts_ti_cs
		  (xs, attr, ts_ti_cs) = bindTypes cti xs ts_ti_cs
		= ([x : xs], attr, ts_ti_cs)

retrieveTypeDefinition :: SymbolPtr !Index !*SymbolTable ![SymbolPtr] -> (!Index, !Index, !*SymbolTable, ![SymbolPtr])
retrieveTypeDefinition type_ptr mod_index symbol_table used_types
	# (entry=:{ste_kind,ste_def_level,ste_index}, symbol_table)	= readPtr type_ptr symbol_table
	= case ste_kind of
		this_kind=:(STE_Imported STE_Type ste_mod_index)
			-> (ste_index, ste_mod_index, symbol_table <:= (type_ptr, { entry & ste_kind = STE_UsedType ste_mod_index this_kind }), [type_ptr : used_types])
		this_kind=:STE_Type
			| ste_def_level == cGlobalScope
				-> (ste_index, mod_index, symbol_table <:= (type_ptr, { entry & ste_kind = STE_UsedType mod_index this_kind }), [type_ptr : used_types])
				-> (NotFound, mod_index, symbol_table, used_types)
		STE_UsedType mod_index _
			-> (ste_index, mod_index, symbol_table, used_types)
		this_kind=:(STE_UsedQualifiedType uqt_mod_index uqt_index orig_kind)
			| uqt_mod_index==mod_index && uqt_index==ste_index
				-> (ste_index, mod_index, symbol_table, used_types) 
				-> retrieve_type_definition orig_kind
		with
			retrieve_type_definition (STE_UsedQualifiedType uqt_mod_index uqt_index orig_kind)
				| uqt_mod_index==mod_index && uqt_index==ste_index
					= (ste_index, mod_index, symbol_table, used_types)
					= retrieve_type_definition orig_kind
			retrieve_type_definition (STE_Imported STE_Type ste_mod_index)
				= (ste_index, ste_mod_index, symbol_table <:= (type_ptr, { entry & ste_kind = STE_UsedType ste_mod_index this_kind }), used_types)
			retrieve_type_definition STE_Type
				| ste_def_level == cGlobalScope
					= (ste_index, mod_index, symbol_table <:= (type_ptr, { entry & ste_kind = STE_UsedType mod_index this_kind }), used_types)
					= (NotFound, mod_index, symbol_table, used_types)
			retrieve_type_definition (STE_UsedType mod_index _)
				= (ste_index, mod_index, symbol_table, used_types)
			retrieve_type_definition _
				= (NotFound, mod_index, symbol_table, used_types)
		_
			-> (NotFound, mod_index, symbol_table, used_types)

determine_type_attribute TA_Unique		= TA_Unique
determine_type_attribute _				= TA_Multi

instance bindTypes Type
where
	bindTypes cti (TV tv) ts_ti_cs
		# (tv, attr, ts_ti_cs) = bindTypes cti tv ts_ti_cs
		= (TV tv, attr, ts_ti_cs)
	bindTypes cti=:{cti_module_index,cti_type_index,cti_lhs_attribute} type=:(TA type_cons=:{type_ident=type_ident=:{id_info}} types)
					(ts=:{ts_type_defs,ts_modules}, ti, cs=:{cs_symbol_table})
		# (type_index, type_module, cs_symbol_table, ti_used_types) = retrieveTypeDefinition id_info cti_module_index cs_symbol_table ti.ti_used_types
		  ti = { ti & ti_used_types = ti_used_types }
		# cs = { cs & cs_symbol_table = cs_symbol_table }
		| type_index <> NotFound
			# ({td_arity,td_attribute,td_rhs},type_index,ts_type_defs,ts_modules) = getTypeDef type_index type_module cti_module_index ts_type_defs ts_modules
			  ts = { ts & ts_type_defs = ts_type_defs, ts_modules = ts_modules }
			| checkArityOfType type_cons.type_arity td_arity td_rhs
				# (types, _, ts_ti_cs) = bindTypes cti types (ts, ti, cs)
				| type_module == cti_module_index && cti_type_index == type_index
					= (TA { type_cons & type_index = { glob_object = type_index, glob_module = type_module}} types, cti_lhs_attribute, ts_ti_cs)
					= (TA { type_cons & type_index = { glob_object = type_index, glob_module = type_module}} types,
								determine_type_attribute td_attribute, ts_ti_cs)
				= (TE, TA_Multi, (ts, ti, { cs & cs_error = checkError type_cons.type_ident "used with wrong arity" cs.cs_error }))
			= (TE, TA_Multi, (ts, ti, { cs & cs_error = checkError type_cons.type_ident "undefined" cs.cs_error}))
	bindTypes cti=:{cti_module_index,cti_type_index,cti_lhs_attribute} type=:(TAS type_cons=:{type_ident=type_ident=:{id_info}} types strictness)
					(ts=:{ts_type_defs,ts_modules}, ti, cs=:{cs_symbol_table})
		# (type_index, type_module, cs_symbol_table, ti_used_types) = retrieveTypeDefinition id_info cti_module_index cs_symbol_table ti.ti_used_types
		  ti = { ti & ti_used_types = ti_used_types }
		# cs = { cs & cs_symbol_table = cs_symbol_table }
		| type_index <> NotFound
			# ({td_arity,td_attribute,td_rhs},type_index,ts_type_defs,ts_modules) = getTypeDef type_index type_module cti_module_index ts_type_defs ts_modules
			  ts = { ts & ts_type_defs = ts_type_defs, ts_modules = ts_modules }
			| checkArityOfType type_cons.type_arity td_arity td_rhs
				# (types, _, ts_ti_cs) = bindTypes cti types (ts, ti, cs)
				| type_module == cti_module_index && cti_type_index == type_index
					= (TAS { type_cons & type_index = { glob_object = type_index, glob_module = type_module}} types strictness, cti_lhs_attribute, ts_ti_cs)
					= (TAS { type_cons & type_index = { glob_object = type_index, glob_module = type_module}} types strictness,
								determine_type_attribute td_attribute, ts_ti_cs)
				= (TE, TA_Multi, (ts, ti, { cs & cs_error = checkError type_cons.type_ident "used with wrong arity" cs.cs_error }))
			= (TE, TA_Multi, (ts, ti, { cs & cs_error = checkError type_cons.type_ident "undefined" cs.cs_error}))	
	bindTypes cti (arg_type --> res_type) ts_ti_cs
		# (arg_type, _, ts_ti_cs) = bindTypes cti arg_type ts_ti_cs
		  (res_type, _, ts_ti_cs) = bindTypes cti res_type ts_ti_cs
		= (arg_type --> res_type, TA_Multi, ts_ti_cs)
	bindTypes cti (TArrow1 type) ts_ti_cs
		# (type, _, ts_ti_cs) = bindTypes cti type ts_ti_cs
		= (TArrow1 type, TA_Multi, ts_ti_cs)	
	bindTypes cti (CV tv :@: types) ts_ti_cs
		# (tv, type_attr, ts_ti_cs) = bindTypes cti tv ts_ti_cs
		  (types, _, ts_ti_cs) = bindTypes cti types ts_ti_cs
		= (CV tv :@: types, type_attr, ts_ti_cs)
	bindTypes cti=:{cti_module_index,cti_type_index,cti_lhs_attribute} type=:(TQualifiedIdent module_id type_name types)
					(ts=:{ts_type_defs,ts_modules}, ti, cs)
		# (found,{decl_kind,decl_ident=type_ident,decl_index=type_index},cs) = search_qualified_ident module_id type_name TypeNameSpaceN cs
		| not found
			= (TE, TA_Multi, (ts, ti, cs))
			= case decl_kind of
				STE_Imported STE_Type type_module
					# ({td_arity,td_attribute,td_rhs},type_index,ts_type_defs,ts_modules) = getTypeDef type_index type_module cti_module_index ts_type_defs ts_modules
					  ts = { ts & ts_type_defs = ts_type_defs, ts_modules = ts_modules }
					  (cs_symbol_table, ti_used_types) = add_qualified_type_to_used_types type_ident.id_info type_module type_index cs.cs_symbol_table ti.ti_used_types
					  cs = {cs & cs_symbol_table = cs_symbol_table}
					  ti = { ti & ti_used_types = ti_used_types }					  
					# type_cons = MakeNewTypeSymbIdent type_ident (length types)
					| checkArityOfType type_cons.type_arity td_arity td_rhs
						# (types, _, ts_ti_cs) = bindTypes cti types (ts, ti, cs)
						| type_module == cti_module_index && cti_type_index == type_index
							-> (TA { type_cons & type_index = { glob_object = type_index, glob_module = type_module}} types, cti_lhs_attribute, ts_ti_cs)
							-> (TA { type_cons & type_index = { glob_object = type_index, glob_module = type_module}} types,
										determine_type_attribute td_attribute, ts_ti_cs)
						-> (TE, TA_Multi, (ts, ti, { cs & cs_error = checkError type_cons.type_ident "used with wrong arity" cs.cs_error }))
				_
					-> (TE, TA_Multi, (ts, ti, { cs & cs_error = checkError ("'"+++module_id.id_name+++"'."+++type_name) "not imported" cs.cs_error}))
		where
			add_qualified_type_to_used_types symbol_table_ptr type_module type_index symbol_table used_types
				# (entry=:{ste_kind,ste_index}, symbol_table) = readPtr symbol_table_ptr symbol_table
				= case ste_kind of
					STE_UsedQualifiedType mod_index decl_index next_kind
						 | (mod_index==type_module && decl_index==type_index) || qualified_type_occurs next_kind ste_index type_module type_index
						 	-> (symbol_table, used_types)
						 	# entry = {entry & ste_kind = STE_UsedQualifiedType type_module type_index ste_kind }
						 	-> (writePtr symbol_table_ptr entry symbol_table, used_types)
					STE_UsedType ste_module next_kind
						 | (ste_module==type_module && ste_index==type_index) || qualified_type_occurs next_kind ste_index type_module type_index
						 	-> (symbol_table, used_types)
						 	# entry = {entry & ste_kind = STE_UsedQualifiedType type_module type_index ste_kind }
						 	-> (writePtr symbol_table_ptr entry symbol_table, used_types)
					_
						 	# entry = {entry & ste_kind = STE_UsedQualifiedType type_module type_index ste_kind }
						 	-> (writePtr symbol_table_ptr entry symbol_table, [symbol_table_ptr:used_types])

			qualified_type_occurs (STE_UsedQualifiedType mod_index decl_index next_kind) ste_index type_module type_index
				| mod_index==type_module && decl_index==type_index
					= True
					= qualified_type_occurs next_kind ste_index type_module type_index
			qualified_type_occurs (STE_UsedType ste_module next_kind) ste_index type_module type_index
				 | ste_module==type_module && ste_index==type_index
				 	= True
					= qualified_type_occurs next_kind ste_index type_module type_index
			qualified_type_occurs _ _ _ _
				= False
	bindTypes cti type=:(TFA vars _) (ts, ti, cs)
		# cs = universal_quantifier_error vars cs
		= (type, TA_Multi, (ts, ti, cs))
	bindTypes cti type ts_ti_cs
		= (type, TA_Multi, ts_ti_cs)

universal_quantifier_error [{atv_variable={tv_ident}}:_] cs
	= {cs & cs_error = checkError tv_ident "universally quantified type variable not allowed here" cs.cs_error}
universal_quantifier_error _ cs
	= {cs & cs_error = checkError "" "universal quantifier not allowed here" cs.cs_error}

addToAttributeEnviron :: !TypeAttribute !TypeAttribute ![AttrInequality] !*ErrorAdmin -> (![AttrInequality],!*ErrorAdmin)
addToAttributeEnviron TA_Multi _ attr_env error
	= (attr_env, error)
addToAttributeEnviron _ TA_Unique attr_env error
	= (attr_env, error)
addToAttributeEnviron (TA_Var attr_var) (TA_Var root_var) attr_env error
	| attr_var.av_info_ptr == root_var.av_info_ptr
		= (attr_env, error)
		= ([ { ai_demanded = attr_var, ai_offered = root_var } :  attr_env], error)
addToAttributeEnviron (TA_RootVar attr_var) root_attr attr_env error
	= (attr_env, error)
addToAttributeEnviron _ _ attr_env error
	= (attr_env, checkError "inconsistent attribution of type definition" "" error)

check_context_class :: TCClass [Type] Int u:{#ClassDef} v:{#DclModule} *CheckState
			 -> (!Bool,TCClass,!BITVECT,u:{#ClassDef},v:{#DclModule},*CheckState)
check_context_class (TCClass cl) tc_types mod_index class_defs modules cs
	# (entry, cs_symbol_table) = readPtr cl.glob_object.ds_ident.id_info cs.cs_symbol_table
  	# cs = { cs & cs_symbol_table = cs_symbol_table }
	# (class_index, class_module) = retrieveGlobalDefinition entry STE_Class mod_index
	| class_index <> NotFound
		# ({class_arity,class_fun_dep_vars}, class_index, class_defs, modules) = getClassDef class_index class_module mod_index class_defs modules
		| class_arity == cl.glob_object.ds_arity
			# checked_class = {cl & glob_module = class_module, glob_object = {cl.glob_object & ds_index = class_index}}
			= (True, TCClass checked_class, class_fun_dep_vars, class_defs, modules, cs) 
			# cs_error = checkError cl.glob_object.ds_ident	"class used with wrong arity" cs.cs_error
			= (False, TCClass cl, class_fun_dep_vars, class_defs, modules, {cs & cs_error = cs_error})
		# cs_error = checkError cl.glob_object.ds_ident	"class undefined" cs.cs_error	
		= (False, TCClass cl, 0, class_defs, modules, {cs & cs_error = cs_error})
check_context_class tc_class=:(TCQualifiedIdent module_id class_name) tc_types mod_index class_defs modules cs
	# (found,{decl_kind,decl_ident=type_ident,decl_index=class_index},cs) = search_qualified_ident module_id class_name ClassNameSpaceN cs
	| not found
		= (False, tc_class, 0, class_defs, modules, cs)
		= case decl_kind of
			STE_Imported STE_Class class_module
				# ({class_ident,class_fun_dep_vars,class_arity}, class_index, class_defs, modules) = getClassDef class_index class_module mod_index class_defs modules
				| class_arity == length tc_types
					# checked_class = { glob_object = MakeDefinedSymbol class_ident class_index class_arity, glob_module = class_module }
					-> (True, TCClass checked_class, class_fun_dep_vars, class_defs, modules, cs)
					# cs_error = checkError ("'"+++module_id.id_name+++"'."+++class_name) "class used with wrong arity" cs.cs_error
					-> (False, tc_class, class_fun_dep_vars, class_defs, modules, {cs & cs_error = cs_error})
			_
				-> (False, tc_class, 0, class_defs, modules, {cs & cs_error = checkError ("'"+++module_id.id_name+++"'."+++class_name) "class undefined" cs.cs_error})
check_context_class (TCGeneric gtc=:{gtc_generic, gtc_kind}) tc_types mod_index class_defs modules cs
  	# gen_ident = gtc_generic.glob_object.ds_ident
	# (entry, cs_symbol_table) = readPtr gen_ident.id_info cs.cs_symbol_table
  	# cs = { cs & cs_symbol_table = cs_symbol_table }
  	# clazz =
  		{ glob_module = -1
		, glob_object = {ds_ident = genericIdentToClassIdent gen_ident.id_name gtc_kind, ds_arity = 1, ds_index = -1}
		}
	# (generic_index, generic_module, generic_function_arity) = retrieveGlobalGenericDefinition entry mod_index
	| generic_index <> NotFound
		| gtc_generic.glob_object.ds_arity == 1
			# checked_gen = 
				{ glob_module = generic_module
				, glob_object = {gtc_generic.glob_object & ds_index = generic_index}					
				}
			| generic_function_arity<0
				= abort "error in check_context_class"
			#! generic_dict_index
				= if (generic_function_arity==0)
					PD_TypeGenericDict0
					PD_TypeGenericDict
			# ({pds_module,pds_def},cs) = cs!cs_predef_symbols.[generic_dict_index]
			  generic_dict = {gi_module=pds_module, gi_index=pds_def}
			#! tc_class = TCGeneric {gtc & gtc_generic = checked_gen, gtc_class=clazz, gtc_generic_dict=generic_dict}
			| not cs.cs_x.x_check_dynamic_types
				= (True, tc_class, 0, class_defs, modules, cs)
				# cs = {cs & cs_error = checkError gen_ident "a generic context is not allowed in a dynamic type" cs.cs_error}
				= (True, tc_class, 0, class_defs, modules, cs)
			# cs_error = checkError gen_ident "generic used with wrong arity: generic always has one class argument" cs.cs_error  
			= (False, TCGeneric {gtc & gtc_class=clazz}, 0, class_defs, modules, {cs & cs_error = cs_error})
		# cs_error = checkError gen_ident "generic undefined" cs.cs_error
		= (True, TCGeneric {gtc & gtc_class=clazz}, 0, class_defs, modules, {cs & cs_error = cs_error})

check_context_types tc_class [] cs=:{cs_error}
	= {cs & cs_error = checkError tc_class "type context should contain one or more type variables" cs_error}
check_context_types tc_class [((CV {tv_ident}) :@: _):_] cs=:{cs_error}
	= cs
//		= { cs & cs_error = checkError tv_ident "not allowed as higher order type variable in context" cs_error}
check_context_types tc_class [TV _ : types] cs
	= cs
check_context_types tc_class [type : types] cs
	= check_context_types tc_class types cs

check_fun_dep_context_types tc_class [(CV _ :@: _):_] fun_dep_vars cs
	| fun_dep_vars bitand 1==0
		= cs
check_fun_dep_context_types tc_class [TV _ : types] fun_dep_vars cs
	| fun_dep_vars bitand 1==0
		= cs
check_fun_dep_context_types tc_class [type : types] fun_dep_vars cs
	= check_fun_dep_context_types tc_class types (fun_dep_vars>>1) cs
check_fun_dep_context_types tc_class [] fun_dep_vars cs=:{cs_error}
	= {cs & cs_error = checkError tc_class "type context should contain one or more type variables" cs_error}

emptyIdent name :== { id_name = name, id_info = nilPtr }

checkTypeDef :: !Index !Index !v:{#ClassDef} !*TypeSymbols !*TypeInfo !*CheckState -> (!v:{#ClassDef},!*TypeSymbols,!*TypeInfo,!*CheckState);
checkTypeDef type_index module_index class_defs ts=:{ts_type_defs} ti=:{ti_type_heaps} cs=:{cs_error}
	# (type_def, ts_type_defs) = ts_type_defs![type_index]
	# {td_ident,td_pos,td_args,td_attribute,td_index} = type_def
	| td_index == NoIndex
		# position = newPosition td_ident td_pos
		  cs_error = pushErrorAdmin position cs_error
		  (td_attribute, attr_vars, th_attrs) = determine_root_attribute td_attribute td_ident.id_name ti_type_heaps.th_attrs
		  (type_vars, (attr_vars, ti_type_heaps, cs))
		  		= addTypeVariablesToSymbolTable cGlobalScope td_args attr_vars { ti_type_heaps & th_attrs = th_attrs } { cs & cs_error = cs_error }
		  type_def = { type_def & td_args = type_vars, td_index = type_index, td_attrs = attr_vars, td_attribute = td_attribute }
		  (td_rhs, (class_defs,ts,ti,cs)) = check_rhs_of_TypeDef type_def attr_vars
				{ cti_module_index = module_index, cti_type_index = type_index, cti_lhs_attribute = td_attribute }
					(class_defs, {ts & ts_type_defs = ts_type_defs}, {ti & ti_type_heaps = ti_type_heaps}, cs)
		  (td_used_types, cs_symbol_table) = retrieve_used_types ti.ti_used_types cs.cs_symbol_table
		  cs = {cs &	cs_error = popErrorAdmin cs.cs_error,
						cs_symbol_table = removeAttributedTypeVarsFromSymbolTable cGlobalScope type_vars cs_symbol_table}
		= (class_defs, {ts & ts_type_defs = {ts.ts_type_defs & [type_index] = {type_def & td_rhs = td_rhs, td_used_types = td_used_types}}}, {ti & ti_used_types = []},cs)
		= (class_defs, {ts & ts_type_defs = ts_type_defs}, ti, cs)
where
	determine_root_attribute TA_None name attr_var_heap
		# (attr_info_ptr, attr_var_heap) = newPtr AVI_Empty attr_var_heap
		  new_var = { av_ident = emptyIdent name, av_info_ptr = attr_info_ptr}
		= (TA_Var new_var, [new_var], attr_var_heap)
	determine_root_attribute TA_Unique name attr_var_heap
		= (TA_Unique, [], attr_var_heap)

	check_rhs_of_TypeDef :: !CheckedTypeDef ![AttributeVar] !CurrentTypeInfo
					  !(!v:{#ClassDef},!*TypeSymbols,!*TypeInfo,!*CheckState)
		-> (!TypeRhs, !(!v:{#ClassDef},!*TypeSymbols,!*TypeInfo,!*CheckState))
	check_rhs_of_TypeDef {td_ident,td_arity,td_args,td_rhs = td_rhs=:AlgType conses} attr_vars cti=:{cti_module_index,cti_type_index,cti_lhs_attribute} class_defs_ts_ti_cs
		# type_lhs = { at_attribute = cti_lhs_attribute,
				  	   at_type = TA (MakeTypeSymbIdent { glob_object = cti_type_index, glob_module = cti_module_index } td_ident td_arity)
									[{at_attribute = atv_attribute,at_type = TV atv_variable} \\ {atv_variable, atv_attribute} <- td_args]}
		  class_defs_ts_ti_cs = bind_types_of_constructors cti 0 (atype_vars_to_type_vars td_args) attr_vars type_lhs conses class_defs_ts_ti_cs
		= (td_rhs, class_defs_ts_ti_cs)
	check_rhs_of_TypeDef {td_ident,td_arity,td_args,td_rhs = td_rhs=:RecordType {rt_constructor={ds_index,ds_arity}, rt_fields}}
			attr_vars cti=:{cti_module_index,cti_type_index,cti_lhs_attribute} (class_defs,ts,ti,cs)
		# type_lhs = {	at_attribute = cti_lhs_attribute,
						at_type = TA (MakeTypeSymbIdent { glob_object = cti_type_index, glob_module = cti_module_index } td_ident td_arity)
									[{ at_attribute = atv_attribute,at_type = TV atv_variable} \\ {atv_variable, atv_attribute} <- td_args]}
		  cs = if (ds_arity>32)
				{ cs & cs_error = checkError ("Record has too many fields ("+++toString ds_arity+++",") "32 are allowed)" cs.cs_error }
				cs;
		  (class_defs,ts,ti,cs) = bind_types_of_constructor cti 0 (atype_vars_to_type_vars td_args) attr_vars type_lhs ds_index (class_defs,ts,ti,cs)
		# (rec_cons_def, ts) = ts!ts_cons_defs.[ds_index]
		# {cons_type = { st_vars,st_args,st_result,st_attr_vars }, cons_exi_vars} = rec_cons_def
		# (ts_selector_defs, ti_var_heap, cs_error) = check_selectors 0 rt_fields cti_type_index st_args st_result st_vars st_attr_vars cons_exi_vars
					ts.ts_selector_defs ti.ti_var_heap cs.cs_error
		= (td_rhs, (class_defs,{ts & ts_selector_defs = ts_selector_defs},{ti & ti_var_heap = ti_var_heap},{cs & cs_error = cs_error}))
	where
		check_selectors :: !Index !{# FieldSymbol} !Index ![AType] !AType ![TypeVar] ![AttributeVar] ![ATypeVar] !*{#SelectorDef} !*VarHeap !*ErrorAdmin
			-> (!*{#SelectorDef}, !*VarHeap, !*ErrorAdmin)
		check_selectors field_nr fields rec_type_index sel_types rec_type st_vars st_attr_vars exi_vars selector_defs var_heap error
			| field_nr < size fields
				# {fs_index} = fields.[field_nr]
				# (sel_def, selector_defs) = selector_defs![fs_index]
				  [sel_type : sel_types] = sel_types
				# (sel_type, (sel_vars, sel_attr_vars)) = lift_quantifier sel_type (st_vars, st_attr_vars)
				# (st_attr_env, error) = addToAttributeEnviron sel_type.at_attribute rec_type.at_attribute [] error
				# (new_type_ptr, var_heap) = newPtr VI_Empty var_heap
				  sd_type = { sel_def.sd_type &  st_arity = 1, st_args = [rec_type], st_result = sel_type,
				  				st_vars = sel_vars, st_attr_vars = sel_attr_vars, st_attr_env = st_attr_env }
				  selector_defs = { selector_defs & [fs_index] = { sel_def & sd_type = sd_type, sd_field_nr = field_nr, sd_type_index = rec_type_index,
				  									sd_type_ptr = new_type_ptr, sd_exi_vars = exi_vars  } }
				= check_selectors (inc field_nr) fields rec_type_index sel_types  rec_type st_vars st_attr_vars exi_vars selector_defs var_heap error
				= (selector_defs, var_heap, error)
		where
			lift_quantifier at=:{at_type = TFA vars type} (type_vars, attr_vars)
				= ({ at & at_type = type}, foldSt add_var_and_attr vars (type_vars, attr_vars))
			lift_quantifier at (type_vars, attr_vars)
				= (at, (type_vars, attr_vars))
				
			add_var_and_attr {atv_variable, atv_attribute} (type_vars, attr_vars)
				= ([atv_variable : type_vars], add_attr_var atv_attribute attr_vars)
	
			add_attr_var (TA_Var av) attr_vars
				= [av : attr_vars]
			add_attr_var attr attr_vars
				= attr_vars
	check_rhs_of_TypeDef {td_rhs = SynType type} _ cti (class_defs,ts,ti,cs)
		# (type, type_attr, (ts,ti,cs)) = bindTypes cti type (ts,ti,cs)
		= (SynType type, (class_defs,ts,ti,cs))
	check_rhs_of_TypeDef {td_ident,td_arity,td_args,td_rhs = td_rhs=:NewType {ds_index}} attr_vars cti=:{cti_module_index,cti_type_index,cti_lhs_attribute} class_defs_ts_ti_cs
		# type_lhs = { at_attribute = cti_lhs_attribute,
				  	   at_type = TA (MakeTypeSymbIdent { glob_object = cti_type_index, glob_module = cti_module_index } td_ident td_arity)
									[{at_attribute = atv_attribute,at_type = TV atv_variable} \\ {atv_variable, atv_attribute} <- td_args]}
		  class_defs_ts_ti_cs = bind_types_of_constructor cti -2 (atype_vars_to_type_vars td_args) attr_vars type_lhs ds_index class_defs_ts_ti_cs
		= (td_rhs, class_defs_ts_ti_cs)
	check_rhs_of_TypeDef {td_rhs = AbstractSynType properties type} _ cti (class_defs,ts,ti,cs)
		# (type, type_attr, (ts,ti,cs)) = bindTypes cti type (ts,ti,cs)
		= (AbstractSynType properties type, (class_defs,ts,ti,cs))
	check_rhs_of_TypeDef {td_ident,td_arity,td_args,td_rhs = td_rhs=:ExtensibleAlgType conses} attr_vars cti=:{cti_module_index,cti_type_index,cti_lhs_attribute} class_defs_ts_ti_cs
		# type_lhs = { at_attribute = cti_lhs_attribute,
				  	   at_type = TA (MakeTypeSymbIdent {glob_object = cti_type_index, glob_module = cti_module_index} td_ident td_arity)
									[{at_attribute = atv_attribute,at_type = TV atv_variable} \\ {atv_variable, atv_attribute} <- td_args]}
		  class_defs_ts_ti_cs = bind_types_of_constructors cti 0 (atype_vars_to_type_vars td_args) attr_vars type_lhs conses class_defs_ts_ti_cs
		= (td_rhs, class_defs_ts_ti_cs)
	check_rhs_of_TypeDef {td_ident,td_arity,td_args,td_rhs = td_rhs=:UncheckedAlgConses type_ext_ident conses} attr_vars cti=:{cti_module_index,cti_type_index,cti_lhs_attribute} class_defs_ts_ti_cs
		# (class_defs,ts,ti,cs) = class_defs_ts_ti_cs
		  (type_index, type_module, cs_symbol_table, ti_used_types) = retrieveTypeDefinition td_ident.id_info cti_module_index cs.cs_symbol_table ti.ti_used_types
		  ti & ti_used_types = ti_used_types
		  cs & cs_symbol_table = cs_symbol_table
		| type_index <> NotFound
		 	# class_defs_ts_ti_cs = (class_defs,ts,ti,cs)
			// to do check if ExtensibleAlgType
			# type_lhs = { at_attribute = cti_lhs_attribute,
					  	   at_type = TA (MakeTypeSymbIdent { glob_object = type_index, glob_module = type_module } td_ident td_arity)
										[{at_attribute = atv_attribute,at_type = TV atv_variable} \\ {atv_variable, atv_attribute} <- td_args]}
			  class_defs_ts_ti_cs = bind_types_of_added_constructors cti (atype_vars_to_type_vars td_args) attr_vars type_lhs conses class_defs_ts_ti_cs
			= (AlgConses conses {gi_module=type_module,gi_index=type_index}, class_defs_ts_ti_cs)
			# cs & cs_error = checkError td_ident "undefined" cs.cs_error
			= (td_rhs, (class_defs,ts,ti,cs))
	check_rhs_of_TypeDef {td_rhs} _ _ class_defs_ts_ti_cs
		= (td_rhs, class_defs_ts_ti_cs)

	atype_vars_to_type_vars atype_vars
		= [atv_variable \\ {atv_variable} <- atype_vars]

	bind_types_of_constructors :: !CurrentTypeInfo !Index ![TypeVar] ![AttributeVar] !AType ![DefinedSymbol]
								!(!v:{#ClassDef},!*TypeSymbols,!*TypeInfo,!*CheckState)
							->   (!v:{#ClassDef},!*TypeSymbols,!*TypeInfo,!*CheckState)
	bind_types_of_constructors cti cons_number free_vars free_attrs type_lhs [{ds_arity,ds_ident,ds_index}:conses] (class_defs,ts,ti,cs)
		# (ts,cs) = if (ds_arity>32)
						(constructor_has_too_many_arguments ds_index ds_ident ds_arity ts cs)
						(ts,cs);
		# class_defs_ts_ti_cs = bind_types_of_constructor cti cons_number free_vars free_attrs type_lhs ds_index (class_defs,ts,ti,cs)
		= bind_types_of_constructors cti (inc cons_number) free_vars free_attrs type_lhs conses class_defs_ts_ti_cs
	bind_types_of_constructors _ _ _ _ _ [] class_defs_ts_ti_cs
		= class_defs_ts_ti_cs

	bind_types_of_added_constructors :: !CurrentTypeInfo ![TypeVar] ![AttributeVar] !AType ![DefinedSymbol]
								!(!v:{#ClassDef},!*TypeSymbols,!*TypeInfo,!*CheckState)
							->   (!v:{#ClassDef},!*TypeSymbols,!*TypeInfo,!*CheckState)
	bind_types_of_added_constructors cti free_vars free_attrs type_lhs [{ds_arity,ds_ident,ds_index}:conses] (class_defs,ts,ti,cs)
		# (ts,cs) = if (ds_arity>32)
						(constructor_has_too_many_arguments ds_index ds_ident ds_arity ts cs)
						(ts,cs);
		# class_defs_ts_ti_cs = bind_types_of_constructor cti -3 free_vars free_attrs type_lhs ds_index (class_defs,ts,ti,cs)
		= bind_types_of_added_constructors cti free_vars free_attrs type_lhs conses class_defs_ts_ti_cs
	bind_types_of_added_constructors _ _ _ _ [] class_defs_ts_ti_cs
		= class_defs_ts_ti_cs

	constructor_has_too_many_arguments ds_index ds_ident ds_arity ts cs
		# (cons_pos,ts2) = ts!ts_cons_defs.[ds_index].cons_pos
		= (ts2, {cs & cs_error = checkErrorWithPosition ds_ident cons_pos ("Constructor has too many arguments ("+++toString ds_arity+++", 32 are allowed)") cs.cs_error})

	bind_types_of_constructor :: !CurrentTypeInfo !Index ![TypeVar] ![AttributeVar] !AType !Index
					!(!v:{#ClassDef},!*TypeSymbols,!*TypeInfo,!*CheckState)
				->	 (!v:{#ClassDef},!*TypeSymbols,!*TypeInfo,!*CheckState)
	bind_types_of_constructor cti=:{cti_lhs_attribute} cons_number free_vars free_attrs type_lhs cons_index (class_defs, ts, ti=:{ti_type_heaps}, cs)
		# (cons_def, ts) = ts!ts_cons_defs.[cons_index]
		# (exi_vars, (ti_type_heaps, cs))
		  		= addExistentionalTypeVariablesToSymbolTable cti_lhs_attribute cons_def.cons_exi_vars ti_type_heaps cs
		  (st_args, st_attr_env,class_defs,(ts, ti, cs))
		  		= bind_types_of_cons cons_def.cons_type.st_args cti free_vars [] class_defs (ts, {ti & ti_type_heaps = ti_type_heaps}, cs)
		  (st_context,class_defs,ts,ti,cs)
		  		= bind_context_of_cons cons_def.cons_type.st_context cti class_defs ts ti cs
		  symbol_table = removeAttributedTypeVarsFromSymbolTable cGlobalScope /* cOuterMostLevel */ exi_vars cs.cs_symbol_table
		  attr_vars = add_universal_attr_vars st_args free_attrs
		  cons_type = {cons_def.cons_type & st_vars = free_vars, st_args = st_args, st_result = type_lhs, st_context = st_context, st_attr_vars = attr_vars, st_attr_env = st_attr_env}
		  (new_type_ptr, ti_var_heap) = newPtr VI_Empty ti.ti_var_heap
		  cons_def = { cons_def & cons_type = cons_type, cons_number = cons_number, cons_type_index = cti.cti_type_index, cons_exi_vars = exi_vars,
		  						  cons_type_ptr = new_type_ptr }
		= (class_defs, {ts & ts_cons_defs.[cons_index] = cons_def}, {ti & ti_var_heap = ti_var_heap}, {cs & cs_symbol_table=symbol_table})
	where
		bind_types_of_cons :: ![AType] !CurrentTypeInfo ![TypeVar] ![AttrInequality] !v:{#ClassDef} !(!*TypeSymbols, !*TypeInfo, !*CheckState)
													 -> (![AType], ![AttrInequality],!v:{#ClassDef},!(!*TypeSymbols, !*TypeInfo, !*CheckState))
		bind_types_of_cons [] cti free_vars attr_env class_defs ts_ti_cs
			= ([], attr_env, class_defs, ts_ti_cs)
		bind_types_of_cons [type : types] cti free_vars attr_env class_defs ts_ti_cs
			# (types, attr_env, class_defs, ts_ti_cs)
					= bind_types_of_cons types cti free_vars attr_env class_defs ts_ti_cs
			  (type, type_attr, class_defs, (ts, ti, cs)) = bindArgAType cti type class_defs ts_ti_cs
			  (attr_env, cs_error) = addToAttributeEnviron type_attr cti.cti_lhs_attribute attr_env cs.cs_error
			= ([type : types], attr_env, class_defs, (ts, ti, {cs & cs_error = cs_error}))

		bind_context_of_cons [context=:{tc_class,tc_types,tc_var}:contexts] cti class_defs ts ti cs
			# (ok,tc_class,fun_dep_vars,class_defs,modules,cs=:{cs_error}) = check_context_class tc_class tc_types cti.cti_module_index class_defs ts.ts_modules cs
			// to do: use ok and fun_dep_vars
			  ts = {ts & ts_modules=modules}
			| cs_error.ea_ok
			 	# (tc_types, _, (ts,ti,cs)) = bindTypes cti tc_types (ts,ti,cs)
				  cs = check_context_types tc_class tc_types cs
				  (contexts,class_defs,ts,ti,cs) = bind_context_of_cons contexts cti class_defs ts ti cs
				= ([{context & tc_class=tc_class, tc_types=tc_types}:contexts],class_defs,ts,ti,cs)
				# (contexts,class_defs,ts,ti,cs) = bind_context_of_cons contexts cti class_defs ts ti cs
				= ([{context & tc_types = []}:contexts],class_defs,ts,ti,cs)
		bind_context_of_cons [] cti class_defs ts ti cs
			= ([],class_defs,ts,ti,cs)

		add_universal_attr_vars [] attr_vars
			= attr_vars
		add_universal_attr_vars [{at_type=TFA vars type}:types] attr_vars
			= add_universal_attr_vars types (add_attr_vars vars attr_vars)
		add_universal_attr_vars [{at_type=TFAC vars type contexts}:types] attr_vars
			= add_universal_attr_vars types (add_attr_vars vars attr_vars)
		add_universal_attr_vars [type:types] attr_vars
			= add_universal_attr_vars types attr_vars

		add_attr_vars vars attr_vars
			= foldSt add_attr_var vars attr_vars
		where
			add_attr_var {atv_attribute=TA_Var av=:{av_info_ptr}} attr_vars
				= [av : attr_vars]
			add_attr_var _ attr_vars
				= attr_vars

	retrieve_used_types symb_ptrs symbol_table
		= foldSt retrieve_used_type symb_ptrs ([], symbol_table)
	where
		retrieve_used_type symb_ptr (used_types, symbol_table)
			# (ste=:{ste_kind,ste_index}, symbol_table) = readPtr symb_ptr symbol_table
			# (orig_kind,used_types) = retrieve_used_types_of_ident ste_kind ste_index used_types 
			= (used_types, symbol_table <:= (symb_ptr, { ste & ste_kind = orig_kind }))

		retrieve_used_types_of_ident (STE_UsedType mod_index orig_kind) ste_index used_types
			# used_types = [{gi_module = mod_index, gi_index = ste_index} : used_types]
			= retrieve_used_types_of_ident orig_kind ste_index used_types
		retrieve_used_types_of_ident (STE_UsedQualifiedType mod_index decl_index orig_kind) ste_index used_types
			# used_types = [{gi_module = mod_index, gi_index = decl_index} : used_types]
			= retrieve_used_types_of_ident orig_kind ste_index used_types
		retrieve_used_types_of_ident orig_kind ste_index used_types
			= (orig_kind,used_types)

CS_Checked	:== 1
CS_Checking	:== 0

checkTypeDefs :: !Index !(Optional (CopiedDefinitions, Int))
		!*{#CheckedTypeDef} !*{#ConsDef} !*{#SelectorDef} !v:{#ClassDef} !*{#DclModule} !*Heaps !*CheckState
	-> (!*{#CheckedTypeDef},!*{#ConsDef},!*{#SelectorDef},!v:{#ClassDef},!*{#DclModule},!*Heaps,!*CheckState)
checkTypeDefs module_index opt_icl_info type_defs cons_defs selector_defs class_defs modules heaps=:{hp_type_heaps,hp_var_heap} cs
	#! nr_of_types = size type_defs
	#  ts = { ts_type_defs = type_defs, ts_cons_defs = cons_defs, ts_selector_defs = selector_defs, ts_modules = modules }
	   ti = { ti_type_heaps = hp_type_heaps, ti_var_heap = hp_var_heap, ti_used_types = [] }
	   (class_defs, {ts_type_defs,ts_cons_defs, ts_selector_defs, ts_modules}, {ti_var_heap,ti_type_heaps}, cs)
	  		= iFoldSt (check_type_def module_index opt_icl_info) 0 nr_of_types (class_defs, ts, ti, cs)
	= (ts_type_defs, ts_cons_defs, ts_selector_defs, class_defs, ts_modules, {heaps& hp_var_heap=ti_var_heap, hp_type_heaps=ti_type_heaps}, cs)
where
	check_type_def module_index opt_icl_info type_index (class_defs, ts, ti, cs)
		| has_to_be_checked module_index opt_icl_info type_index
			= checkTypeDef type_index module_index class_defs ts ti cs
			= (class_defs, ts, ti, cs)

	has_to_be_checked module_index No type_index
		= True
	has_to_be_checked module_index (Yes ({copied_type_defs}, n_cached_dcl_mods)) type_index
		= not (module_index < n_cached_dcl_mods && type_index < size copied_type_defs && copied_type_defs.[type_index])


::	OpenTypeInfo =
	{	oti_heaps		:: !.TypeHeaps
	,	oti_all_vars	:: ![TypeVar]
	,	oti_all_attrs	:: ![AttributeVar]
	,	oti_global_vars	:: ![TypeVar]
	}

::	OpenTypeSymbols =
	{	ots_type_defs	:: .{# CheckedTypeDef}
	,	ots_modules		:: .{# DclModule}
	}

determineAttributeVariable attr_var=:{av_ident=attr_name=:{id_info}} oti=:{oti_heaps,oti_all_attrs} symbol_table
	# (entry=:{ste_kind,ste_def_level}, symbol_table) = readPtr id_info symbol_table
	| ste_kind == STE_Empty || ste_def_level == cModuleScope
		#! (new_attr_ptr, th_attrs) = newPtr AVI_Empty oti_heaps.th_attrs
		# symbol_table = symbol_table <:= (id_info,{	ste_index = NoIndex, ste_kind = STE_TypeAttribute new_attr_ptr,
														ste_def_level = cGlobalScope, ste_previous = entry, ste_doc = No })
		  new_attr = { attr_var & av_info_ptr = new_attr_ptr}
		= (new_attr, { oti & oti_heaps = { oti_heaps & th_attrs = th_attrs }, oti_all_attrs = [new_attr : oti_all_attrs] }, symbol_table)
		# (STE_TypeAttribute attr_ptr) = ste_kind
		= ({ attr_var & av_info_ptr = attr_ptr}, oti, symbol_table)

::	DemandedAttributeKind = DAK_Ignore | DAK_Unique | DAK_None

instance toString DemandedAttributeKind where
	toString DAK_Ignore = "DAK_Ignore"
	toString DAK_Unique = "DAK_Unique"
	toString DAK_None = "DAK_None"

newAttribute :: !DemandedAttributeKind {#Char} TypeAttribute !*OpenTypeInfo !*CheckState -> (!TypeAttribute, !*OpenTypeInfo, !*CheckState)
newAttribute DAK_Ignore var_ident attr oti cs
	= case attr of
		TA_Multi
			-> (TA_Multi, oti, cs)
		TA_None
			-> (TA_Multi, oti, cs)
		_
			-> (TA_Multi, oti, { cs & cs_error = checkError var_ident "attribute not allowed" cs.cs_error })
newAttribute DAK_Unique var_ident new_attr  oti cs
	= case new_attr of
		TA_Unique
			-> (TA_Unique, oti, cs)
		TA_Multi
			-> (TA_Unique, oti, cs)
		TA_None
			-> (TA_Unique, oti, cs)
		_
			-> (TA_Unique, oti, { cs & cs_error = checkError var_ident "inconsistently attributed (2)" cs.cs_error })
newAttribute DAK_None var_ident (TA_Var attr_var) oti cs=:{cs_symbol_table}
	# (attr_var, oti, cs_symbol_table) = determineAttributeVariable attr_var oti cs_symbol_table
	= (TA_Var attr_var, oti, {cs & cs_symbol_table = cs_symbol_table})
newAttribute DAK_None var_ident TA_Anonymous oti=:{oti_heaps, oti_all_attrs} cs
	# (new_attr_ptr, th_attrs) = newPtr AVI_Empty oti_heaps.th_attrs
	  new_attr = {av_info_ptr = new_attr_ptr, av_ident = emptyIdent var_ident}
	= (TA_Var new_attr, {oti & oti_heaps = {oti_heaps & th_attrs = th_attrs}, oti_all_attrs = [new_attr : oti_all_attrs] }, cs)
newAttribute DAK_None var_ident TA_Unique oti cs
	= (TA_Unique, oti, cs)
newAttribute DAK_None var_ident attr oti cs
	= (TA_Multi, oti, cs)

getTypeDef :: !Index !Index !Index !u:{# CheckedTypeDef} !v:{# DclModule} -> (!CheckedTypeDef, !Index , !u:{# CheckedTypeDef}, !v:{# DclModule})
getTypeDef type_index type_module module_index type_defs modules
	| type_module == module_index
		# (type_def, type_defs) = type_defs![type_index]
		= (type_def, type_index, type_defs, modules)
		# ({dcl_common={com_type_defs}}, modules) = modules![type_module]
		  type_def = com_type_defs.[type_index]
		= (type_def, type_index, type_defs, modules)

checkArityOfType act_arity form_arity (SynType _)
	= form_arity == act_arity
checkArityOfType act_arity form_arity _
	= form_arity >= act_arity

checkAbstractType type_index (AbstractType _)			= type_index <> cPredefinedModuleIndex 
checkAbstractType type_index (AbstractSynType _ _)		= type_index <> cPredefinedModuleIndex 
checkAbstractType _ _									= False

getClassDef :: !Index !Index !Index !u:{# ClassDef} !v:{# DclModule} -> (!ClassDef, !Index , !u:{# ClassDef}, !v:{# DclModule})
getClassDef class_index type_module module_index class_defs modules
	| type_module == module_index
		# (class_def, class_defs) = class_defs![class_index]
		= (class_def, class_index, class_defs, modules)
		# ({dcl_common={com_class_defs}}, modules) = modules![type_module]
		  class_def = com_class_defs.[class_index]
		= (class_def, class_index, class_defs, modules)

checkTypeVar :: !Level !DemandedAttributeKind !TypeVar !TypeAttribute !(!*OpenTypeInfo, !*CheckState)
					-> (! TypeVar, !TypeAttribute, !(!*OpenTypeInfo, !*CheckState))
checkTypeVar scope dem_attr tv=:{tv_ident=var_ident=:{id_name,id_info}} tv_attr (oti, cs=:{cs_symbol_table})
	# (entry=:{ste_kind,ste_def_level},cs_symbol_table) = readPtr id_info cs_symbol_table
	| ste_kind == STE_Empty || ste_def_level == cModuleScope
		# (new_attr, oti=:{oti_heaps,oti_all_vars}, cs) = newAttribute dem_attr id_name tv_attr oti {cs & cs_symbol_table = cs_symbol_table}
		  (new_var_ptr, th_vars) = newPtr (TVI_AttrAndRefCount new_attr 1) oti_heaps.th_vars
		  new_var = { tv & tv_info_ptr = new_var_ptr }
		  entry = {ste_index = NoIndex, ste_kind = STE_TypeVariable new_var_ptr, ste_def_level = scope, ste_previous = entry, ste_doc = No}
		  cs & cs_symbol_table = writePtr id_info entry cs.cs_symbol_table
		= (new_var, new_attr, ({ oti & oti_heaps = { oti_heaps & th_vars = th_vars }, oti_all_vars = [new_var : oti_all_vars]}, cs))
		= case ste_kind of
			STE_TypeVariable tv_info_ptr
				# {oti_heaps} = oti
				  (tv_info, th_vars) = readPtr tv_info_ptr oti_heaps.th_vars
				  th_vars = incr_ref_count tv_info_ptr tv_info th_vars	
				  (var_attr, oti, cs) = check_attribute id_name dem_attr tv_info tv_attr {oti & oti_heaps = {oti_heaps & th_vars = th_vars}}
				  								{cs & cs_symbol_table = cs_symbol_table}
				-> ({tv & tv_info_ptr = tv_info_ptr}, var_attr, (oti, cs))
			STE_FunDepTypeVariable tv_info_ptr
				# {oti_heaps,oti_all_vars} = oti
				  (TVI_AttrAndRefCount attr ref_count, th_vars) = readPtr tv_info_ptr oti_heaps.th_vars
				  th_vars = writePtr tv_info_ptr (TVI_AttrAndRefCount attr (inc ref_count)) th_vars
				  entry & ste_kind = STE_TypeVariable tv_info_ptr, ste_def_level = scope
				  cs & cs_symbol_table = writePtr id_info entry cs_symbol_table
				  tv & tv_info_ptr=tv_info_ptr
				-> (tv, attr, ({oti & oti_heaps = {oti_heaps & th_vars = th_vars}, oti_all_vars = [tv : oti_all_vars]}, cs))
where
	incr_ref_count tv_info_ptr (TVI_AttrAndRefCount prev_attr ref_count) th_vars
		= th_vars <:= (tv_info_ptr, TVI_AttrAndRefCount prev_attr (inc ref_count))
	incr_ref_count tv_info_ptr _ th_vars
		= th_vars

	check_attribute var_ident DAK_Ignore (TVI_AttrAndRefCount prev_attr ref_count) this_attr oti cs=:{cs_error}
		= (TA_Multi, oti, cs)
	check_attribute var_ident dem_attr (TVI_AttrAndRefCount prev_attr _) this_attr oti cs=:{cs_error}
		# (new_attr, cs_error) = determine_attribute var_ident dem_attr this_attr cs_error
		= check_var_attribute prev_attr new_attr oti { cs & cs_error = cs_error }
	where					
		check_var_attribute (TA_Var old_var) (TA_Var new_var) oti cs=:{cs_symbol_table,cs_error}
			# (new_var, oti, cs_symbol_table) = determineAttributeVariable new_var oti cs_symbol_table
			| old_var.av_info_ptr == new_var.av_info_ptr
				= (TA_Var old_var, oti, { cs &  cs_symbol_table = cs_symbol_table })
				= (TA_Var old_var, oti, { cs &  cs_symbol_table = cs_symbol_table,
						cs_error = checkError new_var.av_ident "inconsistently attributed (3)" cs_error })
		check_var_attribute var_attr=:(TA_Var old_var) TA_Anonymous oti cs
			= (var_attr, oti, cs)
		check_var_attribute TA_Unique new_attr oti cs
			= case new_attr of
				TA_Unique
					-> (TA_Unique, oti, cs)
				_
					-> (TA_Unique, oti, { cs & cs_error = checkError var_ident "inconsistently attributed (4)" cs.cs_error })
		check_var_attribute TA_Multi new_attr oti cs
			= case new_attr of
				TA_Multi
					-> (TA_Multi, oti, cs)
				TA_None
					-> (TA_Multi, oti, cs)
				_
					-> (TA_Multi, oti, { cs & cs_error = checkError var_ident "inconsistently attributed (5)" cs.cs_error })
		check_var_attribute var_attr new_attr oti cs
			= (var_attr, oti, { cs & cs_error = checkError var_ident "inconsistently attributed (6)" cs.cs_error })// ---> (var_attr, new_attr)

		determine_attribute var_ident DAK_Unique new_attr error
			= case new_attr of
				 TA_Multi
				 	-> (TA_Unique, error)
				 TA_None
				 	-> (TA_Unique, error)
				 TA_Unique
				 	-> (TA_Unique, error)
				 _
				 	-> (TA_Unique, checkError var_ident "inconsistently attributed (1)" error)
		determine_attribute var_ident dem_attr TA_None error
			= (TA_Multi, error)
		determine_attribute var_ident dem_attr new_attr error
			= (new_attr, error)
	check_attribute var_ident dem_attr _ this_attr oti cs
		= (TA_Multi, oti, cs)

check_args_of_type_cons :: !Index !Int !DemandedAttributeKind ![AType] ![ATypeVar] !(!u:OpenTypeSymbols, !*OpenTypeInfo, !*CheckState)
	-> (![AType], !(!u:OpenTypeSymbols, !*OpenTypeInfo, !*CheckState))
check_args_of_type_cons mod_index scope dem_attr_kind [] _ cot_state
	= ([], cot_state)
check_args_of_type_cons mod_index scope dem_attr_kind [arg_type : arg_types] [ {atv_attribute} : td_args ] cot_state
	# (arg_type, cot_state) = checkOpenAType mod_index scope (new_demanded_attribute dem_attr_kind /* DAK_None */ atv_attribute) arg_type cot_state
	  (arg_types, cot_state) = check_args_of_type_cons mod_index scope dem_attr_kind arg_types td_args cot_state
	= ([arg_type : arg_types], cot_state)

new_demanded_attribute DAK_Ignore _
	= DAK_Ignore
new_demanded_attribute _ TA_Unique
	= DAK_Unique
new_demanded_attribute dem_attr_kind _
	= DAK_None /* dem_attr_kind */

checkOpenArgAType :: !Index !Int !DemandedAttributeKind !AType !(!u:OpenTypeSymbols, !*OpenTypeInfo, !*CheckState)
												   -> (!AType, !(!u:OpenTypeSymbols, !*OpenTypeInfo, !*CheckState))
checkOpenArgAType mod_index scope dem_attr atype=:{at_type = TFA vars type, at_attribute} (ots, oti, cs)
	# (vars, (oti, cs)) = add_universal_vars vars oti cs
	  (checked_type, (ots, oti, cs)) = checkOpenAType mod_index cRankTwoScope dem_attr { atype & at_type = type } (ots, oti, cs)
	  cs = {cs & cs_symbol_table = remove_universal_vars vars cs.cs_symbol_table}
	= ({checked_type & at_type = TFA vars checked_type.at_type }, (ots, oti, cs))
checkOpenArgAType mod_index scope dem_attr atype=:{at_type = TFAC vars type contexts, at_attribute} (ots, oti, cs)
	# cs = add_universal_vars_again vars cs
	  (checked_type, (ots, oti, cs)) = checkOpenAType mod_index cRankTwoScope dem_attr {atype & at_type = type} (ots, oti, cs)
	  cs = {cs & cs_symbol_table = remove_universal_vars vars cs.cs_symbol_table}
	= ({checked_type & at_type = TFAC vars checked_type.at_type contexts}, (ots, oti, cs))
checkOpenArgAType mod_index scope dem_attr type ots_oti_cs
	= checkOpenAType mod_index scope dem_attr type ots_oti_cs

checkOpenAType :: !Index !Int !DemandedAttributeKind !AType !(!u:OpenTypeSymbols, !*OpenTypeInfo, !*CheckState)
												-> (!AType, !(!u:OpenTypeSymbols, !*OpenTypeInfo, !*CheckState))
checkOpenAType mod_index scope dem_attr type=:{at_type = TV tv, at_attribute} (ots, oti, cs)
	# (tv, at_attribute, (oti, cs)) = checkTypeVar scope dem_attr tv at_attribute (oti, cs) 
	= ({ type & at_type = TV tv, at_attribute = at_attribute }, (ots, oti, cs))
checkOpenAType mod_index scope dem_attr type=:{at_type = GTV var_id=:{tv_ident={id_info}}, at_attribute} (ots, oti, cs)
	# (new_attr, oti=:{oti_heaps,oti_global_vars}, cs=:{cs_symbol_table}) = newAttribute dem_attr "GTV" at_attribute oti cs
	  (entry, cs_symbol_table) = readPtr id_info cs_symbol_table
	  (type_var, oti_global_vars, th_vars, entry) = retrieve_global_variable var_id entry oti_global_vars oti_heaps.th_vars
	= ({type & at_type = TV type_var, at_attribute = new_attr }, (ots, { oti & oti_heaps = { oti_heaps & th_vars = th_vars }, oti_global_vars = oti_global_vars },
								{ cs & cs_symbol_table = cs_symbol_table <:= (id_info, entry) }))
where
	retrieve_global_variable var entry=:{ste_kind = STE_Empty} global_vars var_heap
		# (new_var_ptr, var_heap) = newPtr TVI_Used var_heap
		  var = { var & tv_info_ptr = new_var_ptr }
		= (var, [var : global_vars], var_heap, 
				{ entry  & ste_kind = STE_TypeVariable new_var_ptr, ste_def_level = cModuleScope, ste_previous = entry }) 
	retrieve_global_variable var entry=:{ste_kind,ste_def_level, ste_previous} global_vars var_heap
		| ste_def_level == cModuleScope
			= case ste_kind of
				STE_TypeVariable glob_info_ptr
					# var = { var & tv_info_ptr = glob_info_ptr }
					  (var_info, var_heap) = readPtr glob_info_ptr var_heap
					-> case var_info of
						TVI_Empty
							-> (var, [var : global_vars], var_heap <:= (glob_info_ptr, TVI_Used), entry)
						TVI_Used
							-> (var, global_vars, var_heap, entry)
			# (var, global_vars, var_heap, ste_previous) = retrieve_global_variable var ste_previous global_vars var_heap
			= (var, global_vars, var_heap, { entry & ste_previous = ste_previous })
checkOpenAType mod_index scope dem_attr_kind type=:{ at_type=TA type_cons=:{type_ident=type_ident=:{id_name,id_info}} types, at_attribute}
		(ots=:{ots_type_defs,ots_modules}, oti, cs=:{cs_symbol_table,cs_x={x_check_dynamic_types}})
	# (entry, cs_symbol_table) = readPtr id_info cs_symbol_table
	  cs = { cs & cs_symbol_table = cs_symbol_table }
	  (type_index, type_module) = retrieveGlobalDefinition entry STE_Type mod_index
	| type_index <> NotFound
		# ({td_arity,td_args,td_attribute,td_rhs},type_index,ots_type_defs,ots_modules) = getTypeDef type_index type_module mod_index ots_type_defs ots_modules
		  ots = { ots & ots_type_defs = ots_type_defs, ots_modules = ots_modules }
		| x_check_dynamic_types && checkAbstractType type_module td_rhs
			= (type, (ots, oti, {cs & cs_error = checkError type_ident "(abstract type) not permitted in a dynamic type" cs.cs_error}))
			| checkArityOfType type_cons.type_arity td_arity td_rhs
				# type_cons = { type_cons & type_index = { glob_object = type_index, glob_module = type_module }}
				  (types, (ots, oti, cs)) = check_args_of_type_cons mod_index scope dem_attr_kind types td_args (ots, oti, cs)
				  (new_attr, oti, cs) = newAttribute (new_demanded_attribute dem_attr_kind td_attribute) id_name at_attribute oti cs
				= ({ type & at_type = TA type_cons types, at_attribute = new_attr } , (ots, oti, cs))
				= (type, (ots, oti, {cs & cs_error = checkError type_ident "used with wrong arity" cs.cs_error}))
		= (type, (ots, oti, {cs & cs_error = checkError type_ident "undefined" cs.cs_error}))
checkOpenAType mod_index scope dem_attr type=:{ at_type=TAS type_cons=:{type_ident=type_ident=:{id_name,id_info}} types strictness, at_attribute}
		(ots=:{ots_type_defs,ots_modules}, oti, cs=:{cs_symbol_table})
	# (entry, cs_symbol_table) = readPtr id_info cs_symbol_table
	  cs = { cs & cs_symbol_table = cs_symbol_table }
	  (type_index, type_module) = retrieveGlobalDefinition entry STE_Type mod_index
	| type_index <> NotFound
		# ({td_arity,td_args,td_attribute,td_rhs},type_index,ots_type_defs,ots_modules) = getTypeDef type_index type_module mod_index ots_type_defs ots_modules
		  ots = { ots & ots_type_defs = ots_type_defs, ots_modules = ots_modules }
		| checkArityOfType type_cons.type_arity td_arity td_rhs
			# type_cons = { type_cons & type_index = { glob_object = type_index, glob_module = type_module }}
			  (types, (ots, oti, cs)) = check_args_of_type_cons mod_index scope dem_attr types td_args (ots, oti, cs)
			  (new_attr, oti, cs) = newAttribute (new_demanded_attribute dem_attr td_attribute) id_name at_attribute oti cs
			= ({ type & at_type = TAS type_cons types strictness, at_attribute = new_attr} , (ots, oti, cs)) 
			= (type, (ots, oti, {cs & cs_error = checkError type_ident "used with wrong arity" cs.cs_error}))
		= (type, (ots, oti, {cs & cs_error = checkError type_ident "undefined" cs.cs_error}))
checkOpenAType mod_index scope dem_attr type=:{at_type = arg_type --> result_type, at_attribute} cot_state
	# (arg_type, cot_state) = checkOpenAType mod_index scope DAK_None arg_type cot_state
	  (result_type, (ots, oti, cs)) = checkOpenAType mod_index scope DAK_None result_type cot_state
	  (new_attr, oti, cs) = newAttribute dem_attr "-->" at_attribute oti cs
	= ({ type & at_type = arg_type --> result_type, at_attribute = new_attr }, (ots, oti, cs))
checkOpenAType mod_index scope dem_attr type=:{at_type = TArrow1 arg_type, at_attribute} cot_state
	# (arg_type, (ots, oti, cs)) = checkOpenAType mod_index scope DAK_None arg_type cot_state
	  (new_attr, oti, cs) = newAttribute dem_attr "TArrow1" at_attribute oti cs
	= ({ type & at_type = TArrow1 arg_type, at_attribute = new_attr }, (ots, oti, cs))
/*
checkOpenAType mod_index scope dem_attr type=:{at_type = CV tv :@: types, at_attribute} (ots, oti, cs)
	# (cons_var, _, (oti, cs)) = checkTypeVar scope DAK_None tv TA_Multi (oti, cs)
	  (types, (ots, oti, cs)) = mapSt (checkOpenAType mod_index scope DAK_None) types (ots, oti, cs)
	  (new_attr, oti, cs) = newAttribute dem_attr ":@:" at_attribute oti cs
	= ({ type & at_type = CV cons_var :@: types, at_attribute = new_attr }, (ots, oti, cs))
*/
checkOpenAType mod_index scope dem_attr type=:{at_type = CV tv :@: types, at_attribute} (ots, oti, cs)
	# (cons_var, var_attr, (oti, cs)) = checkTypeVar scope dem_attr tv at_attribute (oti, cs)
	  (types, (ots, oti, cs)) = mapSt (checkOpenAType mod_index scope DAK_None) types (ots, oti, cs)
	= ({ type & at_type = CV cons_var :@: types, at_attribute = var_attr }, (ots, oti, cs))
checkOpenAType mod_index scope dem_attr_kind type=:{ at_type=TQualifiedIdent module_id type_name types, at_attribute}
		(ots=:{ots_type_defs,ots_modules}, oti, cs=:{cs_symbol_table,cs_x={x_check_dynamic_types}})
	# (found,{decl_kind,decl_ident=type_ident,decl_index=type_index},cs) = search_qualified_ident module_id type_name TypeNameSpaceN cs
	| not found
		= (type, (ots, oti, cs))
		= case decl_kind of
			STE_Imported STE_Type type_module
				# id_name = type_name
				# type_cons = MakeNewTypeSymbIdent type_ident (length types)
				# ({td_arity,td_args,td_attribute,td_rhs},type_index,ots_type_defs,ots_modules) = getTypeDef type_index type_module mod_index ots_type_defs ots_modules
				  ots = { ots & ots_type_defs = ots_type_defs, ots_modules = ots_modules }
				| x_check_dynamic_types && checkAbstractType type_module td_rhs
					-> (type, (ots, oti, {cs & cs_error = checkError type_ident "(abstract type) not permitted in a dynamic type" cs.cs_error}))
				| checkArityOfType type_cons.type_arity td_arity td_rhs
					# type_cons = { type_cons & type_index = { glob_object = type_index, glob_module = type_module }}
					  (types, (ots, oti, cs)) = check_args_of_type_cons mod_index scope dem_attr_kind types td_args (ots, oti, cs)
					  (new_attr, oti, cs) = newAttribute (new_demanded_attribute dem_attr_kind td_attribute) id_name at_attribute oti cs
					-> ({ type & at_type = TA type_cons types, at_attribute = new_attr } , (ots, oti, cs))
					-> (type, (ots, oti, {cs & cs_error = checkError type_ident "used with wrong arity" cs.cs_error}))
			_
				-> (type, (ots, oti, {cs & cs_error = checkError ("'"+++module_id.id_name+++"'."+++type_name) "not imported" cs.cs_error}))
checkOpenAType mod_index scope dem_attr atype=:{at_type = TFA vars type} (ots, oti, cs)
	# cs = universal_quantifier_error vars cs
	= (atype, (ots, oti, cs))
checkOpenAType mod_index scope dem_attr atype=:{at_type = TFAC vars type contexts} (ots, oti, cs)
	# cs = universal_quantifier_error vars cs
	= (atype, (ots, oti, cs))
checkOpenAType mod_index scope dem_attr type=:{at_attribute} (ots, oti, cs)
	# (new_attr, oti, cs) = newAttribute dem_attr "." at_attribute oti cs
	= ({ type & at_attribute = new_attr}, (ots, oti, cs))

checkOpenTypes mod_index scope dem_attr types cot_state
	= mapSt (checkOpenType mod_index scope dem_attr) types cot_state

checkOpenType mod_index scope dem_attr type cot_state
	# ({at_type}, cot_state) = checkOpenAType mod_index scope dem_attr { at_type = type, at_attribute = TA_Multi } cot_state
	= (at_type, cot_state)

checkOpenArgATypes mod_index scope types cot_state
	= mapSt (checkOpenArgAType mod_index scope DAK_None) types cot_state

add_universal_vars vars oti cs
	= mapSt add_universal_var vars (oti, cs)
  where
	add_universal_var atv=:{atv_variable = tv=:{tv_ident={id_name,id_info}}, atv_attribute} (oti, cs=:{cs_symbol_table,cs_error})
		# (entry=:{ste_kind,ste_def_level},cs_symbol_table) = readPtr id_info cs_symbol_table
		| ste_kind == STE_Empty || ste_def_level < cRankTwoScope
			# (new_attr, oti=:{oti_heaps}, cs) = newAttribute DAK_None id_name atv_attribute oti {cs & cs_symbol_table = cs_symbol_table}
			  (new_var_ptr, th_vars) = newPtr (TVI_AttrAndRefCount new_attr 1) oti_heaps.th_vars
			  cs = {cs & cs_symbol_table = cs.cs_symbol_table <:= (id_info, {ste_index = NoIndex, ste_kind = STE_TypeVariable new_var_ptr,
																			 ste_def_level = cRankTwoScope, ste_previous = entry, ste_doc = No})}
			= ({atv & atv_variable = {tv & tv_info_ptr = new_var_ptr}, atv_attribute = new_attr}, 
					({oti & oti_heaps = {oti_heaps & th_vars = th_vars}}, cs))
			= (atv, (oti, {cs & cs_error = checkError id_name "type variable already defined" cs_error, cs_symbol_table = cs_symbol_table}))

add_universal_vars_again vars cs
	= foldSt add_universal_var_and_attribute_again vars cs
  where
	add_universal_var_and_attribute_again {atv_variable,atv_attribute=TA_Var {av_ident=attr_name=:{id_info},av_info_ptr}} cs=:{cs_symbol_table}
		# (entry=:{ste_kind,ste_def_level},cs_symbol_table) = readPtr id_info cs_symbol_table
		| ste_kind == STE_Empty || ste_def_level == cModuleScope
			# cs_symbol_table = cs_symbol_table <:= (id_info,
				{ste_index = NoIndex, ste_kind = STE_TypeAttribute av_info_ptr, ste_def_level = cGlobalScope, ste_previous = entry, ste_doc = No})
			= add_universal_var_again atv_variable {cs & cs_symbol_table=cs_symbol_table}
			= add_universal_var_again atv_variable {cs & cs_symbol_table=cs_symbol_table}
	add_universal_var_and_attribute_again {atv_variable} cs
		= add_universal_var_again atv_variable cs

	add_universal_var_again {tv_ident={id_name,id_info},tv_info_ptr} cs=:{cs_symbol_table}
		# (entry=:{ste_kind,ste_def_level},cs_symbol_table) = readPtr id_info cs_symbol_table
		| ste_kind == STE_Empty || ste_def_level < cRankTwoScope
			= {cs & cs_symbol_table = cs_symbol_table <:= (id_info,
						{ste_index = NoIndex, ste_kind = STE_TypeVariable tv_info_ptr, ste_def_level = cRankTwoScope, ste_previous = entry, ste_doc = No})}
			# cs_error = checkError id_name "type variable already defined" cs.cs_error
			= {cs & cs_symbol_table = cs_symbol_table,cs_error=cs_error}

remove_universal_vars vars symbol_table
	= foldSt remove_universal_var vars symbol_table
  where
	remove_universal_var {atv_variable = {tv_ident}, atv_attribute = TA_Var {av_ident}} cs_symbol_table
		= removeDefinitionFromSymbolTable cGlobalScope av_ident (removeDefinitionFromSymbolTable cRankTwoScope tv_ident cs_symbol_table)
	remove_universal_var {atv_variable = {tv_ident}} cs_symbol_table
		= removeDefinitionFromSymbolTable cRankTwoScope tv_ident cs_symbol_table

checkInstanceType :: !Index !GlobalIndex !ClassIdent !BITVECT
		!InstanceType !Specials !u:{# CheckedTypeDef} !v:{# ClassDef} !u:{# DclModule} !*TypeHeaps !*CheckState
	-> (!InstanceType,!Specials,!u:{# CheckedTypeDef},!v:{# ClassDef},!u:{# DclModule},!*TypeHeaps,!*CheckState)
checkInstanceType mod_index ins_class_index ins_class_ident class_fun_dep_vars it=:{it_types,it_context} specials type_defs class_defs modules heaps cs
	# cs_error = check_fully_polymorphity it_types it_context cs.cs_error
	  ots = { ots_type_defs = type_defs, ots_modules = modules }
	  oti = { oti_heaps = heaps, oti_all_vars = [], oti_all_attrs = [], oti_global_vars= [] }
	  (it_types, undefined_contexts_vars, (ots, oti=:{oti_all_vars = it_vars, oti_all_attrs = it_attr_vars}, cs))
		= check_instance_type class_fun_dep_vars mod_index it_types (ots, oti, {cs & cs_error = cs_error})
	  (heaps, cs) = check_linearity_of_type_vars it_vars oti.oti_heaps cs
	  oti = { oti &  oti_all_vars = [], oti_all_attrs = [], oti_heaps = heaps }
	  (it_context, type_context_vars, type_defs, class_defs, modules, heaps, cs)
		= checkInstanceTypeContexts it_context it_vars it_types undefined_contexts_vars mod_index class_defs ots oti cs
	  it_vars = case type_context_vars of
	  				[] -> it_vars
	  				_ -> it_vars ++ type_context_vars
	  cs_error = foldSt (compare_context_and_instance_types ins_class_index ins_class_ident it_types) it_context cs.cs_error
	  (specials, cs) = checkSpecialTypeVars specials { cs & cs_error = cs_error }
	  cs_symbol_table = removeVariablesFromSymbolTable cGlobalScope it_vars cs.cs_symbol_table
	  cs_symbol_table = removeAttributesFromSymbolTable it_attr_vars cs_symbol_table
	  (specials, type_defs, modules, heaps, cs) = checkSpecialTypes mod_index specials type_defs modules heaps { cs & cs_symbol_table = cs_symbol_table }
	= ({it & it_vars = it_vars, it_types = it_types, it_attr_vars = it_attr_vars, it_context = it_context },
	    	specials, type_defs, class_defs, modules, heaps, cs)
  where
	check_fully_polymorphity it_types it_context cs_error
		| all is_type_var it_types && not (isEmpty it_context)
			= IF_ALLOW_NON_LINEAR_AND_OVERLAPPING_INSTANCES
				cs_error
				(checkError "context restriction not allowed for fully polymorph instance" "" cs_error)
		= cs_error
	  where
		is_type_var (TV _) = True
		is_type_var _ = False

	check_instance_type :: Int Int [Type] *(u:OpenTypeSymbols,*OpenTypeInfo,*CheckState) -> ([Type],[[TypeVar]],(u:OpenTypeSymbols,*OpenTypeInfo,*CheckState))
	check_instance_type 0 mod_index it_types ots_oti_cs
		# (it_types, ots_oti_cs) = checkOpenTypes mod_index cGlobalScope DAK_None it_types ots_oti_cs
		= (it_types, [], ots_oti_cs)
	check_instance_type fun_dep_vars mod_index it_types ots_oti_cs
		# (it_types, (ots, oti, cs)) = check_fun_non_dep_types it_types fun_dep_vars DAK_None mod_index ots_oti_cs
		  (free_non_dep_vars,oti) = oti!oti_all_vars
		  oti & oti_all_vars = []
		  (it_types, (ots, oti, cs)) = check_fun_dep_types it_types fun_dep_vars DAK_None mod_index (ots, oti, cs)
		  (free_dep_vars,oti) = oti!oti_all_vars
		  cs = mark_free_vars free_dep_vars cs
		  oti & oti_all_vars = free_non_dep_vars
		  free_dep_vars = if (isEmpty free_dep_vars) [] [free_dep_vars]
		= (it_types, free_dep_vars, (ots, oti, cs))

	check_linearity_of_type_vars vars heaps=:{th_vars} cs=:{cs_error}
		# (th_vars, cs_error) = foldSt check_linearity vars (th_vars, cs_error)
		= ({heaps & th_vars = th_vars}, {cs & cs_error = cs_error})
	where
		check_linearity {tv_ident, tv_info_ptr} (th_vars, error)
			# (TVI_AttrAndRefCount prev_attr ref_count, th_vars) = readPtr tv_info_ptr th_vars
			| ref_count > 1
				= IF_ALLOW_NON_LINEAR_AND_OVERLAPPING_INSTANCES
					(th_vars, error)
					(th_vars, checkError tv_ident ": this type variable occurs more than once in an instance type" error)
				= (th_vars, error)

	compare_context_and_instance_types ins_class_index ins_class_ident it_types {tc_class=TCGeneric _, tc_types} cs_error
		= cs_error
	compare_context_and_instance_types ins_class_index ins_class_ident it_types {tc_class=TCClass clazz, tc_types} cs_error
		| ins_class_index.gi_module<>clazz.glob_module || ins_class_index.gi_index<>clazz.glob_object.ds_index
			= cs_error
		# are_equal
				= fold2St compare_context_and_instance_type it_types tc_types True
		| are_equal
			= checkError ins_class_ident.ci_ident "context restriction equals instance type" cs_error
		= cs_error
	  where
		compare_context_and_instance_type (TA {type_index=ti1} _) (TA {type_index=ti2} _) are_equal_accu
			= ti1==ti2 && are_equal_accu
		compare_context_and_instance_type (TA {type_index=ti1} _) (TAS {type_index=ti2} _ _) are_equal_accu
			= ti1==ti2 && are_equal_accu
		compare_context_and_instance_type (TAS {type_index=ti1} _ _) (TA {type_index=ti2} _) are_equal_accu
			= ti1==ti2 && are_equal_accu
		compare_context_and_instance_type (TAS {type_index=ti1} _ _) (TAS {type_index=ti2} _ _) are_equal_accu
			= ti1==ti2 && are_equal_accu
		compare_context_and_instance_type (_ --> _) (_ --> _) are_equal_accu
			= are_equal_accu
		compare_context_and_instance_type (TB bt1) (TB bt2) are_equal_accu
			= bt1==bt2 && are_equal_accu
		compare_context_and_instance_type (TV tv1) (TV tv2) are_equal_accu
			= tv1==tv2 && are_equal_accu
		compare_context_and_instance_type (CV tv1 :@: _) (CV tv2 :@: _) are_equal_accu
			= tv1==tv2 && are_equal_accu
		compare_context_and_instance_type TArrow TArrow are_equal_accu
			= are_equal_accu	
		compare_context_and_instance_type (TArrow1 _) (TArrow1 _) are_equal_accu
			= are_equal_accu	
		compare_context_and_instance_type _ _ are_equal_accu
			= False

checkFunctionType :: !Index !SymbolType !FunSpecials !u:{#CheckedTypeDef} !v:{#ClassDef} !u:{#DclModule} !*TypeHeaps !*CheckState
						-> (!SymbolType,!FunSpecials,!u:{#CheckedTypeDef},!v:{#ClassDef},!u:{#DclModule},!*TypeHeaps,!*CheckState)
checkFunctionType mod_index st specials type_defs class_defs modules heaps cs
	= checkSymbolType True mod_index st specials type_defs class_defs modules heaps cs

checkMemberType :: !Index !SymbolType !u:{#CheckedTypeDef} !v:{#ClassDef} !u:{#DclModule} !*TypeHeaps !*CheckState
		  -> (!SymbolType,![ATypeVar],!u:{#CheckedTypeDef},!v:{#ClassDef},!u:{#DclModule},!*TypeHeaps,!*CheckState)
checkMemberType mod_index st type_defs class_defs modules heaps cs
	# {st_args,st_result,st_context,st_attr_env} = st
	  ots = {ots_type_defs = type_defs, ots_modules = modules}
	  oti = {oti_heaps = heaps, oti_all_vars = [], oti_all_attrs = [], oti_global_vars= []}
	  (st_args, cot_state) = checkOpenArgATypes mod_index cGlobalScope st_args (ots, oti, cs)
	  (st_result, (ots, oti=:{oti_all_vars = st_vars,oti_all_attrs = st_attr_vars,oti_global_vars}, cs))
	  	= checkOpenAType mod_index cGlobalScope DAK_None st_result cot_state	  
	  oti = {oti & oti_all_vars = [], oti_all_attrs = []}
	  (st_context, type_context_vars, type_defs, class_defs, modules, heaps, cs)
	  	= check_symbol_type_contexts False st_context mod_index class_defs ots oti cs

	  (atype_class_vars,th_vars) = add_attrs_to_class_vars (hd st_context).tc_types heaps.th_vars
	  heaps & th_vars = th_vars

	  st_vars = case type_context_vars of
	  				[] -> st_vars
	  				_ -> st_vars ++ type_context_vars
	  (st_attr_env, cs) = check_attr_inequalities st_attr_env cs
	  cs_symbol_table = removeVariablesFromSymbolTable cGlobalScope st_vars cs.cs_symbol_table
	  cs_symbol_table = removeAttributesFromSymbolTable st_attr_vars cs_symbol_table
	  cs & cs_symbol_table = cs_symbol_table
	  checked_st = {st & st_vars = st_vars, st_args = st_args, st_result = st_result, st_context = st_context,
	    					st_attr_vars = st_attr_vars, st_attr_env = st_attr_env }
	= (checked_st, atype_class_vars, type_defs, class_defs, modules, heaps, cs) 
	where
		add_attrs_to_class_vars [TV type_var=:{tv_info_ptr}:tv_type_vars] th_vars
			# (TVI_AttrAndRefCount attr _, th_vars) = readPtr tv_info_ptr th_vars
			# (atype_class_vars,th_vars) = add_attrs_to_class_vars tv_type_vars th_vars
			= ([{atv_attribute = attr, atv_variable = type_var}:atype_class_vars],th_vars)
		add_attrs_to_class_vars [] th_vars
			= ([],th_vars)

checkSymbolType :: !Bool !Index !SymbolType !FunSpecials !u:{#CheckedTypeDef} !v:{#ClassDef} !u:{#DclModule} !*TypeHeaps !*CheckState
							-> (!SymbolType,!FunSpecials,!u:{#CheckedTypeDef},!v:{#ClassDef},!u:{#DclModule},!*TypeHeaps,!*CheckState)
checkSymbolType is_function mod_index st=:{st_args,st_result,st_context,st_attr_env} specials type_defs class_defs modules heaps cs
	# ots = {ots_type_defs = type_defs, ots_modules = modules}
	  oti = {oti_heaps = heaps, oti_all_vars = [], oti_all_attrs = [], oti_global_vars= []}
	  (st_args,class_defs,ots,oti,cs) = check_argument_type_contexts st_args mod_index class_defs ots oti cs
	  (st_args, cot_state) = checkOpenArgATypes mod_index cGlobalScope st_args (ots, oti, cs)
	  (st_result, (ots, oti=:{oti_all_vars = st_vars,oti_all_attrs = st_attr_vars,oti_global_vars}, cs))
	  	= checkOpenAType mod_index cGlobalScope DAK_None st_result cot_state	  
	  oti = {oti & oti_all_vars = [], oti_all_attrs = []}
	  (st_context, type_context_vars, type_defs, class_defs, modules, heaps, cs)
	  	= check_symbol_type_contexts is_function st_context mod_index class_defs ots oti cs
	  st_vars = case type_context_vars of
	  				[] -> st_vars
	  				_ -> st_vars ++ type_context_vars
	  (st_attr_env, cs) = check_attr_inequalities st_attr_env cs
	  (specials, cs) = checkFunSpecialTypeVars specials cs 
	  cs_symbol_table = removeVariablesFromSymbolTable cGlobalScope st_vars cs.cs_symbol_table
	  cs_symbol_table = removeAttributesFromSymbolTable st_attr_vars cs_symbol_table
	  (specials, type_defs, modules, heaps, cs) = checkFunSpecialTypes mod_index specials type_defs modules heaps { cs & cs_symbol_table = cs_symbol_table }
	  checked_st = {st & st_vars = st_vars, st_args = st_args, st_result = st_result, st_context = st_context,
	    					st_attr_vars = st_attr_vars, st_attr_env = st_attr_env }
	= (checked_st, specials, type_defs, class_defs, modules, heaps, cs)
where
	check_argument_type_contexts [arg=:{at_type=TFAC vars type contexts}:args] mod_index class_defs ots oti cs
		# (vars, (oti, cs)) = add_universal_vars vars oti cs
		  (contexts, type_context_vars, type_defs, class_defs, modules, heaps, cs)
		  	= checkTypeContexts contexts [] [] mod_index class_defs ots {oti & oti_all_vars=[],oti_all_attrs=[],oti_global_vars=[]} cs
		  // to do: use type_context_vars
		  oti = {oti & oti_heaps=heaps}
		  ots = {ots_modules = modules, ots_type_defs = type_defs}
		  cs = {cs & cs_symbol_table = remove_universal_vars vars cs.cs_symbol_table}
		  arg = {arg & at_type = TFAC vars type contexts}
	 	  (args,class_defs,ots,oti,cs) = check_argument_type_contexts args mod_index class_defs ots oti cs
		= ([arg:args],class_defs,ots,oti,cs)
	check_argument_type_contexts [arg:args] mod_index class_defs ots oti cs
	 	# (args,class_defs,ots,oti,cs) = check_argument_type_contexts args mod_index class_defs ots oti cs
		= ([arg:args],class_defs,ots,oti,cs)
	check_argument_type_contexts [] mod_index class_defs ots oti cs
		= ([],class_defs,ots,oti,cs)

check_attr_inequalities st_attr_env cs
	= mapSt check_attr_inequality st_attr_env cs
where
	check_attr_inequality ineq=:{ai_demanded=ai_demanded=:{av_ident=dem_name},ai_offered=ai_offered=:{av_ident=off_name}} cs=:{cs_symbol_table,cs_error}
		# (dem_entry, cs_symbol_table) = readPtr dem_name.id_info cs_symbol_table
		# (found_dem_attr, dem_attr_ptr) = retrieve_attribute dem_entry
		| found_dem_attr
		   	# (off_entry, cs_symbol_table) = readPtr off_name.id_info cs_symbol_table
			# (found_off_attr, off_attr_ptr) = retrieve_attribute off_entry
			| found_off_attr
				= ({ai_demanded = { ai_demanded & av_info_ptr = dem_attr_ptr }, ai_offered = { ai_offered & av_info_ptr = off_attr_ptr }},
						{ cs & cs_symbol_table = cs_symbol_table })
				= (ineq, { cs & cs_error = checkError off_name "attribute variable undefined" cs_error, cs_symbol_table = cs_symbol_table })
			= (ineq, { cs & cs_error = checkError dem_name "attribute variable undefined" cs_error, cs_symbol_table = cs_symbol_table })
	where
		retrieve_attribute {ste_kind = STE_TypeAttribute attr_ptr, ste_def_level, ste_index}
			| ste_def_level == cGlobalScope
				= (True, attr_ptr)
		retrieve_attribute entry
			= (False, abort "no attribute")

check_symbol_type_contexts is_function st_context mod_index class_defs ots oti cs
	| is_function
	 	= checkTypeContexts st_context [] [] mod_index class_defs ots oti cs
		= check_member_contexts st_context mod_index class_defs ots oti cs
where
	// AA generic members do not have a context at the moment of checking
	check_member_contexts [] mod_index class_defs ots oti cs
		= checkTypeContexts [] [] [] mod_index class_defs ots oti cs
	check_member_contexts [tc : tcs] mod_index class_defs ots oti cs
		# (tc, type_context_vars, (class_defs, ots, oti, cs)) = checkTypeContext mod_index tc (class_defs, ots, oti, cs)
          cs_symbol_table = cs.cs_symbol_table
		  (tcs, type_context_vars, type_defs, class_defs, modules, heaps, cs) = checkTypeContexts tcs type_context_vars [] mod_index class_defs ots oti {cs & cs_symbol_table = cs_symbol_table}
          cs & cs_symbol_table = removeVariablesFromSymbolTable cGlobalScope [ tv \\ (TV tv) <- tc.tc_types] cs.cs_symbol_table
		= ([tc : tcs], type_context_vars, type_defs, class_defs, modules, heaps, cs)

NewEntry symbol_table symb_ptr def_kind def_index level previous :==
	 symbol_table <:= (symb_ptr,{  ste_kind = def_kind, ste_index = def_index, ste_def_level = level, ste_previous = previous, ste_doc = No })

checkSuperClasses :: ![TypeVar] ![TypeContext] !Index !u:{# CheckedTypeDef} !v:{# ClassDef} !u:{# DclModule} !*TypeHeaps !*CheckState
	-> (![TypeVar], ![TypeContext], !u:{#CheckedTypeDef}, !v:{# ClassDef}, !u:{# DclModule}, !*TypeHeaps, !*CheckState)
checkSuperClasses class_args class_contexts mod_index type_defs class_defs modules heaps=:{th_vars} cs=:{cs_symbol_table,cs_error}
	# (rev_class_args, cs_symbol_table, th_vars, cs_error)
			= foldSt add_variable_to_symbol_table class_args ([], cs_symbol_table, th_vars, cs_error)
	  cs = {cs & cs_symbol_table = cs_symbol_table, cs_error = cs_error }
	  ots = { ots_modules = modules, ots_type_defs = type_defs }
	  oti = { oti_heaps = { heaps & th_vars = th_vars }, oti_all_vars = [], oti_all_attrs = [], oti_global_vars = [] }
	  (class_contexts, _, type_defs, class_defs, modules, type_heaps, cs)
		  		= checkTypeContexts class_contexts [] [] mod_index class_defs ots oti cs
	  (class_args, cs_symbol_table) = retrieve_variables_from_symbol_table rev_class_args [] cs.cs_symbol_table
	= (class_args, class_contexts, type_defs, class_defs, modules, type_heaps, {cs & cs_symbol_table = cs_symbol_table})
where
	add_variable_to_symbol_table :: !TypeVar !(![TypeVar], !*SymbolTable, !*TypeVarHeap, !*ErrorAdmin)
		-> (![TypeVar],!*SymbolTable,!*TypeVarHeap,!*ErrorAdmin)
	add_variable_to_symbol_table tv=:{tv_ident={id_name,id_info}} (rev_class_args, symbol_table, th_vars, error)
	  	# (entry, symbol_table) = readPtr id_info symbol_table
		| entry.ste_kind == STE_Empty || entry.ste_def_level < cGlobalScope
			# (new_var_ptr, th_vars) = newPtr TVI_Empty th_vars
			# symbol_table = NewEntry symbol_table id_info (STE_TypeVariable new_var_ptr) NoIndex cGlobalScope entry
			= ([{ tv & tv_info_ptr = new_var_ptr} : rev_class_args], symbol_table, th_vars, error)
			= (rev_class_args, symbol_table, th_vars, checkError id_name "(variable) already defined" error)

	retrieve_variables_from_symbol_table :: ![TypeVar] ![TypeVar] !*SymbolTable -> (![TypeVar],!*SymbolTable)
	retrieve_variables_from_symbol_table [var=:{tv_ident={id_name,id_info}} : vars] class_args symbol_table
		# (entry, symbol_table) = readPtr id_info symbol_table
		= retrieve_variables_from_symbol_table vars [var : class_args] (symbol_table <:= (id_info,entry.ste_previous))
	retrieve_variables_from_symbol_table [] class_args symbol_table
		= (class_args, symbol_table)

check_type_context ::  !Index !TypeContext ![TypeVar] !(!v:{#ClassDef}, !u:OpenTypeSymbols, !*OpenTypeInfo, !*CheckState)
	-> (!TypeContext,![TypeVar],![TypeVar],![TypeVar],!(!v:{#ClassDef}, !u:OpenTypeSymbols, !*OpenTypeInfo, !*CheckState))
check_type_context mod_index tc=:{tc_class, tc_types} fun_dep_determined_vars (class_defs, ots, oti, cs)
	# (ok, tc_class, fun_dep_vars, class_defs, modules, cs=:{cs_error}) = check_context_class tc_class tc_types mod_index class_defs ots.ots_modules cs
	# ots & ots_modules = modules
	| ok
		| fun_dep_vars==0
			# (tc_types, (ots, oti, cs)) = checkOpenTypes mod_index cGlobalScope DAK_Ignore tc_types (ots, oti, cs)
			  tc & tc_class = tc_class, tc_types = tc_types
			  cs = check_context_types tc_class tc_types cs
			  (free_vars,oti) = oti!oti_all_vars
			  cs = mark_free_vars free_vars cs
			  oti & oti_all_vars = []
			= (tc, free_vars, [], fun_dep_determined_vars, (class_defs, ots, oti, cs))
			# (tc_types, (ots, oti, cs)) = check_fun_non_dep_types tc_types fun_dep_vars DAK_Ignore mod_index (ots, oti, cs)
			  (free_non_dep_vars,oti) = oti!oti_all_vars
			| isEmpty free_non_dep_vars
				# (tc_types, (ots, oti, cs)) = check_fun_dep_types tc_types fun_dep_vars DAK_Ignore mod_index (ots, oti, cs)
				  tc & tc_class = tc_class, tc_types = tc_types
				  cs = check_fun_dep_context_types tc_class tc_types fun_dep_vars cs
				  (new_fun_dep_determined_vars,oti) = oti!oti_all_vars
				  oti & oti_all_vars = []
				  fun_dep_determined_vars = fun_dep_determined_vars++new_fun_dep_determined_vars
				= (tc, [], new_fun_dep_determined_vars, fun_dep_determined_vars, (class_defs, ots, oti, cs))
				# cs = mark_free_vars free_non_dep_vars cs
				  oti & oti_all_vars = []
				  (tc_types, (ots, oti, cs)) = check_fun_dep_types tc_types fun_dep_vars DAK_Ignore mod_index (ots, oti, cs)
				  tc & tc_class = tc_class, tc_types = tc_types
				  cs = check_fun_dep_context_types tc_class tc_types fun_dep_vars cs
				  (free_dep_vars,oti) = oti!oti_all_vars
				  cs = mark_free_vars free_dep_vars cs
				  oti & oti_all_vars = []
				= (tc, free_non_dep_vars, free_dep_vars, fun_dep_determined_vars, (class_defs, ots, oti, cs))
		= ({tc & tc_types = []}, [], [], fun_dep_determined_vars, (class_defs, ots, oti, cs))

mark_free_vars :: ![TypeVar] !*CheckState -> *CheckState
mark_free_vars [{tv_ident={id_info}}:free_vars] cs=:{cs_symbol_table}
	# (entry=:{ste_kind},cs_symbol_table) = readPtr id_info cs_symbol_table
	= case ste_kind of
		STE_TypeVariable tv_info_ptr
			# entry & ste_kind = STE_FunDepTypeVariable tv_info_ptr
			# cs & cs_symbol_table = writePtr id_info entry cs_symbol_table
			-> mark_free_vars free_vars cs
mark_free_vars [] cs
  	= cs

checkTypeContext ::  !Index !TypeContext !(!v:{#ClassDef}, !u:OpenTypeSymbols, !*OpenTypeInfo, !*CheckState)
			 -> (!TypeContext,![TypeVar],!(!v:{#ClassDef}, !u:OpenTypeSymbols, !*OpenTypeInfo, !*CheckState))
checkTypeContext mod_index tc=:{tc_class, tc_types} (class_defs, ots, oti, cs)
	# (ok, tc_class, fun_dep_vars, class_defs, modules, cs=:{cs_error}) = check_context_class tc_class tc_types mod_index class_defs ots.ots_modules cs
	# ots & ots_modules = modules
	| ok
		| fun_dep_vars==0
			# (tc_types, (ots, oti, cs)) = checkOpenTypes mod_index cGlobalScope DAK_Ignore tc_types (ots, oti, cs)
			  tc & tc_class = tc_class, tc_types = tc_types
			  cs = check_context_types tc_class tc_types cs
			= (tc, [], (class_defs, ots, oti, cs))
			# (tc_types, (ots, oti, cs)) = check_fun_non_dep_types tc_types fun_dep_vars DAK_Ignore mod_index (ots, oti, cs)
			  (free_non_dep_vars,oti) = oti!oti_all_vars
			| isEmpty free_non_dep_vars
				# (tc_types, (ots, oti, cs)) = check_fun_dep_types tc_types fun_dep_vars DAK_Ignore mod_index (ots, oti, cs)
				  tc & tc_class = tc_class, tc_types = tc_types
				  cs = check_fun_dep_context_types tc_class tc_types fun_dep_vars cs
				  (new_fun_dep_determined_vars,oti) = oti!oti_all_vars
				  oti & oti_all_vars = []
				= (tc, new_fun_dep_determined_vars, (class_defs, ots, oti, cs))
				# (tc_types, (ots, oti, cs)) = check_fun_dep_types tc_types fun_dep_vars DAK_Ignore mod_index (ots, oti, cs)
				  tc & tc_class = tc_class, tc_types = tc_types
				  cs = check_fun_dep_context_types tc_class tc_types fun_dep_vars cs
				= (tc, [], (class_defs, ots, oti, cs))
		= ({tc & tc_types = []}, [], (class_defs, ots, oti, cs))

check_fun_non_dep_types [tc_type:tc_types] fun_dep_vars dem_attr mod_index cot_state
	| fun_dep_vars bitand 1==0
		# (tc_type,cot_state) = checkOpenType mod_index cGlobalScope dem_attr tc_type cot_state
		# (tc_types,cot_state) = check_fun_non_dep_types tc_types (fun_dep_vars>>1) dem_attr mod_index cot_state
		= ([tc_type:tc_types],cot_state)
		# (tc_types,cot_state) = check_fun_non_dep_types tc_types (fun_dep_vars>>1) dem_attr mod_index cot_state
		= ([tc_type:tc_types],cot_state)
check_fun_non_dep_types [] fun_dep_vars dem_attr mod_index cot_state
	= ([],cot_state)

check_fun_dep_types [tc_type:tc_types] fun_dep_vars dem_attr mod_index cot_state
	| fun_dep_vars bitand 1<>0
		# (tc_type,cot_state) = checkOpenType mod_index cGlobalScope dem_attr tc_type cot_state
		# (tc_types,cot_state) = check_fun_dep_types tc_types (fun_dep_vars>>1) dem_attr mod_index cot_state
		= ([tc_type:tc_types],cot_state)
		# (tc_types,cot_state) = check_fun_dep_types tc_types (fun_dep_vars>>1) dem_attr mod_index cot_state
		= ([tc_type:tc_types],cot_state)
check_fun_dep_types [] fun_dep_vars mod_index dem_attr cot_state
	= ([],cot_state)

check_no_global_type_vars [] cs
	= cs
check_no_global_type_vars [{tv_ident}:global_vars] cs=:{cs_error}
	# cs = {cs & cs_error = checkError tv_ident ": type variable with ^ only allowed in dynamic types" cs_error }
	= check_no_global_type_vars global_vars cs

checkInstanceTypeContexts :: ![TypeContext] ![TypeVar] ![Type] ![[TypeVar]] !Index !v:{#ClassDef} !u:OpenTypeSymbols !*OpenTypeInfo !*CheckState
	-> (![TypeContext], ![TypeVar], !u:{#CheckedTypeDef}, !v:{#ClassDef}, u:{#DclModule}, !*TypeHeaps, !*CheckState)
checkInstanceTypeContexts tcs instance_type_vars instance_type undefined_contexts_vars mod_index class_defs ots oti cs
	# (instance_type_ref_counts,instance_type_size,oti,cs)
		= zero_ref_counts_and_size_of_instance_type tcs instance_type_vars instance_type oti cs
	  (tcs, fun_dep_determined_vars, undefined_contexts_vars, fun_contexts_vars, (class_defs, {ots_modules,ots_type_defs}, oti, cs))
		= check_type_contexts tcs [] undefined_contexts_vars [] instance_type_vars instance_type_ref_counts instance_type_size mod_index (class_defs, ots, oti, cs)
	  (new_fun_dep_determined_vars,cs_symbol_table) = iterate_define_fun_contexts_dep_vars fun_contexts_vars [] cs.cs_symbol_table
	  cs & cs_symbol_table=cs_symbol_table
	  fun_dep_determined_vars = if (isEmpty new_fun_dep_determined_vars) fun_dep_determined_vars (fun_dep_determined_vars++reverse new_fun_dep_determined_vars)
	  (free_vars,cs) = collect_free_contexts_vars_error undefined_contexts_vars oti.oti_all_vars cs
	  cs = check_class_attributes oti.oti_all_attrs cs
	  cs = check_no_global_type_vars oti.oti_global_vars cs
	= (tcs, fun_dep_determined_vars ++ free_vars, ots_type_defs, class_defs, ots_modules, oti.oti_heaps, cs)
where
	zero_ref_counts_and_size_of_instance_type [] instance_type_vars instance_type oti cs
		= ([],0,oti,cs)
	zero_ref_counts_and_size_of_instance_type _ instance_type_vars instance_type oti cs
		# (instance_type_ref_counts,oti,cs) = zero_ref_counts instance_type_vars oti cs
		  instance_type_size = size_types instance_type 0
		= (instance_type_ref_counts,instance_type_size,oti,cs)

	check_type_contexts [tc:tcs] fun_dep_determined_vars undefined_contexts_vars fun_contexts_vars instance_type_vars instance_type_ref_counts instance_type_size mod_index (class_defs, ots, oti, cs)
		# (tc,free_vars,free_dep_vars,fun_dep_determined_vars,(class_defs, ots, oti, cs))
			= check_type_context mod_index tc fun_dep_determined_vars (class_defs, ots, oti, cs)
		  (undefined_contexts_vars,fun_contexts_vars)
			= update_undefined_and_fun_contexts_vars free_vars free_dep_vars undefined_contexts_vars fun_contexts_vars
		  (oti,cs) = check_and_zero_ref_counts instance_type_vars instance_type_ref_counts oti cs
		  cs = if (size_types tc.tc_types 0 > instance_type_size)
		  		{cs & cs_error = checkError (name_of_class tc.tc_class) "type constraint is too large" cs.cs_error}
		  		cs
		  	with
		  		name_of_class (TCClass {glob_object={ds_ident}}) = ds_ident.id_name
				name_of_class (TCGeneric {gtc_class={glob_object={ds_ident}}}) = ds_ident.id_name
				name_of_class (TCQualifiedIdent _ class_name) = class_name
		  (tcs,fun_dep_determined_vars,undefined_contexts_vars,fun_contexts_vars,class_defs_ots_oti_cs)
			= check_type_contexts tcs fun_dep_determined_vars undefined_contexts_vars fun_contexts_vars instance_type_vars instance_type_ref_counts instance_type_size mod_index (class_defs, ots, oti, cs)
		= ([tc:tcs],fun_dep_determined_vars,undefined_contexts_vars,fun_contexts_vars,class_defs_ots_oti_cs)
	check_type_contexts [] fun_dep_determined_vars undefined_contexts_vars fun_contexts_vars instance_type_vars instance_type_ref_counts instance_type_size mod_index class_defs_ots_oti_cs
		= ([],fun_dep_determined_vars,undefined_contexts_vars,fun_contexts_vars,class_defs_ots_oti_cs)

	zero_ref_counts [{tv_ident={id_info}}:free_vars] oti=:{oti_heaps} cs=:{cs_symbol_table}
		# (entry=:{ste_kind},cs_symbol_table) = readPtr id_info cs_symbol_table
		  cs & cs_symbol_table=cs_symbol_table
		  tv_info_ptr 
			= case ste_kind of
				STE_TypeVariable tv_info_ptr -> tv_info_ptr
				STE_FunDepTypeVariable tv_info_ptr -> tv_info_ptr
		  (TVI_AttrAndRefCount attr ref_count, th_vars) = readPtr tv_info_ptr oti_heaps.th_vars
		  th_vars = writePtr tv_info_ptr (TVI_AttrAndRefCount attr 0) th_vars
		  oti & oti_heaps = {oti_heaps & th_vars = th_vars}
		  (ref_counts,oti,cs) = zero_ref_counts free_vars oti cs
		= ([ref_count:ref_counts],oti,cs)
	zero_ref_counts [] oti cs
	  	= ([],oti,cs)

	check_and_zero_ref_counts [{tv_ident}:free_vars] [instance_type_ref_count:instance_type_ref_counts] oti=:{oti_heaps} cs=:{cs_symbol_table}
		# (entry=:{ste_kind},cs_symbol_table) = readPtr tv_ident.id_info cs_symbol_table
		  cs & cs_symbol_table=cs_symbol_table
		  tv_info_ptr 
			= case ste_kind of
				STE_TypeVariable tv_info_ptr -> tv_info_ptr
				STE_FunDepTypeVariable tv_info_ptr -> tv_info_ptr
		  (TVI_AttrAndRefCount attr ref_count, th_vars) = readPtr tv_info_ptr oti_heaps.th_vars
		  th_vars = writePtr tv_info_ptr (TVI_AttrAndRefCount attr 0) th_vars
		  oti & oti_heaps = {oti_heaps & th_vars = th_vars}
		| ref_count > instance_type_ref_count
			# cs & cs_error = checkError tv_ident "used too many times in type constraint" cs.cs_error
			= check_and_zero_ref_counts free_vars instance_type_ref_counts oti cs
			= check_and_zero_ref_counts free_vars instance_type_ref_counts oti cs
	check_and_zero_ref_counts [] [] oti cs
	  	= (oti,cs)

checkTypeContexts :: ![TypeContext] ![TypeVar] ![[TypeVar]] !Index !v:{# ClassDef} !u:OpenTypeSymbols !*OpenTypeInfo !*CheckState
	-> (![TypeContext], ![TypeVar], !u:{# CheckedTypeDef}, !v:{# ClassDef}, u:{# DclModule}, !*TypeHeaps, !*CheckState)
checkTypeContexts tcs fun_dep_determined_vars undefined_contexts_vars mod_index class_defs ots oti cs
	# (tcs, fun_dep_determined_vars, undefined_contexts_vars, fun_contexts_vars, (class_defs, {ots_modules,ots_type_defs}, oti, cs))
		= check_type_contexts tcs fun_dep_determined_vars undefined_contexts_vars [] mod_index (class_defs, ots, oti, cs)
	  (new_fun_dep_determined_vars,cs_symbol_table) = iterate_define_fun_contexts_dep_vars fun_contexts_vars [] cs.cs_symbol_table
	  cs & cs_symbol_table=cs_symbol_table
	  fun_dep_determined_vars = if (isEmpty new_fun_dep_determined_vars) fun_dep_determined_vars (fun_dep_determined_vars++reverse new_fun_dep_determined_vars)
	  (free_vars,cs) = collect_free_contexts_vars undefined_contexts_vars oti.oti_all_vars cs
	  cs = check_class_variables free_vars cs
	  cs = check_class_attributes oti.oti_all_attrs cs
	  cs = check_no_global_type_vars oti.oti_global_vars cs
	= (tcs, fun_dep_determined_vars, ots_type_defs, class_defs, ots_modules, oti.oti_heaps, cs)
where
	check_type_contexts [tc:tcs] fun_dep_determined_vars undefined_contexts_vars fun_contexts_vars mod_index class_defs_ots_oti_cs
		# (tc,free_vars,free_dep_vars,fun_dep_determined_vars,class_defs_ots_oti_cs)
			= check_type_context mod_index tc fun_dep_determined_vars class_defs_ots_oti_cs
		# (undefined_contexts_vars,fun_contexts_vars)
			= update_undefined_and_fun_contexts_vars free_vars free_dep_vars undefined_contexts_vars fun_contexts_vars
		# (tcs,fun_dep_determined_vars,undefined_contexts_vars,fun_contexts_vars,class_defs_ots_oti_cs)
			= check_type_contexts tcs fun_dep_determined_vars undefined_contexts_vars fun_contexts_vars mod_index class_defs_ots_oti_cs
		= ([tc:tcs],fun_dep_determined_vars,undefined_contexts_vars,fun_contexts_vars,class_defs_ots_oti_cs)
	check_type_contexts [] fun_dep_determined_vars undefined_contexts_vars fun_contexts_vars mod_index class_defs_ots_oti_cs
		= ([],fun_dep_determined_vars,undefined_contexts_vars,fun_contexts_vars,class_defs_ots_oti_cs)

update_undefined_and_fun_contexts_vars :: [TypeVar] [TypeVar] [[TypeVar]] [([TypeVar],[TypeVar])] -> (![[TypeVar]],![([TypeVar],[TypeVar])])
update_undefined_and_fun_contexts_vars [] free_dep_vars undefined_contexts_vars fun_contexts_vars
	= (undefined_contexts_vars,fun_contexts_vars)
update_undefined_and_fun_contexts_vars free_vars [] undefined_contexts_vars fun_contexts_vars
	= ([free_vars:undefined_contexts_vars],fun_contexts_vars)
update_undefined_and_fun_contexts_vars free_vars free_dep_vars undefined_contexts_vars fun_contexts_vars
	= ([free_vars:undefined_contexts_vars],[(free_vars,free_dep_vars) : fun_contexts_vars])

iterate_define_fun_contexts_dep_vars :: [([TypeVar],[TypeVar])] [TypeVar] *SymbolTable -> (![TypeVar],!*SymbolTable)
iterate_define_fun_contexts_dep_vars fun_contexts_vars new_fun_dep_determined_vars symbol_table
	# (repeat,fun_contexts_vars,new_fun_dep_determined_vars,symbol_table)
		= define_fun_contexts_dep_vars_pass fun_contexts_vars [] False new_fun_dep_determined_vars symbol_table
	| repeat
		= iterate_define_fun_contexts_dep_vars fun_contexts_vars new_fun_dep_determined_vars symbol_table
		= (new_fun_dep_determined_vars,symbol_table)
where
	define_fun_contexts_dep_vars_pass [fun_context_vars=:(non_dep_vars,dep_vars):fun_contexts_vars] new_fun_contexts_vars repeat fun_dep_determined_vars symbol_table
		# (all_non_dep_vars_defined,symbol_table) = all_vars_defined non_dep_vars symbol_table
		| all_non_dep_vars_defined
			# (repeat,fun_dep_determined_vars,symbol_table) = define_dep_vars dep_vars repeat fun_dep_determined_vars symbol_table 
			= define_fun_contexts_dep_vars_pass fun_contexts_vars new_fun_contexts_vars repeat fun_dep_determined_vars symbol_table
		# (all_dep_vars_defined,symbol_table) = all_vars_defined dep_vars symbol_table
		| all_dep_vars_defined
			= define_fun_contexts_dep_vars_pass fun_contexts_vars new_fun_contexts_vars repeat fun_dep_determined_vars symbol_table
			= define_fun_contexts_dep_vars_pass fun_contexts_vars [fun_context_vars:new_fun_contexts_vars] repeat fun_dep_determined_vars symbol_table
	define_fun_contexts_dep_vars_pass [] new_fun_contexts_vars repeat fun_dep_determined_vars symbol_table
		= (repeat,new_fun_contexts_vars,fun_dep_determined_vars,symbol_table)

	all_vars_defined [{tv_ident={id_info}}:vars] symbol_table
		# ({ste_kind},symbol_table) = readPtr id_info symbol_table
		= case ste_kind of
			STE_TypeVariable tv_info_ptr
				-> all_vars_defined vars symbol_table
			_
				-> (False,symbol_table)
	all_vars_defined [] symbol_table
	  	= (True,symbol_table)

	define_dep_vars [tv=:{tv_ident=tv_ident=:{id_info}}:dep_vars] repeat fun_dep_determined_vars symbol_table
		# (entry=:{ste_kind},symbol_table) = readPtr id_info symbol_table
		= case ste_kind of
			STE_TypeVariable tv_info_ptr
				-> define_dep_vars dep_vars repeat fun_dep_determined_vars symbol_table
			STE_FunDepTypeVariable tv_info_ptr
				# entry & ste_kind = STE_TypeVariable tv_info_ptr
				  symbol_table = writePtr id_info entry symbol_table
				  fun_dep_determined_vars = [tv : fun_dep_determined_vars]
				-> define_dep_vars dep_vars True fun_dep_determined_vars symbol_table
	define_dep_vars [] repeat fun_dep_determined_vars symbol_table
		= (repeat,fun_dep_determined_vars,symbol_table) 

collect_free_contexts_vars :: [[TypeVar]] [TypeVar] *CheckState -> (![TypeVar],!*CheckState)
collect_free_contexts_vars [undefined_context_vars:undefined_contexts_vars] free_vars cs
	# (free_vars,cs) = collect_free_context_vars undefined_context_vars free_vars cs
	= collect_free_contexts_vars undefined_contexts_vars free_vars cs
where
	collect_free_context_vars [tv=:{tv_ident={id_info}}:undefined_context_vars] free_vars cs=:{cs_symbol_table}
		# (entry=:{ste_kind},cs_symbol_table) = readPtr id_info cs_symbol_table
		= case ste_kind of
			STE_FunDepTypeVariable tv_info_ptr
				# entry & ste_kind = STE_TypeVariable tv_info_ptr
				# cs & cs_symbol_table = writePtr id_info entry cs_symbol_table
				-> collect_free_context_vars undefined_context_vars [tv:free_vars] cs
			STE_TypeVariable tv_info_ptr
				# cs & cs_symbol_table = cs_symbol_table
				-> collect_free_context_vars undefined_context_vars free_vars cs
	collect_free_context_vars [] free_vars cs
	  	= (free_vars,cs)
collect_free_contexts_vars [] free_vars cs
	= (free_vars,cs)

collect_free_contexts_vars_error :: [[TypeVar]] [TypeVar] *CheckState -> (![TypeVar],!*CheckState)
collect_free_contexts_vars_error [undefined_context_vars:undefined_contexts_vars] free_vars cs
	# (free_vars,cs) = collect_free_context_vars undefined_context_vars free_vars cs
	= collect_free_contexts_vars undefined_contexts_vars free_vars cs
where
	collect_free_context_vars [tv=:{tv_ident=tv_ident=:{id_info}}:undefined_context_vars] free_vars cs=:{cs_symbol_table}
		# (entry=:{ste_kind},cs_symbol_table) = readPtr id_info cs_symbol_table
		= case ste_kind of
			STE_FunDepTypeVariable tv_info_ptr
				# entry & ste_kind = STE_TypeVariable tv_info_ptr
				# cs & cs_symbol_table = writePtr id_info entry cs_symbol_table

				# cs & cs_error = checkWarning tv_ident.id_name ": coverage condition fails" cs.cs_error

				-> collect_free_context_vars undefined_context_vars [tv:free_vars] cs
			STE_TypeVariable tv_info_ptr
				# cs & cs_symbol_table = cs_symbol_table
				-> collect_free_context_vars undefined_context_vars free_vars cs
	collect_free_context_vars [] free_vars cs
	  	= (free_vars,cs)
collect_free_contexts_vars_error [] free_vars cs
	= (free_vars,cs)

check_class_variables :: ![TypeVar] !*CheckState -> *CheckState
check_class_variables class_variables cs
	= foldSt check_class_variable class_variables cs
where
	check_class_variable {tv_ident} cs=:{cs_symbol_table,cs_error}
		= { cs & cs_symbol_table	= removeDefinitionFromSymbolTable cGlobalScope tv_ident cs_symbol_table,
				 cs_error			= checkError tv_ident "wrongly used or not used at all" cs_error}

check_class_attributes :: ![AttributeVar] !*CheckState -> *CheckState
check_class_attributes class_attributes cs
	= foldSt check_class_attribute class_attributes cs
where
	check_class_attribute {av_ident} cs=:{cs_symbol_table,cs_error}
		= { cs & cs_symbol_table	= removeDefinitionFromSymbolTable cGlobalScope av_ident cs_symbol_table,
				 cs_error			= checkError av_ident "attribute variable in context undefined" cs_error}

checkDynamicTypes :: !Index ![ExprInfoPtr] !(Optional SymbolType)
		!u:{#CheckedTypeDef} !v:{#ClassDef} !u:{#DclModule} !*TypeHeaps !*ExpressionHeap !*CheckState
	-> (!u:{#CheckedTypeDef},!v:{#ClassDef},!u:{#DclModule},!*TypeHeaps,!*ExpressionHeap,!*CheckState)
checkDynamicTypes mod_index dyn_type_ptrs No type_defs class_defs modules type_heaps expr_heap cs
	# (type_defs, class_defs, modules, heaps, expr_heap, cs) = checkDynamics mod_index (inc cModuleScope) dyn_type_ptrs type_defs class_defs modules type_heaps expr_heap cs
	  (expr_heap, cs_symbol_table) = remove_global_type_variables_in_dynamics dyn_type_ptrs (expr_heap, cs.cs_symbol_table)
	= (type_defs, class_defs, modules, heaps, expr_heap, { cs & cs_symbol_table = cs_symbol_table })
where
	remove_global_type_variables_in_dynamics dyn_info_ptrs expr_heap_and_symbol_table
		= foldSt remove_global_type_variables_in_dynamic dyn_info_ptrs expr_heap_and_symbol_table
	where
		remove_global_type_variables_in_dynamic dyn_info_ptr (expr_heap, symbol_table)
			# (dyn_info, expr_heap) = readPtr dyn_info_ptr expr_heap
			= case dyn_info of
				EI_UnmarkedDynamic (Yes {dt_global_vars}) local_dynamics
					-> remove_global_type_variables_in_dynamics local_dynamics (expr_heap, remove_global_type_variables dt_global_vars symbol_table)
				EI_UnmarkedDynamic No local_dynamics
					-> remove_global_type_variables_in_dynamics local_dynamics (expr_heap, symbol_table)
				EI_DynamicTypeWithVars loc_type_vars {dt_global_vars} local_dynamics
					-> remove_global_type_variables_in_dynamics local_dynamics (expr_heap, remove_global_type_variables dt_global_vars symbol_table)

		remove_global_type_variables global_vars symbol_table
			= foldSt remove_global_type_variable global_vars symbol_table
		where		
			remove_global_type_variable {tv_ident=tv_ident=:{id_info}} symbol_table
				# (entry, symbol_table) = readPtr id_info symbol_table
				| entry.ste_kind == STE_Empty
					= symbol_table
					= symbol_table <:= (id_info, entry.ste_previous)
checkDynamicTypes mod_index dyn_type_ptrs (Yes {st_vars}) type_defs class_defs modules type_heaps expr_heap cs=:{cs_symbol_table}
	# (th_vars, cs_symbol_table) = foldSt add_type_variable_to_symbol_table st_vars (type_heaps.th_vars, cs_symbol_table)
	  (type_defs, class_defs, modules, heaps, expr_heap, cs)
	  	= checkDynamics mod_index (inc cModuleScope) dyn_type_ptrs type_defs class_defs modules
	  		{ type_heaps & th_vars = th_vars } expr_heap { cs & cs_symbol_table = cs_symbol_table }
	  cs_symbol_table =	removeVariablesFromSymbolTable cModuleScope st_vars cs.cs_symbol_table
	  (expr_heap, cs) = check_global_type_variables_in_dynamics dyn_type_ptrs (expr_heap, { cs & cs_symbol_table = cs_symbol_table })
	= (type_defs, class_defs, modules, heaps, expr_heap, cs) 
where
	add_type_variable_to_symbol_table {tv_ident={id_info},tv_info_ptr} (var_heap,symbol_table)
		# (entry, symbol_table) = readPtr id_info symbol_table
		= (	var_heap <:= (tv_info_ptr, TVI_Empty),
			symbol_table <:= (id_info, {ste_index = NoIndex, ste_kind = STE_TypeVariable tv_info_ptr,
									ste_def_level = cModuleScope, ste_previous = entry, ste_doc = No }))

	check_global_type_variables_in_dynamics dyn_info_ptrs expr_heap_and_cs
		= foldSt check_global_type_variables_in_dynamic dyn_info_ptrs expr_heap_and_cs
	where
		check_global_type_variables_in_dynamic dyn_info_ptr (expr_heap, cs)
			# (dyn_info, expr_heap) = readPtr dyn_info_ptr expr_heap
			= case dyn_info of
				EI_UnmarkedDynamic (Yes {dt_global_vars}) loc_dynamics
					-> check_global_type_variables_in_dynamics loc_dynamics (expr_heap, check_global_type_variables dt_global_vars cs)
				EI_UnmarkedDynamic No loc_dynamics
					-> check_global_type_variables_in_dynamics loc_dynamics (expr_heap, cs)
				EI_DynamicTypeWithVars loc_type_vars {dt_global_vars} loc_dynamics
					-> check_global_type_variables_in_dynamics loc_dynamics (expr_heap, check_global_type_variables dt_global_vars cs)

		check_global_type_variables global_vars cs
			= foldSt check_global_type_variable global_vars cs
		where		
			check_global_type_variable {tv_ident=tv_ident=:{id_info}} cs=:{cs_symbol_table, cs_error}
				# (entry, cs_symbol_table) = readPtr id_info cs_symbol_table
				| entry.ste_kind == STE_Empty
					= { cs & cs_symbol_table = cs_symbol_table }
					= { cs & cs_symbol_table = cs_symbol_table <:= (id_info, entry.ste_previous),
							 cs_error = checkError tv_ident.id_name "global type variable not used in type of the function" cs_error }

checkDynamics mod_index scope dyn_type_ptrs type_defs class_defs modules type_heaps expr_heap cs
	= foldSt (check_dynamic mod_index scope) dyn_type_ptrs (type_defs, class_defs, modules, type_heaps, expr_heap, cs)
where	
	check_dynamic mod_index scope dyn_info_ptr (type_defs, class_defs, modules, type_heaps, expr_heap, cs)
		# (dyn_info, expr_heap) = readPtr dyn_info_ptr expr_heap
		= case dyn_info of
			EI_UnmarkedDynamic opt_type loc_dynamics
				-> case opt_type of
					Yes dyn_type
						# (dyn_type, loc_type_vars, type_defs, class_defs, modules, type_heaps, cs)
							= check_dynamic_type_in_pattern/*check_dynamic_type_in_expression*/ mod_index scope dyn_type type_defs class_defs modules type_heaps cs
						| isEmpty loc_type_vars
							# expr_heap =  expr_heap <:= (dyn_info_ptr, EI_UnmarkedDynamic (Yes dyn_type) loc_dynamics)
				  		  	-> check_local_dynamics mod_index scope loc_dynamics type_defs class_defs modules type_heaps expr_heap cs
				  			# cs_symbol_table = removeVariablesFromSymbolTable scope loc_type_vars cs.cs_symbol_table
							  cs_error = checkError loc_type_vars "type variable(s) not defined" cs.cs_error
							  expr_heap = expr_heap <:= (dyn_info_ptr, EI_UnmarkedDynamic (Yes dyn_type) loc_dynamics)
							-> (type_defs, class_defs, modules, type_heaps, expr_heap, {cs & cs_error = cs_error, cs_symbol_table = cs_symbol_table})
					No
				  		 -> check_local_dynamics mod_index scope loc_dynamics type_defs class_defs modules type_heaps expr_heap cs
			EI_DynamicType dyn_type loc_dynamics
				# (dyn_type, loc_type_vars, type_defs, class_defs, modules, type_heaps, cs)
					= check_dynamic_type_in_pattern mod_index scope dyn_type type_defs class_defs modules type_heaps cs
				  (type_defs, class_defs, modules, type_heaps, expr_heap, cs)
				  	= check_local_dynamics mod_index scope loc_dynamics type_defs class_defs modules type_heaps expr_heap cs
				  cs_symbol_table = removeVariablesFromSymbolTable scope loc_type_vars cs.cs_symbol_table
				  expr_heap = expr_heap <:= (dyn_info_ptr, EI_DynamicTypeWithVars loc_type_vars dyn_type loc_dynamics)
				-> (type_defs, class_defs, modules, type_heaps, expr_heap, {cs & cs_symbol_table = cs_symbol_table}) 

	check_local_dynamics mod_index scope local_dynamics type_defs class_defs modules type_heaps expr_heap cs
		= foldSt (check_dynamic mod_index (inc scope)) local_dynamics (type_defs, class_defs, modules, type_heaps, expr_heap, cs)

	check_dynamic_type_in_expression mod_index scope dt=:{dt_uni_vars,dt_type,dt_contexts} type_defs class_defs modules type_heaps=:{th_vars} cs
		# (dt_uni_vars, (th_vars, cs)) = add_type_variables_to_symbol_table scope dt_uni_vars th_vars cs
		  ots = { ots_type_defs = type_defs, ots_modules = modules }
		  oti = { oti_heaps = { type_heaps & th_vars = th_vars }, oti_all_vars = [], oti_all_attrs = [], oti_global_vars = [] }

		  (contexts, type_defs, class_defs, modules, heaps, cs)
		  	= checkTypeContexts dt_contexts mod_index class_defs ots {oti & oti_all_vars=[],oti_all_attrs=[],oti_global_vars=[]} cs
		  oti = {oti & oti_heaps=heaps}
		  ots = {ots_modules = modules, ots_type_defs = type_defs}

		  (dt_type, ({ots_type_defs, ots_modules}, oti, cs))
		  		= checkOpenAType mod_index scope DAK_None dt_type (ots, oti, { cs & cs_x = {cs.cs_x & x_check_dynamic_types = True} })
		= check_dynamic_type_uniqueness dt_type dt_uni_vars contexts oti ots_type_defs ots_modules class_defs cs

	check_dynamic_type_in_pattern mod_index scope dt=:{dt_uni_vars,dt_type,dt_contexts} type_defs class_defs modules type_heaps=:{th_vars} cs
		# (dt_uni_vars, (th_vars, cs)) = add_type_variables_to_symbol_table scope dt_uni_vars th_vars cs
		  ots = { ots_type_defs = type_defs, ots_modules = modules }
		  oti = { oti_heaps = { type_heaps & th_vars = th_vars }, oti_all_vars = [], oti_all_attrs = [], oti_global_vars = [] }
		  (dt_type, (ots, oti, cs))
		  		= checkOpenAType mod_index scope DAK_None dt_type (ots, oti, { cs & cs_x = {cs.cs_x & x_check_dynamic_types = True} })

		  (contexts, type_context_vars, type_defs, class_defs, modules, heaps, cs)
		  	= checkTypeContexts dt_contexts [] [] mod_index class_defs ots {oti & oti_all_vars=[],oti_all_attrs=[],oti_global_vars=[]} cs
		  // to do: use type_context_vars
		  oti = {oti & oti_heaps=heaps}

		= check_dynamic_type_uniqueness dt_type dt_uni_vars contexts oti type_defs modules class_defs cs

	check_dynamic_type_uniqueness dt_type dt_uni_vars contexts {oti_heaps,oti_all_vars,oti_all_attrs, oti_global_vars} ots_type_defs ots_modules class_defs cs
		# cs = check_dynamic_uniqueness dt_type.at_attribute cs
		  cs = { cs & cs_x = {cs.cs_x & x_check_dynamic_types = False} }
		  th_vars = foldSt (\{tv_info_ptr} -> writePtr tv_info_ptr TVI_Empty) oti_global_vars oti_heaps.th_vars
	  	  cs_symbol_table = removeAttributedTypeVarsFromSymbolTable scope dt_uni_vars cs.cs_symbol_table
		  dt = { dt_uni_vars = dt_uni_vars, dt_global_vars = oti_global_vars, dt_type = dt_type, dt_contexts=contexts }
		| isEmpty oti_all_attrs
			= (dt, oti_all_vars, ots_type_defs, class_defs, ots_modules, {oti_heaps & th_vars = th_vars}, {cs & cs_symbol_table = cs_symbol_table})
			# cs_symbol_table = removeAttributesFromSymbolTable oti_all_attrs cs_symbol_table
			  cs_error = checkError (hd oti_all_attrs).av_ident "type attribute variable not allowed" cs.cs_error
			= (dt, oti_all_vars, ots_type_defs, class_defs, ots_modules, {oti_heaps & th_vars = th_vars}, {cs & cs_symbol_table = cs_symbol_table, cs_error = cs_error})
		where
			check_dynamic_uniqueness TA_None cs
				=	cs
			check_dynamic_uniqueness TA_Multi cs
				=	cs
			check_dynamic_uniqueness _ cs
				= {cs & cs_error = checkError "result type of dynamic must be non-unique " "" cs.cs_error}

	add_type_variables_to_symbol_table scope type_vars type_var_heap cs
		= mapSt (add_type_variable_to_symbol_table scope) type_vars (type_var_heap, cs)
	where
		add_type_variable_to_symbol_table :: !Level !ATypeVar !*(!*TypeVarHeap,!*CheckState) -> (!ATypeVar,!(!*TypeVarHeap, !*CheckState))
		add_type_variable_to_symbol_table scope atv=:{atv_variable=atv_variable=:{tv_ident}, atv_attribute} (type_var_heap, cs=:{cs_symbol_table,cs_error})
			#  var_info = tv_ident.id_info
			   (var_entry, cs_symbol_table) = readPtr var_info cs_symbol_table
			| var_entry.ste_kind == STE_Empty || scope < var_entry.ste_def_level
				#! (new_var_ptr, type_var_heap) = newPtr TVI_Empty type_var_heap
				# cs_symbol_table = cs_symbol_table <:=
					(var_info, {ste_index = NoIndex, ste_kind = STE_TypeVariable new_var_ptr, ste_def_level = scope, ste_previous = var_entry, ste_doc = No })
				= ({atv & atv_attribute = TA_Multi, atv_variable = { atv_variable & tv_info_ptr = new_var_ptr }}, (type_var_heap,
						{ cs & cs_symbol_table = cs_symbol_table, cs_error = check_attribute atv_attribute cs_error}))
				= (atv, (type_var_heap, { cs & cs_symbol_table = cs_symbol_table, cs_error = checkError tv_ident.id_name "type variable already defined" cs_error }))

		check_attribute TA_Unique error
			= error
		check_attribute TA_Multi error
			= error
		check_attribute TA_None error
			= error
		check_attribute attr error
			= checkError attr "attribute not allowed in type of dynamic" error

checkSpecialTypeVars :: !Specials !*CheckState -> (!Specials, !*CheckState)
checkSpecialTypeVars (SP_ParsedSubstitutions env) cs
	# (env, cs) = mapSt check_type_vars env cs
	= (SP_ParsedSubstitutions env, cs)
checkSpecialTypeVars SP_None cs
	= (SP_None, cs)

checkFunSpecialTypeVars :: !FunSpecials !*CheckState -> (!FunSpecials, !*CheckState)
checkFunSpecialTypeVars (FSP_ParsedSubstitutions env) cs
	# (env, cs) = mapSt check_type_vars env cs
	= (FSP_ParsedSubstitutions env, cs)
checkFunSpecialTypeVars FSP_None cs
	= (FSP_None, cs)

check_type_vars [] cs
	= ([],cs)
check_type_vars [bind:binds] cs
	# (bind,cs) = check_type_var bind binds cs
	# (binds,cs) = check_type_vars binds cs
	= ([bind:binds],cs)
where
	check_type_var bind=:{bind_dst=type_var=:{tv_ident={id_name,id_info}}} binds cs=:{cs_symbol_table,cs_error}
		# ({ste_kind,ste_def_level}, cs_symbol_table) = readPtr id_info cs_symbol_table
		| ste_kind <> STE_Empty && ste_def_level == cGlobalScope
			# (STE_TypeVariable tv_info_ptr) = ste_kind
			| id_info_occurs_in_list id_info binds
				= (bind, { cs & cs_symbol_table= cs_symbol_table, cs_error = checkError id_name "type variable is already defined" cs_error })
				= ({ bind & bind_dst = { type_var & tv_info_ptr = tv_info_ptr}}, { cs & cs_symbol_table = cs_symbol_table })
			= (bind, { cs & cs_symbol_table= cs_symbol_table, cs_error = checkError id_name "type variable not used in type" cs_error })
	
	id_info_occurs_in_list id_info [{bind_dst}:l]
		= id_info==bind_dst.tv_ident.id_info || id_info_occurs_in_list id_info l
	id_info_occurs_in_list id_info []
		= False

checkSpecialTypes :: !Index !Specials !v:{#CheckedTypeDef} !u:{#DclModule} !*TypeHeaps !*CheckState
						-> (!Specials,!x:{#CheckedTypeDef},!w:{#DclModule},!.TypeHeaps,!.CheckState), [u v <= w, v u <= x];
checkSpecialTypes mod_index (SP_ParsedSubstitutions envs) type_defs modules heaps cs
	# ots = { ots_type_defs = type_defs, ots_modules = modules }
	  (specials, (heaps, ots, cs)) = mapSt (check_environment mod_index) envs (heaps, ots, cs)
	= (SP_Substitutions specials, ots.ots_type_defs, ots.ots_modules, heaps, cs)
checkSpecialTypes mod_index SP_None type_defs modules heaps cs
	= (SP_None, type_defs, modules, heaps, cs)

checkFunSpecialTypes :: !Index !FunSpecials !v:{#CheckedTypeDef} !u:{#DclModule} !*TypeHeaps !*CheckState
						   -> (!FunSpecials,!x:{#CheckedTypeDef},!w:{#DclModule},!.TypeHeaps,!.CheckState), [u v <= w, v u <= x];
checkFunSpecialTypes mod_index (FSP_ParsedSubstitutions envs) type_defs modules heaps cs
	# ots = { ots_type_defs = type_defs, ots_modules = modules }
	  (specials, (heaps, ots, cs)) = mapSt (check_environment mod_index) envs (heaps, ots, cs)
	= (FSP_Substitutions specials, ots.ots_type_defs, ots.ots_modules, heaps, cs)
checkFunSpecialTypes mod_index FSP_None type_defs modules heaps cs
	= (FSP_None, type_defs, modules, heaps, cs)

check_environment :: Int (Env Type TypeVar) *(*TypeHeaps,u:OpenTypeSymbols,*CheckState) -> *(SpecialSubstitution,(*TypeHeaps,u:OpenTypeSymbols,*CheckState))
check_environment mod_index env (heaps, ots, cs)
 	# oti = { oti_heaps = heaps, oti_all_vars = [], oti_all_attrs = [], oti_global_vars = [] }
 	  (env, (ots, {oti_heaps,oti_all_vars,oti_all_attrs,oti_global_vars}, cs)) = mapSt (check_substituted_type mod_index) env (ots, oti, cs)
  	  cs_symbol_table = removeVariablesFromSymbolTable cGlobalScope oti_all_vars cs.cs_symbol_table
	  cs_symbol_table = removeAttributesFromSymbolTable oti_all_attrs cs_symbol_table
  	  cs = check_no_global_type_vars oti_global_vars {cs & cs_symbol_table = cs_symbol_table}
	= ({ ss_environ = env, ss_context = [], ss_vars = oti_all_vars, ss_attrs = oti_all_attrs}, (oti_heaps, ots, cs))
where
	check_substituted_type mod_index bind=:{bind_src} cot_state
		 # (bind_src, cot_state) = checkOpenType mod_index cGlobalScope DAK_Ignore bind_src cot_state
		 = ({ bind & bind_src = bind_src }, cot_state)

/* cOuterMostLevel :== 0 */

addTypeVariablesToSymbolTable :: !Level ![ATypeVar] ![AttributeVar] !*TypeHeaps !*CheckState
	-> (![ATypeVar], !(![AttributeVar], !*TypeHeaps, !*CheckState))
addTypeVariablesToSymbolTable scope type_vars attr_vars heaps cs
	= mapSt (add_type_variable_to_symbol_table scope) type_vars (attr_vars, heaps, cs)
where
	add_type_variable_to_symbol_table :: !Level !ATypeVar !(![AttributeVar], !*TypeHeaps, !*CheckState)
		-> (!ATypeVar, !(![AttributeVar], !*TypeHeaps, !*CheckState))
	add_type_variable_to_symbol_table scope atv=:{atv_variable=atv_variable=:{tv_ident}, atv_attribute}
		(attr_vars, heaps=:{th_vars,th_attrs}, cs=:{ cs_symbol_table, cs_error })
		# tv_info = tv_ident.id_info
		  (entry, cs_symbol_table) = readPtr tv_info cs_symbol_table	  
		| entry.ste_def_level < scope // cOuterMostLevel
			# (tv_info_ptr, th_vars) = newPtr TVI_Empty th_vars
		      atv_variable = { atv_variable & tv_info_ptr = tv_info_ptr }
		      (atv_attribute, attr_vars, th_attrs, cs_error) = check_attribute (scope == cRankTwoScope) atv_attribute tv_ident.id_name attr_vars th_attrs cs_error
			  cs_symbol_table = cs_symbol_table <:= (tv_info, {ste_index = NoIndex, ste_kind = STE_BoundTypeVariable {stv_attribute = atv_attribute,
			  						stv_info_ptr = tv_info_ptr}, ste_def_level = scope /* cOuterMostLevel */, ste_previous = entry, ste_doc = No })
			  heaps = { heaps & th_vars = th_vars, th_attrs = th_attrs }
			= ({atv & atv_variable = atv_variable, atv_attribute = atv_attribute},
					(attr_vars, heaps, { cs & cs_symbol_table = cs_symbol_table, cs_error = cs_error }))
			= (atv, (attr_vars, { heaps & th_vars = th_vars },
					 { cs & cs_symbol_table = cs_symbol_table, cs_error = checkError tv_ident.id_name "type variable already defined" cs_error }))

	check_attribute :: !Bool !TypeAttribute !String ![AttributeVar] !*AttrVarHeap !*ErrorAdmin
		-> (!TypeAttribute, ![AttributeVar], !*AttrVarHeap, !*ErrorAdmin)
	check_attribute _ TA_Unique name attr_vars attr_var_heap cs
		= (TA_Unique, attr_vars, attr_var_heap, cs)
	check_attribute is_rank_two attr name attr_vars attr_var_heap cs
		| is_rank_two
			= check_rank_two_attribute attr attr_vars attr_var_heap cs
			= check_global_attribute attr name attr_vars attr_var_heap cs
	where
		check_global_attribute TA_Multi name attr_vars attr_var_heap cs
			# (attr_info_ptr, attr_var_heap) = newPtr AVI_Empty attr_var_heap
			  new_var = { av_ident = emptyIdent name, av_info_ptr = attr_info_ptr}
			= (TA_Var new_var, [new_var : attr_vars], attr_var_heap, cs)
		check_global_attribute TA_None name attr_vars attr_var_heap cs
			# (attr_info_ptr, attr_var_heap) = newPtr AVI_Empty attr_var_heap
			  new_var = { av_ident = emptyIdent name, av_info_ptr = attr_info_ptr}
			= (TA_Var new_var, [new_var : attr_vars], attr_var_heap, cs)
		check_global_attribute _ name attr_vars attr_var_heap cs
			= (TA_Multi, attr_vars, attr_var_heap, checkError name "specified attribute variable not allowed" cs)

		check_rank_two_attribute (TA_Var var) attr_vars attr_var_heap cs
			# (attr_info_ptr, attr_var_heap) = newPtr AVI_Empty attr_var_heap
			  new_var = { var & av_info_ptr = attr_info_ptr}
			= (TA_Var new_var, [new_var : attr_vars], attr_var_heap, cs)
		check_rank_two_attribute TA_Anonymous attr_vars attr_var_heap cs
			= abort "check_rank_two_attribute (TA_Anonymous, check_types.icl)"
/*			# (attr_info_ptr, attr_var_heap) = newPtr AVI_Empty attr_var_heap
			  new_var = { av_ident = emptyIdent name, av_info_ptr = attr_info_ptr}
			= (TA_Var new_var, [new_var : attr_vars], attr_var_heap, cs)
*/		check_rank_two_attribute attr attr_vars attr_var_heap cs
			= (attr, attr_vars, attr_var_heap, cs)

addExistentionalTypeVariablesToSymbolTable :: !TypeAttribute ![ATypeVar] !*TypeHeaps !*CheckState
	-> (![ATypeVar], !(!*TypeHeaps, !*CheckState))
addExistentionalTypeVariablesToSymbolTable root_attr type_vars heaps cs
	= mapSt (add_exi_variable_to_symbol_table root_attr) type_vars (heaps, cs)
where
	add_exi_variable_to_symbol_table :: !TypeAttribute !ATypeVar !(!*TypeHeaps, !*CheckState)
		-> (!ATypeVar, !(!*TypeHeaps, !*CheckState))
	add_exi_variable_to_symbol_table root_attr atv=:{atv_variable=atv_variable=:{tv_ident}, atv_attribute}
		(heaps=:{th_vars,th_attrs}, cs=:{ cs_symbol_table, cs_error})
		# tv_info = tv_ident.id_info
		  (entry, cs_symbol_table) = readPtr tv_info cs_symbol_table
		| entry.ste_def_level < cGlobalScope // cOuterMostLevel
			# (tv_info_ptr, th_vars) = newPtr TVI_Empty th_vars
		      atv_variable = { atv_variable & tv_info_ptr = tv_info_ptr }
		      (atv_attribute, th_attrs, cs_error) = check_attribute atv_attribute root_attr tv_ident.id_name th_attrs cs_error
			  cs_symbol_table = cs_symbol_table <:= (tv_info, {ste_index = NoIndex, ste_kind = STE_BoundTypeVariable {stv_attribute = atv_attribute,
			  						stv_info_ptr = tv_info_ptr }, ste_def_level = cGlobalScope /* cOuterMostLevel */, ste_previous = entry, ste_doc = No })
			  heaps = { heaps & th_vars = th_vars, th_attrs = th_attrs }
			= ({atv & atv_variable = atv_variable, atv_attribute = atv_attribute},
					(heaps, { cs & cs_symbol_table = cs_symbol_table, cs_error = cs_error}))
			= (atv, ({ heaps & th_vars = th_vars },
					 { cs & cs_symbol_table = cs_symbol_table, cs_error = checkError tv_ident.id_name "type variable already defined" cs_error}))

	check_attribute :: !TypeAttribute !TypeAttribute !String !*AttrVarHeap !*ErrorAdmin
		-> (!TypeAttribute, !*AttrVarHeap, !*ErrorAdmin)
	check_attribute TA_Multi root_attr name attr_var_heap error
		= (TA_Multi, attr_var_heap, error)
	check_attribute TA_None root_attr name attr_var_heap error
		= (TA_Multi, attr_var_heap, error)
	check_attribute TA_Unique root_attr name attr_var_heap error
		= (TA_Unique, attr_var_heap, error)
	check_attribute (TA_Var var) root_attr name attr_var_heap error
		= case root_attr of
			TA_Var root_var
				-> (TA_RootVar root_var, attr_var_heap, error)
			TA_Unique
				# (attr_info_ptr, attr_var_heap) = newPtr AVI_Empty attr_var_heap
				-> (TA_Var { var & av_info_ptr = attr_info_ptr}, attr_var_heap, error)
	check_attribute attr root_attr name attr_var_heap error
		= (TA_Multi, attr_var_heap, checkError name "specified attribute not allowed" error)

removeAttributedTypeVarsFromSymbolTable :: !Level ![ATypeVar] !*SymbolTable -> *SymbolTable
removeAttributedTypeVarsFromSymbolTable level vars symbol_table
	= foldr (\{atv_variable={tv_ident}} -> removeDefinitionFromSymbolTable level tv_ident) symbol_table vars


cExistentialVariable	:== True
cUniversalVariable 		:== False

removeDefinitionFromSymbolTable level {id_info} symbol_table
	| isNilPtr id_info
		= symbol_table
		# ({ste_def_level, ste_previous}, symbol_table) = readPtr id_info symbol_table
		| ste_def_level >= level
			= symbol_table <:= (id_info, ste_previous)
			= symbol_table

removeAttributesFromSymbolTable :: ![AttributeVar] !*SymbolTable -> *SymbolTable
removeAttributesFromSymbolTable attrs symbol_table
	= foldr (\{av_ident} -> removeDefinitionFromSymbolTable cGlobalScope av_ident) symbol_table attrs

removeVariablesFromSymbolTable :: !Int ![TypeVar] !*SymbolTable -> *SymbolTable
removeVariablesFromSymbolTable scope vars symbol_table
	= foldr (\{tv_ident} -> removeDefinitionFromSymbolTable scope tv_ident) symbol_table vars

::	Indexes =
	{	index_type		:: !Index
	,	index_cons		:: !Index
	,	index_selector	:: !Index
	}

makeAttributedType attr type :== { at_attribute = attr, at_type = type }

createClassDictionaries ::		  !Bool !Index !Index !Index !Index !*{#CheckedTypeDef} !*{# SelectorDef} !*{# ConsDef} !*{#ClassDef} !*{#DclModule} !*TypeVarHeap !*VarHeap !*SymbolTable
	-> (![CheckedTypeDef],![SelectorDef],![ConsDef],!DictionaryInfo,!*{#CheckedTypeDef},!*{# SelectorDef},!*{# ConsDef},!*{#ClassDef},!*{#DclModule},!*TypeVarHeap,!*VarHeap,!*SymbolTable)
createClassDictionaries is_dcl mod_index first_type_index first_selector_index first_cons_index type_defs selector_defs cons_defs class_defs modules type_var_heap var_heap symbol_table
	| is_dcl
		# indexes = { index_type = first_type_index, index_cons= first_cons_index, index_selector = first_selector_index }
		# (class_defs, modules, rev_dictionary_list, indexes, type_var_heap, var_heap, symbol_table)
				= create_class_dictionaries mod_index 0 class_defs modules [] indexes type_var_heap var_heap symbol_table
		  (type_def_list, sel_def_list, cons_def_list, symbol_table) = foldSt collect_type_def rev_dictionary_list  ([], [], [], symbol_table)
		  dictionary_info = {	n_dictionary_types = indexes.index_type-first_type_index,
		  						n_dictionary_constructors = indexes.index_cons-first_cons_index,
		  						n_dictionary_selectors = indexes.index_selector-first_selector_index
		  					}
		= (type_def_list, sel_def_list, cons_def_list, dictionary_info, type_defs, selector_defs, cons_defs, class_defs, modules, type_var_heap, var_heap, symbol_table)

		# (dcl_class_defs,modules) = modules![mod_index].dcl_common.com_class_defs
		# class_defs = number_exported_icl_class_dictionaries 0 dcl_class_defs class_defs
		# (class_defs,last_type_index_plus1) = number_icl_class_dictionaries 0 class_defs first_type_index
		#! first_dcl_dictionary_cons_index = modules.[mod_index].dcl_sizes.[cConstructorDefs]
		#! first_dcl_dictionary_selector_index = modules.[mod_index].dcl_sizes.[cSelectorDefs]

		# indexes = { index_type = first_type_index, index_cons = first_dcl_dictionary_cons_index, index_selector = first_dcl_dictionary_selector_index }
		# (type_defs, class_defs, modules, rev_dictionary_list, indexes, type_var_heap, var_heap, symbol_table)
			= create_exported_icl_class_dictionaries mod_index 0 dcl_class_defs type_defs class_defs modules [] indexes type_var_heap var_heap symbol_table

		# indexes = { index_type = first_type_index, index_cons= first_cons_index, index_selector = first_selector_index }
		# (class_defs, modules, rev_dictionary_list, indexes, type_var_heap, var_heap, symbol_table)
			= create_icl_class_dictionaries mod_index 0 last_type_index_plus1 first_type_index class_defs modules rev_dictionary_list indexes type_var_heap var_heap symbol_table

		# (size_type_defs,type_defs) = usize type_defs
		  (type_def_list, sel_def_list, cons_def_list, selector_defs, cons_defs, symbol_table)
		  	= collect_type_defs_in_icl_module size_type_defs rev_dictionary_list selector_defs cons_defs symbol_table
		# (dictionary_info,modules)=modules![mod_index].dcl_dictionary_info
		= (type_def_list, sel_def_list, cons_def_list, dictionary_info, type_defs, selector_defs, cons_defs, class_defs, modules, type_var_heap, var_heap, symbol_table)
		with
			number_exported_icl_class_dictionaries dcl_class_index dcl_class_defs class_defs
				| dcl_class_index < size dcl_class_defs
					# icl_class_index = dcl_class_index
					# dcl_dictionary_index = dcl_class_defs.[dcl_class_index].class_dictionary.ds_index
					# class_defs = { class_defs & [icl_class_index].class_dictionary.ds_index = dcl_dictionary_index }
					= number_exported_icl_class_dictionaries (inc dcl_class_index) dcl_class_defs class_defs
					= class_defs
where
	collect_type_def type_ptr (type_defs, sel_defs, cons_defs, symbol_table)
		# ({ ste_kind = STE_DictType type_def }, symbol_table) = readPtr type_ptr symbol_table
		  (RecordType {rt_constructor, rt_fields}) = type_def.td_rhs
		  ({ ste_kind = STE_DictCons cons_def }, symbol_table) = readPtr rt_constructor.ds_ident.id_info symbol_table
	 	  (sel_defs, symbol_table) = collect_fields 0 rt_fields (sel_defs, symbol_table)
	 	= ( [type_def : type_defs ] , sel_defs, [cons_def : cons_defs], symbol_table)

	create_class_dictionaries mod_index class_index class_defs modules rev_dictionary_list indexes type_var_heap var_heap cs
		| class_index < size class_defs
			# (class_defs, modules, type_id_info, indexes, type_var_heap, var_heap, cs)
				= create_class_dictionary mod_index class_index class_defs modules indexes type_var_heap var_heap cs
			# rev_dictionary_list = [ type_id_info : rev_dictionary_list ]
			= create_class_dictionaries mod_index (inc class_index) class_defs modules rev_dictionary_list indexes type_var_heap var_heap cs
			= (class_defs, modules, rev_dictionary_list, indexes, type_var_heap, var_heap, cs)

	create_exported_icl_class_dictionaries mod_index dcl_class_index dcl_class_defs type_defs class_defs modules rev_dictionary_list indexes type_var_heap var_heap symbol_table
		| dcl_class_index < size dcl_class_defs
			# icl_class_index = dcl_class_index
			# dcl_dictionary_index = dcl_class_defs.[dcl_class_index].class_dictionary.ds_index
			# indexes = {indexes & index_type=dcl_dictionary_index}
			# (class_defs, modules, type_id_info, indexes, type_var_heap, var_heap, symbol_table)
				= create_class_dictionary mod_index icl_class_index class_defs modules indexes type_var_heap var_heap symbol_table
			# ({ ste_kind = STE_DictType type_def }, symbol_table) = readPtr type_id_info symbol_table
			# type_defs = {type_defs & [type_def.td_index]=type_def}
			# rev_dictionary_list = [ type_id_info : rev_dictionary_list ]
			= create_exported_icl_class_dictionaries mod_index (inc dcl_class_index) dcl_class_defs type_defs class_defs modules rev_dictionary_list indexes type_var_heap var_heap symbol_table
			= (type_defs, class_defs, modules, rev_dictionary_list, indexes, type_var_heap, var_heap, symbol_table)

createMoreClassDictionaries ::	   !Int !Index !Index !Index !Index !*{#CheckedTypeDef} !*{#SelectorDef} !*{#ConsDef} !*{#ClassDef} !*{#DclModule} !*TypeVarHeap !*VarHeap !*SymbolTable
					-> (![CheckedTypeDef],![SelectorDef],![ConsDef],!*{#CheckedTypeDef},!*{#SelectorDef},!*{#ConsDef},!*{#ClassDef},!*{#DclModule},!*TypeVarHeap,!*VarHeap,!*SymbolTable)
createMoreClassDictionaries first_new_class_index mod_index first_type_index first_selector_index first_cons_index type_defs selector_defs cons_defs class_defs modules type_var_heap var_heap symbol_table
	# (class_defs,last_type_index_plus1) = number_icl_class_dictionaries first_new_class_index class_defs first_type_index
	# indexes = { index_type = first_type_index, index_cons= first_cons_index, index_selector = first_selector_index }
	# (class_defs, modules, rev_dictionary_list, indexes, type_var_heap, var_heap, symbol_table)
		= create_icl_class_dictionaries mod_index first_new_class_index last_type_index_plus1 first_type_index class_defs modules [] indexes type_var_heap var_heap symbol_table
	# (size_type_defs,type_defs) = usize type_defs
	  (type_def_list, sel_def_list, cons_def_list, selector_defs, cons_defs, symbol_table)
	  	= collect_type_defs_in_icl_module size_type_defs rev_dictionary_list selector_defs cons_defs symbol_table
	= (type_def_list, sel_def_list, cons_def_list, type_defs, selector_defs, cons_defs, class_defs, modules, type_var_heap, var_heap, symbol_table)

number_icl_class_dictionaries class_index class_defs index_type
	| class_index < size class_defs
		| class_defs.[class_index].class_dictionary.ds_index==NoIndex
			# class_defs = { class_defs & [class_index].class_dictionary.ds_index = index_type }
			= number_icl_class_dictionaries (inc class_index) class_defs (inc index_type)
			= number_icl_class_dictionaries (inc class_index) class_defs index_type
		= (class_defs,index_type)

create_icl_class_dictionaries mod_index class_index last_type_index_plus1 first_type_index class_defs modules rev_dictionary_list indexes type_var_heap var_heap symbol_table
	| class_index < size class_defs
		# index=class_defs.[class_index].class_dictionary.ds_index
		| index>=first_type_index && index<last_type_index_plus1
			# (class_defs, modules, type_id_info, indexes, type_var_heap, var_heap, symbol_table)
				= create_class_dictionary mod_index class_index class_defs modules indexes type_var_heap var_heap symbol_table
			# rev_dictionary_list = [ type_id_info : rev_dictionary_list ]
			= create_icl_class_dictionaries mod_index (inc class_index) last_type_index_plus1 first_type_index class_defs modules rev_dictionary_list indexes type_var_heap var_heap symbol_table
			= create_icl_class_dictionaries mod_index (inc class_index) last_type_index_plus1 first_type_index class_defs modules rev_dictionary_list indexes type_var_heap var_heap symbol_table
		= (class_defs, modules, rev_dictionary_list, indexes, type_var_heap, var_heap, symbol_table)

collect_type_defs_in_icl_module size_type_defs rev_dictionary_list selector_defs cons_defs symbol_table
	= foldSt (collect_type_def_in_icl_module size_type_defs) rev_dictionary_list ([], [], [], selector_defs, cons_defs, symbol_table)
where
	collect_type_def_in_icl_module size_type_defs type_ptr (type_defs, sel_def_list, cons_def_list, selector_defs, cons_defs, symbol_table)
		# ({ ste_kind = STE_DictType type_def,ste_index}, symbol_table) = readPtr type_ptr symbol_table
		  (RecordType {rt_constructor, rt_fields}) = type_def.td_rhs
		  ({ ste_kind = STE_DictCons cons_def }, symbol_table) = readPtr rt_constructor.ds_ident.id_info symbol_table
	 	| ste_index < size_type_defs	 		
			# cons_defs = {cons_defs & [rt_constructor.ds_index] = cons_def}
			# (selector_defs, symbol_table) = store_fields_in_selector_array 0 rt_fields (selector_defs, symbol_table)
			= (type_defs , sel_def_list, cons_def_list, selector_defs, cons_defs, symbol_table)
			# (sel_def_list, symbol_table) = collect_fields 0 rt_fields (sel_def_list, symbol_table)
			= ([type_def : type_defs ] , sel_def_list, [cons_def : cons_def_list], selector_defs, cons_defs, symbol_table)

	store_fields_in_selector_array field_nr fields (sel_defs, symbol_table)
		| field_nr < size fields
			# field = fields.[field_nr]
			# ({ ste_kind = STE_DictField sel_def }, symbol_table) = readPtr field.fs_ident.id_info symbol_table
			# sel_defs = {sel_defs & [field.fs_index] = sel_def }
			= store_fields_in_selector_array (inc field_nr) fields (sel_defs, symbol_table)
			= ( sel_defs, symbol_table)

collect_fields field_nr fields (sel_defs, symbol_table)
	| field_nr < size fields
		# (sel_defs, symbol_table) = collect_fields (inc field_nr) fields (sel_defs, symbol_table)
		  ({ ste_kind = STE_DictField sel_def }, symbol_table) = readPtr fields.[field_nr].fs_ident.id_info symbol_table
		= ( [ sel_def : sel_defs ], symbol_table)
		= ( sel_defs, symbol_table)

create_class_dictionary :: !Index !Index !*{#ClassDef} !w:{#DclModule} !u:Indexes !*TypeVarHeap !*VarHeap !*SymbolTable 
						-> (!*{#ClassDef}, !w:{#DclModule}, !SymbolPtr,!u:Indexes,!*TypeVarHeap,!*VarHeap,!*SymbolTable)
create_class_dictionary mod_index class_index class_defs =:{[class_index] = class_def } modules indexes type_var_heap var_heap symbol_table
	# {class_ident,class_args,class_arity,class_members,class_context,class_dictionary=ds=:{ds_ident={id_name,id_info}},class_lazy_members} = class_def
	  (type_id_info, symbol_table) = newPtr EmptySymbolTableEntry symbol_table
	  nr_of_members = size class_members
	  nr_of_fields = nr_of_members + length class_context

	  dictionary_record_name = class_ident.id_name+++";";
	  rec_type_id = { id_name = dictionary_record_name, id_info = type_id_info }

	  { index_type, index_cons, index_selector } = indexes

	  class_dictionary = { ds & ds_ident = rec_type_id, ds_index = index_type }

	  type_symb = MakeTypeSymbIdent { glob_object = index_type, glob_module = mod_index } rec_type_id class_arity

	  rec_type		= makeAttributedType TA_Multi (TA type_symb [makeAttributedType TA_Multi TE \\ i <- [1..class_arity]])
	  field_type	= makeAttributedType TA_Multi TE
	  (rev_fields, var_heap, symbol_table)
  			= build_fields 0 nr_of_members class_members rec_type field_type index_type index_selector [] var_heap symbol_table

	  (index_selector, rev_fields, rev_field_types, class_defs, modules, var_heap, symbol_table)
  			= build_context_fields mod_index nr_of_members class_context rec_type index_type (index_selector + nr_of_members) rev_fields
  					[ field_type \\ i <- [1..nr_of_members] ] class_defs modules var_heap symbol_table

	  (cons_id_info, symbol_table) = newPtr EmptySymbolTableEntry symbol_table
	  rec_cons_id = { id_name = dictionary_record_name, id_info = cons_id_info }

	  cons_symbol = { ds_ident = rec_cons_id, ds_arity = nr_of_fields, ds_index = index_cons }
	  (cons_type_ptr, var_heap) = newPtr VI_Empty var_heap

	  cons_def = 	
		{	cons_ident		= rec_cons_id
		,	cons_type		= { st_vars	= [], st_args = reverse rev_field_types,
								st_args_strictness = if (class_lazy_members==0) (first_n_strict nr_of_fields) NotStrict,
								st_result = rec_type, st_arity = nr_of_fields,
							    st_context = [], st_attr_vars = [], st_attr_env = [] }
		,	cons_priority	= NoPrio
		,	cons_number		= 0
		,	cons_type_index	= index_type
		,	cons_exi_vars	= []
		,	cons_type_ptr	= cons_type_ptr
		,	cons_pos		= NoPos
		}

	  (td_args, type_var_heap) = mapSt new_attributed_type_variable class_args type_var_heap
	  
	  type_def =
	 	{	td_ident		= rec_type_id
		,	td_index		= index_type
		,	td_arity		= 0
		,	td_args			= td_args
		,	td_attrs		= []
		,	td_rhs			= RecordType {rt_constructor = cons_symbol, rt_fields = { field \\ field <- reverse rev_fields }, rt_is_boxed_record=False}
		,	td_attribute	= TA_None
		,	td_pos			= NoPos
		,	td_used_types	= []
		,	td_fun_index	= NoIndex
		}

	  symbol_table = symbol_table <:= (type_id_info, { ste_kind = STE_DictType type_def, ste_index = index_type,
											ste_def_level = NotALevel, ste_previous = abort "empty SymbolTableEntry", ste_doc = No })
								  <:= (cons_id_info, { ste_kind = STE_DictCons cons_def, ste_index = index_cons,
											ste_def_level = NotALevel, ste_previous = abort "empty SymbolTableEntry", ste_doc = No })

	= ({class_defs & [class_index] = {class_def & class_dictionary = class_dictionary}}, modules,
			type_id_info, { index_type = inc index_type, index_cons = inc index_cons, index_selector = index_selector },
				type_var_heap, var_heap, symbol_table)
where
	new_attributed_type_variable tv type_var_heap
		# (new_tv_ptr, type_var_heap) = newPtr TVI_Empty type_var_heap
		= ({atv_attribute = TA_Multi, atv_variable = { tv & tv_info_ptr = new_tv_ptr }}, type_var_heap)
		
	build_fields field_nr nr_of_fields class_members rec_type field_type rec_type_index next_selector_index rev_fields var_heap symbol_table
		| field_nr < nr_of_fields
			# field_name = class_members.[field_nr].ds_ident.id_name
			# (field, var_heap, symbol_table)
				= build_field field_nr field_name rec_type_index rec_type field_type next_selector_index var_heap symbol_table
			= build_fields (inc field_nr) nr_of_fields class_members rec_type field_type rec_type_index (inc next_selector_index)
				 [field : rev_fields] var_heap symbol_table
			= (rev_fields, var_heap, symbol_table)			

	build_context_fields mod_index field_nr [{tc_class = TCClass {glob_module, glob_object={ds_index}}}:tcs] rec_type rec_type_index
			next_selector_index rev_fields rev_field_types class_defs modules var_heap symbol_table
		# ({class_ident, class_arity, class_dictionary = {ds_ident, ds_index}}, _, class_defs, modules) = getClassDef ds_index glob_module mod_index class_defs modules
		  type_symb = MakeTypeSymbIdent { glob_object = ds_index, glob_module = glob_module } ds_ident class_arity
		  field_type = makeAttributedType TA_Multi (TA type_symb [makeAttributedType TA_Multi TE \\ i <- [1..class_arity]])
		  (field, var_heap, symbol_table)
			= build_field field_nr class_ident.id_name rec_type_index rec_type field_type next_selector_index var_heap symbol_table
		= build_context_fields mod_index (inc field_nr) tcs rec_type rec_type_index (inc next_selector_index) [field : rev_fields]
				 [field_type : rev_field_types] class_defs modules var_heap symbol_table
	build_context_fields mod_index field_nr [{tc_class = TCGeneric {gtc_generic,gtc_kind,gtc_generic_dict}} :tcs] rec_type rec_type_index
			next_selector_index rev_fields rev_field_types class_defs modules var_heap symbol_table
		// FIXME: We do not know the type before the generic phase.
		// The generic phase currently does not update the type.
		# field_type = {at_attribute = TA_Multi, at_type = TGenericFunctionInDictionary gtc_generic gtc_kind gtc_generic_dict}
		# class_ident = genericIdentToClassIdent gtc_generic.glob_object.ds_ident.id_name gtc_kind
		# (field, var_heap, symbol_table)
			= build_field field_nr class_ident.id_name rec_type_index rec_type field_type next_selector_index var_heap symbol_table
		= build_context_fields mod_index (inc field_nr) tcs rec_type rec_type_index (inc next_selector_index) [field : rev_fields]
				 [field_type : rev_field_types] class_defs modules var_heap symbol_table
	build_context_fields mod_index field_nr [] rec_type rec_type_index next_selector_index rev_fields rev_field_types class_defs modules var_heap symbol_table
		= (next_selector_index, rev_fields, rev_field_types , class_defs, modules, var_heap, symbol_table)			

	build_field field_nr field_name rec_type_index rec_type field_type selector_index var_heap symbol_table
		# (id_info, symbol_table) = newPtr EmptySymbolTableEntry symbol_table
		  (sd_type_ptr, var_heap) = newPtr VI_Empty var_heap
  		  field_id = { id_name = field_name, id_info = id_info }
  		  sel_def =
  		  	{	sd_ident		= field_id
  		  	,	sd_field		= field_id
  		  	,	sd_type			= { st_vars	= [], st_args = [ rec_type ], st_args_strictness=Strict 1, st_result = field_type, st_arity = 1,
  		  	                        st_context = [], st_attr_vars = [], st_attr_env = [] }
			,	sd_exi_vars		= []
			,	sd_field_nr		= field_nr
			,	sd_type_index	= rec_type_index
			,	sd_type_ptr		= sd_type_ptr
			,	sd_pos			= NoPos
			}
		  field = { fs_ident = field_id, fs_var = field_id, fs_index = selector_index }
		= (field, var_heap, symbol_table <:= (id_info, { ste_kind = STE_DictField sel_def, ste_index = selector_index,
				ste_def_level = NotALevel, ste_previous = abort "empty SymbolTableEntry", ste_doc = No }))

size_types :: ![Type] !Int -> Int
size_types [type:types] s = size_types types (size_type type s)
where
	size_type :: !Type !Int -> Int
	size_type (TB _) s = s+1
	size_type (TV _) s = s+1
	size_type (TA _ atypes) s = size_atypes atypes (s+1)
	size_type (TAS _ atypes _) s = size_atypes atypes (s+1)
	size_type (arg_type --> result_type) s = size_type result_type.at_type (size_type arg_type.at_type (s+1))
	size_type (CV _ :@: atypes) s = size_atypes atypes (s+1)
	size_type (TArrow1 {at_type}) s = size_type at_type (s+1)
	size_type TArrow s = s+1
	size_type (TFA vars type) s = size_type type s
	size_type _ s = s
	
	size_atypes :: ![AType] !Int -> Int
	size_atypes [{at_type}:atypes] s = size_atypes atypes (size_type at_type s)
	size_atypes [] s = s
size_types [] s = s

class toVariable var :: !STE_Kind !Ident -> var

instance toVariable TypeVar
where
	toVariable (STE_TypeVariable info_ptr) ident = { tv_ident = ident, tv_info_ptr = info_ptr }

instance toVariable AttributeVar
where
	toVariable (STE_TypeAttribute info_ptr) ident = { av_ident = ident, av_info_ptr = info_ptr }

instance <<< DynamicType
where
	(<<<) file {dt_global_vars,dt_type} = file <<< dt_global_vars <<< dt_type
