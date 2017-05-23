implementation module checkgenerics

import syntax,checksupport,checktypes,genericsupport,explicitimports,compare_types,typesupport

checkGenericDefs :: !Index !(Optional (CopiedDefinitions, Int))
		!*{#GenericDef} !*{#CheckedTypeDef} !*{#ClassDef} !*{#DclModule} !*Heaps !*CheckState
	-> (!*{#GenericDef},!*{#CheckedTypeDef},!*{#ClassDef},!*{#DclModule},!*Heaps,!*CheckState)
checkGenericDefs mod_index opt_icl_info gen_defs type_defs class_defs modules heaps cs 	
	= check_generics 0 mod_index opt_icl_info gen_defs type_defs class_defs modules heaps cs
where
	check_generics index mod_index opt_icl_info gen_defs type_defs class_defs modules heaps cs
		# (n_generics, gen_defs) = usize gen_defs
		| index == n_generics 
			= (gen_defs, type_defs, class_defs, modules, heaps, cs)
			# (gen_defs, type_defs, class_defs, modules, heaps, cs) 
				= check_generic_def index mod_index opt_icl_info gen_defs type_defs class_defs modules heaps cs
			= check_generics (inc index) mod_index opt_icl_info gen_defs type_defs class_defs modules heaps cs		

	check_generic_def index mod_index opt_icl_info gen_defs type_defs class_defs modules heaps cs
		| has_to_be_checked mod_index index opt_icl_info 	
			= check_generic index mod_index gen_defs type_defs class_defs modules heaps cs		
			= (gen_defs, type_defs, class_defs, modules, heaps, cs)

	has_to_be_checked module_index generic_index No 
		= True
	has_to_be_checked module_index generic_index (Yes ({copied_generic_defs}, n_cached_dcl_mods))
		= not (module_index < n_cached_dcl_mods && generic_index < size copied_generic_defs && copied_generic_defs.[generic_index])
		
	check_generic index mod_index gen_defs type_defs class_defs modules heaps cs
		# (gen_def=:{gen_ident, gen_pos}, gen_defs) = gen_defs![index]
		# cs = pushErrorAdmin (newPosition gen_ident gen_pos) cs

		# (gen_def, heaps) = alloc_gen_info gen_def heaps

		# (gen_def, type_defs, class_defs, modules, heaps, cs)
			= check_generic_type gen_def mod_index type_defs class_defs modules heaps cs

		# (gen_def, gen_defs, modules, cs) = check_generic_dependencies index mod_index gen_ident gen_def gen_defs modules cs

		# gen_defs = {gen_defs & [index] = gen_def} 
		# (cs=:{cs_x}) = popErrorAdmin cs
		#! cs = { cs & cs_x = {cs_x & x_needed_modules = cs_x.x_needed_modules bitor cNeedStdGeneric}}			
		= (gen_defs, type_defs, class_defs, modules, heaps, cs)

	alloc_gen_info gen_def heaps=:{hp_generic_heap}
		# initial_info = 
			{ gen_classes = createArray 32 []
			, gen_var_kinds = []
			, gen_rep_conses
				= createArray 7 {grc_module = -1, grc_index = GCB_None, grc_local_fun_index = -1, grc_generic_info = -1,
								 grc_generic_instance_deps = AllGenericInstanceDependencies,
								 grc_ident={id_name="",id_info=nilPtr},
								 grc_optional_fun_type=No}
			}
		# (gen_info_ptr, hp_generic_heap) = newPtr initial_info hp_generic_heap 
		= (	{gen_def & gen_info_ptr = gen_info_ptr}, 
			{heaps & hp_generic_heap = hp_generic_heap})

	check_generic_type gen_def=:{gen_type, gen_vars, gen_ident, gen_pos} module_index type_defs class_defs modules heaps=:{hp_type_heaps} cs
		#! (checked_gen_type, _, type_defs, class_defs, modules, hp_type_heaps, cs)
			= checkFunctionType module_index gen_type FSP_None type_defs class_defs modules hp_type_heaps cs
		
		#! (checked_gen_vars, cs) = check_generic_vars gen_vars checked_gen_type.st_vars cs		
		#! checked_gen_type = { checked_gen_type & st_vars = move_gen_vars checked_gen_vars checked_gen_type.st_vars}
				
		#! (hp_type_heaps, cs) = check_no_generic_vars_in_contexts checked_gen_type checked_gen_vars hp_type_heaps cs
		= 	( {gen_def & gen_type = checked_gen_type, gen_vars = checked_gen_vars}
			, type_defs
			, class_defs
			, modules
			, {heaps & hp_type_heaps = hp_type_heaps}
			, cs
			)
	where
		check_generic_vars gen_vars st_vars cs=:{cs_error}
			# (gen_vars, _, cs_error) = foldSt check_generic_var gen_vars ([], st_vars, cs_error)
			= (reverse gen_vars, {cs & cs_error = cs_error})
	
		// make sure generic variables are first
		move_gen_vars gen_vars st_vars
			= gen_vars ++ (removeMembers st_vars gen_vars)
				
		check_generic_var gv (acc_gvs, [], error)
			= (acc_gvs, [], checkError gv.tv_ident "generic variable not used" error) 
		check_generic_var gv (acc_gvs, [tv:tvs], error)
			| gv.tv_ident.id_name == tv.tv_ident.id_name
				= ([tv:acc_gvs], tvs, error)
				# (acc_gvs, tvs, error) = check_generic_var gv (acc_gvs, tvs, error)
				= (acc_gvs, [tv:tvs], error)
									
	// returns reversed variable list		
	add_vars_to_symbol_table gen_vars type_heaps=:{th_vars} cs=:{cs_error, cs_symbol_table}
		#! (rev_gen_vars,cs_symbol_table,th_vars, cs_error) 
			= foldSt add_var_to_symbol_table gen_vars ([],cs.cs_symbol_table,th_vars, cs_error)
		= (	rev_gen_vars,
			{type_heaps & th_vars = th_vars}, 
			{cs & cs_error = cs_error, cs_symbol_table = cs_symbol_table})			
	
	add_var_to_symbol_table :: !TypeVar !(![TypeVar], !*SymbolTable, !*TypeVarHeap, !*ErrorAdmin)
		-> (![TypeVar],!*SymbolTable,!*TypeVarHeap,!*ErrorAdmin)
	add_var_to_symbol_table tv=:{tv_ident={id_name,id_info}} (rev_class_args, symbol_table, th_vars, error)
	  	#! (entry, symbol_table) = readPtr id_info symbol_table
		| entry.ste_kind == STE_Empty || entry.ste_def_level < cGlobalScope
			# (new_var_ptr, th_vars) = newPtr TVI_Empty th_vars
			# symbol_table = NewEntry symbol_table id_info (STE_TypeVariable new_var_ptr) NoIndex cGlobalScope entry
			= ([{ tv & tv_info_ptr = new_var_ptr} : rev_class_args], symbol_table, th_vars, error)
			= (rev_class_args, symbol_table, th_vars, checkError id_name "generic variable already defined" error)
	
	// also reverses variable list (but does not make coffe)
	remove_vars_from_symbol_table rev_gen_vars cs=:{cs_symbol_table}
		#! (gen_vars, cs_symbol_table) = foldSt remove_var_from_symbol_table rev_gen_vars ([], cs_symbol_table)
		= (gen_vars, { cs & cs_symbol_table = cs_symbol_table})
	remove_var_from_symbol_table tv=:{tv_ident={id_name,id_info}} (gen_vars, symbol_table)
	  	#! (entry, symbol_table) = readPtr id_info symbol_table
		#! symbol_table = writePtr id_info entry.ste_previous symbol_table
		=([tv:gen_vars], symbol_table)

	check_no_generic_vars_in_contexts :: !SymbolType ![TypeVar] !*TypeHeaps !*CheckState -> (!*TypeHeaps, !*CheckState)
	check_no_generic_vars_in_contexts gen_type gen_vars th=:{th_vars} cs=:{cs_error}
		#! th_vars = clear_type_vars gen_type.st_vars th_vars
		#! th_vars = mark_type_vars_used gen_vars th_vars
		#! (th_vars, cs_error) = check_type_vars_not_used gen_type.st_context th_vars cs_error
		#! th_vars = clear_type_vars gen_type.st_vars th_vars
	
		= ({th & th_vars = th_vars}, {cs & cs_error = cs_error})
	where
		mark_type_vars_used gen_vars th_vars
			= foldSt (write_type_var_info TVI_Used) gen_vars th_vars
		clear_type_vars gen_vars th_vars
			= foldSt (write_type_var_info TVI_Empty) gen_vars th_vars
		write_type_var_info tvi {tv_ident, tv_info_ptr} th_vars 
			= writePtr tv_info_ptr tvi th_vars
		
		check_type_vars_not_used :: ![TypeContext] !*TypeVarHeap !*ErrorAdmin -> (!*TypeVarHeap, !*ErrorAdmin) 
		check_type_vars_not_used contexts th_vars cs_error
			# types	= flatten [tc_types \\ {tc_types} <- contexts]		
			# atypes = [{at_type=t,at_attribute=TA_None} \\ t <- types]
			= performOnTypeVars check_type_var_not_used atypes (th_vars, cs_error)
		check_type_var_not_used attr tv=:{tv_ident, tv_info_ptr} (th_vars, cs_error)
			#! (tv_info, th_vars) = readPtr tv_info_ptr th_vars
			= case tv_info of
				TVI_Empty  
					-> (th_vars, cs_error)
				TVI_Used
					#! cs_error = checkError tv_ident "context restrictions on generic variables are not allowed" cs_error 
					-> (th_vars, cs_error)
				_	-> abort ("check_no_generic_vars_in_contexts: wrong TVI" ---> (tv, tv_info))

        // TODO: TvN: check that a generic function also includes all the dependencies of its dependencies, and so on. This is required when
        // deriving generic functions since then the generated function needs to have all the arguments to all the generic functions called. In a
        // that process collapses all dependencies.
	check_generic_dependencies index mod_index gen_ident gen_def=:{gen_vars, gen_deps} gen_defs modules cs
		# (gen_deps, (gen_defs, modules, cs)) = foldSt check_dependency gen_deps ([], (gen_defs, modules, cs))
		= ({gen_def & gen_deps = reverse gen_deps}, gen_defs, modules, cs)
	where
		check_dependency gen_dep=:{gd_ident, gd_vars} (acc, (gen_defs, modules, cs))
			# (gen_dep, cs) = resolve_dependency_index gen_dep cs
			| gen_dep.gd_index.gi_index < 0
				= (acc, (gen_defs, modules, cs))
			# (gen_dep=:{gd_index, gd_vars}, gen_defs, modules, cs) = check_dependency_vars gen_dep gen_defs modules cs
			| gd_index.gi_index == index && gd_index.gi_module == mod_index && gd_vars == gen_vars
				= (acc, (gen_defs, modules, check_generic_dep_error gd_ident "already implicitly depends on itself" cs))
			| isMember gen_dep acc
				= (acc, (gen_defs, modules, check_generic_dep_error gd_ident "duplicate generic dependency" cs))
                        // TODO: TvN: This check is to prevent duplicate dependencies with different generic dependency variables
                        // See functions: generics1.build_specialized_expr and generics1.specialize_type_var
			| isMember gen_dep.gd_index [gd_index \\ {gd_index} <- acc]
				= (acc, (gen_defs, modules, check_generic_dep_error gd_ident "dependency occurs multiple times with different generic dependency variables, but only one occurrence of the same generic function as a dependency is currently allowed" cs))
			= ([gen_dep:acc], (gen_defs, modules, cs))

		resolve_dependency_index gen_dep=:{gd_ident} cs 
			= case gd_ident of
				Ident ident 
					# (index, cs) = get_generic_index ident mod_index cs
					= ({gen_dep & gd_index = index}, cs)
				QualifiedIdent mod_ident name 
					# (found, {decl_kind, decl_ident, decl_index}, cs) = search_qualified_ident mod_ident name GenericNameSpaceN cs
					| not found 
						= (gen_dep, check_generic_dep_error gd_ident "generic dependency not defined" cs)	
					= case decl_kind of
						STE_Imported (STE_Generic _) generic_module
							-> ({gen_dep & gd_ident = Ident decl_ident, gd_index = {gi_module = generic_module, gi_index = decl_index}}, cs)
						_ 
							-> (gen_dep, check_generic_dep_error gd_ident "not a generic function" cs)

		check_dependency_vars gen_dep=:{gd_ident, gd_vars} gen_defs modules cs 
			# (gen_defs, modules, cs) = check_dependency_arity gen_dep gen_defs modules cs
			# (gd_vars, gd_nums, cs) = mapY2St (resolve_dependency_var 0 gen_vars) gd_vars cs
			= ({gen_dep & gd_vars = gd_vars, gd_nums = gd_nums}, gen_defs, modules, cs)
		where
			check_dependency_arity {gd_ident, gd_index, gd_vars} gen_defs modules cs
				# (gen_def, gen_defs, modules) = lookup_dependency_def gd_index gen_defs modules
				| not (length gd_vars == length gen_def.gen_vars)
					= (gen_defs, modules, check_generic_dep_error gd_ident "incorrect dependency variable arity" cs)
				= (gen_defs, modules, cs)
			where
				lookup_dependency_def {gi_module, gi_index} gen_defs modules
					| gi_module == mod_index
						# (gen_def, gen_defs) = gen_defs![gi_index]
						= (gen_def, gen_defs, modules)
					# (gen_def, modules) = modules![gi_module].dcl_common.com_generic_defs.[gi_index]
					= (gen_def, gen_defs, modules)
				
			resolve_dependency_var num [] var cs
				= (var, -1, check_generic_dep_error gd_ident "generic dependency is indexed by an unbound generic variable" cs)
			resolve_dependency_var num [gen_var:gen_vars] var cs
				| var.tv_ident.id_name == gen_var.tv_ident.id_name
					= (gen_var, num, cs)
				= resolve_dependency_var (inc num) gen_vars var cs

		check_generic_dep_error ident msg cs = {cs & cs_error = checkError ident msg cs.cs_error}	

checkGenericCaseDefs :: !Index !*{#GenericCaseDef} !*{#GenericDef} !u:{#CheckedTypeDef} !*{#ClassDef} !*{#DclModule} !*Heaps !*CheckState
						   -> (!*{#GenericCaseDef},!*{#GenericDef},!u:{#CheckedTypeDef},!*{#ClassDef},!*{#DclModule},!.Heaps,!.CheckState)
checkGenericCaseDefs mod_index gen_case_defs generic_defs type_defs class_defs modules heaps cs
	| size gen_case_defs==0
		= (gen_case_defs, generic_defs, type_defs, class_defs, modules, heaps, cs)	
		# {cs_x} = cs
		# cs = {cs & cs_x = {cs_x & x_needed_modules = cs_x.x_needed_modules bitor cNeedStdGeneric}}
		= check_generic_case_defs 0 mod_index gen_case_defs generic_defs type_defs class_defs modules heaps cs
where
	check_generic_case_defs index mod_index gen_case_defs generic_defs type_defs class_defs modules heaps cs
		| index == size gen_case_defs
			= (gen_case_defs, generic_defs, type_defs, class_defs, modules, heaps, cs)	
			# (gen_case_defs, generic_defs, type_defs, class_defs, modules, heaps, cs) 
				= check_generic_case_def index mod_index gen_case_defs generic_defs type_defs class_defs modules heaps cs
			= check_generic_case_defs (inc index) mod_index gen_case_defs generic_defs type_defs class_defs modules heaps cs

	check_generic_case_def index mod_index gen_case_defs generic_defs type_defs class_defs modules heaps cs
		# (case_def=:{gc_pos,gc_type,gc_gcf}, gen_case_defs) = gen_case_defs![index]
		= case gc_gcf of
			GCF gc_ident gcf=:{gcf_gident}
				# cs = pushErrorAdmin (newPosition gc_ident gc_pos) cs
				# (gc_type, gc_type_cons, type_defs, modules, heaps, cs)
					= check_instance_type mod_index gc_type type_defs modules heaps cs
				# (generic_gi, cs) = get_generic_index gcf_gident mod_index cs
				| not cs.cs_error.ea_ok
					# cs = popErrorAdmin cs
					-> (gen_case_defs, generic_defs, type_defs, class_defs, modules, heaps, cs)
				# case_def = {case_def & gc_gcf=GCF gc_ident {gcf & gcf_generic = generic_gi}, gc_type=gc_type, gc_type_cons=gc_type_cons}
				# gen_case_defs = {gen_case_defs & [index] = case_def}
				# cs = popErrorAdmin cs
				-> (gen_case_defs, generic_defs, type_defs, class_defs, modules, heaps, cs)
			GCFS gcfs
				# cs = pushErrorAdmin (newPosition {id_name="derive generic superclass",id_info=nilPtr} gc_pos) cs
				# (gc_type, gc_type_cons, type_defs, modules, heaps, cs)
				 	= check_instance_type mod_index gc_type type_defs modules heaps cs 
				| not cs.cs_error.ea_ok
					# cs = popErrorAdmin cs
					-> (gen_case_defs, generic_defs, type_defs, class_defs, modules, heaps, cs)
				# (gcfs,cs) = check_generic_superclasses gcfs mod_index cs
				# cs = popErrorAdmin cs
				# case_def = {case_def & gc_gcf=GCFS gcfs, gc_type=gc_type, gc_type_cons=gc_type_cons}
				# gen_case_defs = {gen_case_defs & [index] = case_def}
				-> (gen_case_defs, generic_defs, type_defs, class_defs, modules, heaps, cs)
			GCFC _ gcfc_class_ident=:{id_info}
				# cs = pushErrorAdmin (newPosition {id_name="derive generic superclass",id_info=nilPtr} gc_pos) cs
				# (gc_type, gc_type_cons, type_defs, modules, heaps, cs)
				 	= check_instance_type mod_index gc_type type_defs modules heaps cs 
				| not cs.cs_error.ea_ok
					# cs = popErrorAdmin cs
					-> (gen_case_defs, generic_defs, type_defs, class_defs, modules, heaps, cs)
				# (entry,symbol_table) = readPtr id_info cs.cs_symbol_table
				# cs = {cs & cs_symbol_table=symbol_table}
				-> case entry.ste_kind of
					STE_Class
						# (class_context,class_defs) = class_defs![entry.ste_index].class_context
						# (gen_case_defs,cs) = check_generic_superclasses_of_case_def class_context index mod_index gc_type gc_type_cons gen_case_defs cs
						# cs = popErrorAdmin cs
						-> (gen_case_defs,generic_defs,type_defs,class_defs, modules,heaps,cs)
					STE_Imported STE_Class decl_index
	 					# (class_context,modules) = modules![decl_index].dcl_common.com_class_defs.[entry.ste_index].class_context
						# (gen_case_defs,cs) = check_generic_superclasses_of_case_def class_context index mod_index gc_type gc_type_cons gen_case_defs cs
						# cs = popErrorAdmin cs
						-> (gen_case_defs,generic_defs,type_defs,class_defs, modules,heaps,cs)
					_
						# cs = popErrorAdmin cs
						# cs = {cs & cs_error = checkErrorWithPosition gcfc_class_ident gc_pos "class undefined" cs.cs_error}
						-> (gen_case_defs,generic_defs,type_defs,class_defs, modules,heaps,cs)
				where
					check_generic_superclasses_of_case_def class_context index mod_index gc_type gc_type_cons gen_case_defs cs
						# gcfs = convert_generic_contexts class_context
						  (gcfs,cs) = check_generic_superclasses gcfs mod_index cs
						  case_def = {case_def & gc_gcf=GCFS gcfs, gc_type=gc_type, gc_type_cons=gc_type_cons}
						  gen_case_defs = {gen_case_defs & [index]=case_def}
						= (gen_case_defs,cs)

	convert_generic_contexts [{tc_class=TCGeneric {gtc_generic={glob_object={ds_ident}}}}:type_contexts]
		# gcf = {
			gcf_gident = ds_ident,
		 	gcf_generic = {gi_module=NoIndex,gi_index=NoIndex},
			gcf_arity = 0,
			gcf_generic_info = 0,
			gcf_body = GCB_None,
			gcf_kind = KindError,
			gcf_generic_instance_deps = AllGenericInstanceDependencies }
		# gcfs = convert_generic_contexts type_contexts
		= [!gcf:gcfs!]
	convert_generic_contexts [_:type_contexts]
		= convert_generic_contexts type_contexts
	convert_generic_contexts []
		= [!!]

	check_generic_superclasses [!gcf=:{gcf_gident}:gcfs!] mod_index cs
		# (generic_gi,cs) = get_generic_index gcf_gident mod_index cs
		| not cs.cs_error.ea_ok
			# (gcfs,cs) = check_generic_superclasses gcfs mod_index cs
			= ([!gcf:gcfs!],cs)
			# gcf = {gcf & gcf_generic = generic_gi}
			# (gcfs,cs) = check_generic_superclasses gcfs mod_index cs
			= ([!gcf:gcfs!],cs)
	check_generic_superclasses [!!] mod_index cs
		= ([!!],cs)

	check_instance_type module_index (TA type_cons []) type_defs modules heaps=:{hp_type_heaps} cs
		# (entry, cs_symbol_table) = readPtr type_cons.type_ident.id_info cs.cs_symbol_table
		# cs = {cs & cs_symbol_table = cs_symbol_table}
	  	# (type_index, type_module) = retrieveGlobalDefinition entry STE_Type module_index
		| type_index == NotFound
			# cs_error = checkError type_cons.type_ident "generic argument type undefined" cs.cs_error
 			= (TA type_cons [], TypeConsSymb type_cons, type_defs, modules,{heaps&hp_type_heaps = hp_type_heaps}, {cs&cs_error=cs_error})
		# (type_def, type_defs, modules) 
			= getTypeDef module_index {glob_module=type_module, glob_object=type_index} type_defs modules
		# type_cons = { type_cons & type_index = { glob_object = type_index, glob_module = type_module }}
		| type_synonym_with_arguments type_def.td_rhs type_def.td_arity
			# cs = {cs & cs_error = checkError type_def.td_ident "type synonym not allowed" cs.cs_error}
			= (TA type_cons [], TypeConsSymb type_cons, type_defs, modules,{heaps&hp_type_heaps = hp_type_heaps}, cs)
			= (TA type_cons [], TypeConsSymb type_cons, type_defs, modules,{heaps&hp_type_heaps = hp_type_heaps}, cs)
		where
			type_synonym_with_arguments (SynType _) arity
				= arity>0
			type_synonym_with_arguments _ _
				= False
	check_instance_type module_index (TB b) type_defs modules heaps cs
		= (TB b, TypeConsBasic b, type_defs, modules,heaps, cs) 
	check_instance_type module_index TArrow type_defs modules heaps cs
		= (TArrow, TypeConsArrow, type_defs, modules, heaps , cs) 		
// General instance ..
	check_instance_type module_index (TV tv) type_defs modules heaps=:{hp_type_heaps} cs
		# (tv_info_ptr, th_vars) = newPtr TVI_Empty hp_type_heaps.th_vars
		# tv = {tv & tv_info_ptr = tv_info_ptr}		
		= 	( TV tv, TypeConsVar tv, type_defs, modules
			, {heaps& hp_type_heaps = {hp_type_heaps & th_vars = th_vars}}, cs)
// .. General instance
	check_instance_type module_index ins_type type_defs modules heaps cs=:{cs_error}
		# cs_error = checkError {id_name="<>",id_info=nilPtr} "invalid generic type argument" cs_error
		= (ins_type, TypeConsArrow, type_defs, modules, heaps, {cs & cs_error=cs_error})

get_generic_index :: !Ident !Index !*CheckState -> (!GlobalIndex, !*CheckState)
get_generic_index {id_name,id_info} mod_index cs=:{cs_symbol_table}
	# (ste, cs_symbol_table) = readPtr id_info cs_symbol_table
	# cs = {cs & cs_symbol_table = cs_symbol_table}
	= case ste.ste_kind of
		STE_Generic _
			-> ({gi_module=mod_index,gi_index = ste.ste_index}, cs) 
		STE_Imported (STE_Generic _) imported_generic_module
			-> ({gi_module=imported_generic_module,gi_index = ste.ste_index}, cs)
		_	->	( {gi_module=NoIndex,gi_index = NoIndex}
				, {cs & cs_error = checkError id_name "undefined generic function" cs.cs_error})

convert_generic_instances :: !Int !Int !*{#GenericCaseDef} !*{#ClassDef} !*SymbolTable !*ErrorAdmin !*{#DclModule}
						-> (!.[FunDef],!*{#GenericCaseDef},!*{#ClassDef},!*SymbolTable,!*ErrorAdmin,!*{#DclModule})
convert_generic_instances gci next_fun_index gencase_defs class_defs symbol_table error dcl_modules
	| gci<size gencase_defs
		# (gencase_def,gencase_defs)=gencase_defs![gci]
		= case gencase_def of
			gc=:{gc_gcf=GCF gc_ident gcf=:{gcf_body=GCB_FunDef fun_def}}
				# gc = {gc & gc_gcf=GCF gc_ident {gcf & gcf_body = GCB_FunIndex next_fun_index}}
				  gencase_defs = {gencase_defs & [gci]=gc}
				  (fun_defs,gencase_defs,class_defs,symbol_table,error,dcl_modules)
					= convert_generic_instances (gci+1) (next_fun_index+1) gencase_defs class_defs symbol_table error dcl_modules
				-> ([fun_def : fun_defs],gencase_defs,class_defs,symbol_table,error,dcl_modules)
			gc=:{gc_pos, gc_type_cons, gc_gcf=GCF gc_ident gcf=:{gcf_body=GCB_None}}
				# fun_def =
					{ fun_ident = genericIdentToFunIdent gc_ident.id_name gc_type_cons
					, fun_arity = 0, fun_priority = NoPrio
					, fun_body = GeneratedBody, fun_type = No
					, fun_pos = gc_pos, fun_kind = FK_Unknown
					, fun_lifted = 0, fun_info = EmptyFunInfo
					}
				  gc = {gc & gc_gcf=GCF gc_ident {gcf & gcf_body = GCB_FunIndex next_fun_index}} 
				  gencase_defs = {gencase_defs & [gci]=gc}
				  (fun_defs,gencase_defs,class_defs,symbol_table,error,dcl_modules)
					= convert_generic_instances (gci+1) (next_fun_index+1) gencase_defs class_defs symbol_table error dcl_modules
				-> ([fun_def : fun_defs],gencase_defs,class_defs,symbol_table,error,dcl_modules)
			gc=:{gc_gcf=GCFC _ gcfc_class_ident=:{id_info},gc_type_cons,gc_pos}
				# (entry,symbol_table) = readPtr id_info symbol_table
				-> case entry.ste_kind of
					STE_Class
						# (class_context,class_defs) = class_defs![entry.ste_index].class_context
						-> convert_generic_instances_and_superclasses class_context gci next_fun_index gencase_defs class_defs symbol_table error dcl_modules
					STE_Imported STE_Class decl_index
	 					# (class_context,dcl_modules) = dcl_modules![decl_index].dcl_common.com_class_defs.[entry.ste_index].class_context
						-> convert_generic_instances_and_superclasses class_context gci next_fun_index gencase_defs class_defs symbol_table error dcl_modules
					_
						# error = checkErrorWithPosition gcfc_class_ident gc_pos "class undefined" error
						-> convert_generic_instances (gci+1) next_fun_index gencase_defs class_defs symbol_table error dcl_modules
			where
				convert_generic_instances_and_superclasses class_context gci next_fun_index gencase_defs class_defs symbol_table error dcl_modules
					# (gcfs,next_fun_index,new_fun_defs) = convert_generic_contexts class_context gc_type_cons gc_pos next_fun_index []
					  gc = {gc & gc_gcf=GCFS gcfs}
					  gencase_defs = {gencase_defs & [gci]=gc}
					  (fun_defs,gencase_defs,class_defs,symbol_table,error,dcl_modules)
						= convert_generic_instances (gci+1) next_fun_index gencase_defs class_defs symbol_table error dcl_modules
					= (new_fun_defs++fun_defs,gencase_defs,class_defs,symbol_table,error,dcl_modules)
		= ([],gencase_defs,class_defs,symbol_table,error,dcl_modules)
	where
		convert_generic_contexts [{tc_class=TCGeneric {gtc_generic={glob_object={ds_ident}}}}:type_contexts] type_cons pos next_fun_index new_fun_defs
			# fun_def = {
				fun_ident = genericIdentToFunIdent ds_ident.id_name type_cons,
				fun_arity = 0, fun_priority = NoPrio,
				fun_body = GeneratedBody, fun_type = No,
				fun_pos = pos, fun_kind = FK_Unknown,
				fun_lifted = 0, fun_info = EmptyFunInfo
				}
			# gcf = {
				gcf_gident = ds_ident,
			 	gcf_generic = {gi_module=NoIndex,gi_index=NoIndex},
				gcf_arity = 0,
				gcf_generic_info = 0,
				gcf_body = GCB_FunIndex next_fun_index,
				gcf_kind = KindError,
				gcf_generic_instance_deps = AllGenericInstanceDependencies }
			# (gcfs,next_fun_index,new_fun_defs) = convert_generic_contexts type_contexts type_cons pos (next_fun_index+1) new_fun_defs
			= ([!gcf:gcfs!],next_fun_index,[fun_def:new_fun_defs])
		convert_generic_contexts [_:type_contexts] type_cons pos next_fun_index new_fun_defs
			= convert_generic_contexts type_contexts type_cons pos next_fun_index new_fun_defs
		convert_generic_contexts [] type_cons pos next_fun_index new_fun_defs
			= ([!!],next_fun_index,new_fun_defs)

create_gencase_funtypes :: !Index !*{#GenericCaseDef} !*Heaps
			-> (!Index,![FunType],!*{#GenericCaseDef},!*Heaps)
create_gencase_funtypes fun_index gencase_defs heaps
	#! (fun_index, new_funs, gencase_defs, hp_var_heap) 
		= create_funs 0 fun_index gencase_defs heaps.hp_var_heap
	= (fun_index, new_funs, gencase_defs, {heaps & hp_var_heap = hp_var_heap}) 
where
	create_funs gc_index fun_index gencase_defs hp_var_heap
		| gc_index == size gencase_defs
			= (fun_index, [], gencase_defs, hp_var_heap)
		# (gencase_def,gencase_defs) = gencase_defs![gc_index]
		= case gencase_def of
			{gc_gcf=GCF gc_ident gcf=:{gcf_body=GCB_MacroIndex macro_index},gc_pos,gc_type_cons}
				# gencase_def & gc_gcf=GCF gc_ident {gcf & gcf_body = GCB_FunAndMacroIndex fun_index macro_index}
				  gencase_defs & [gc_index] = gencase_def 
				  (fun,hp_var_heap) = create_gencase_function_type gc_ident gc_type_cons gc_pos hp_var_heap
				  (fun_index,funs,gencase_defs,hp_var_heap)
					= create_funs (gc_index+1) (fun_index+1) gencase_defs hp_var_heap
				-> (fun_index, [fun:funs], gencase_defs, hp_var_heap)
			{gc_gcf=GCF gc_ident gcf,gc_pos,gc_type_cons}
				# gencase_def & gc_gcf=GCF gc_ident {gcf & gcf_body = GCB_FunIndex fun_index}
				  gencase_defs & [gc_index] = gencase_def
				  (fun,hp_var_heap) = create_gencase_function_type gc_ident gc_type_cons gc_pos hp_var_heap
				  (fun_index,funs,gencase_defs,hp_var_heap)
					= create_funs (gc_index+1) (fun_index+1) gencase_defs hp_var_heap
				-> (fun_index, [fun:funs], gencase_defs, hp_var_heap)
			{gc_gcf=GCFS gcfs,gc_pos,gc_type_cons}
				# (gcfs,superclass_funs,fun_index,hp_var_heap)
					= create_functions_for_generic_superclasses gcfs gc_type_cons gc_pos fun_index hp_var_heap
				  gencase_def & gc_gcf=GCFS gcfs
				  gencase_defs & [gc_index] = gencase_def 
				  (fun_index,funs,gencase_defs,hp_var_heap) 
					= create_funs (gc_index+1) fun_index gencase_defs hp_var_heap
				-> (fun_index,superclass_funs++funs,gencase_defs,hp_var_heap)
		where
			create_functions_for_generic_superclasses [!gcf=:{gcf_gident}:gcfs!] gc_type_cons gc_pos fun_index hp_var_heap
				# (fun,hp_var_heap) = create_gencase_function_type gcf_gident gc_type_cons gc_pos hp_var_heap
	  			# gcf={gcf & gcf_body = GCB_FunIndex fun_index}
				# (gcfs,superclass_funs,fun_index,hp_var_heap)
					= create_functions_for_generic_superclasses gcfs gc_type_cons gc_pos (fun_index+1) hp_var_heap
				= ([!gcf:gcfs!],[fun:superclass_funs],fun_index,hp_var_heap)
			create_functions_for_generic_superclasses [!!] gc_type_cons gc_pos fun_index hp_var_heap
				= ([!!],[],fun_index,hp_var_heap)

	create_gencase_function_type {id_name} gc_type_cons gc_pos var_heap
		#! fun_ident = genericIdentToFunIdent id_name gc_type_cons
	 	#! (var_info_ptr, var_heap) = newPtr VI_Empty var_heap
		#! fun =
			{ ft_ident = fun_ident
			, ft_arity = 0
			, ft_priority = NoPrio
			, ft_type = {st_vars=[],st_attr_vars=[],st_arity=0,st_args=[],st_result={at_type=TE,at_attribute=TA_Multi},st_attr_env=[],st_context=[],st_args_strictness=NotStrict}
			, ft_pos = gc_pos
			, ft_specials = FSP_None
			, ft_type_ptr = var_info_ptr
			}
		= (fun,var_heap)

NewEntry symbol_table symb_ptr def_kind def_index level previous :==
	 symbol_table <:= (symb_ptr,{  ste_kind = def_kind, ste_index = def_index, ste_def_level = level, ste_previous = previous, ste_doc = No })

getTypeDef :: !Index !(Global Index) !v:{#CheckedTypeDef} !w:{#DclModule}
		-> (!CheckedTypeDef, !v:{#CheckedTypeDef}, !w:{#DclModule})
getTypeDef x_main_dcl_module_n {glob_module,glob_object} type_defs modules
	| glob_module==x_main_dcl_module_n
		# (type_def, type_defs) = type_defs![glob_object]
		= (type_def, type_defs, modules)
	# (type_def, modules) = modules![glob_module].dcl_common.com_type_defs.[glob_object]
	= (type_def, type_defs, modules)
