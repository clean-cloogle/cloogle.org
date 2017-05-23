implementation module frontend

import scanner, parse, postparse, check, type, trans, partition, convertcases, overloading, utilities, convertDynamics,
		convertimportedtypes, compilerSwitches, analtypes, generics1,
		typereify, compare_types
from CoclSystemDependent import DirectorySeparator

instance == FrontEndPhase where
	(==) a b
		=	equal_constructor a b

frontSyntaxTree cached_dcl_macros cached_dcl_mods main_dcl_module_n predef_symbols hash_table files error io out tcl_file icl_mod dcl_mods fun_defs components array_instances heaps
	:== (Yes {
				fe_icl = {icl_mod & icl_functions=fun_defs }
			,	fe_dcls = dcl_mods
			,	fe_components = components
			,	fe_arrayInstances = array_instances
			},cached_dcl_macros,cached_dcl_mods,main_dcl_module_n,predef_symbols,hash_table,files,error,io,out,tcl_file,heaps
		)

frontEndInterface :: !(Optional (*File,{#Char},{#Char})) !FrontEndOptions !Ident !SearchPaths !{#DclModule} !*{#*{#FunDef}} !(Optional Bool) !*PredefinedSymbols !*HashTable (ModTimeFunction *Files) !*Files !*File !*File !*File !(Optional *File) !*Heaps
  	-> ( !Optional *FrontEndSyntaxTree,!*{#*{#FunDef}},!{#DclModule},!Int,!*PredefinedSymbols, !*HashTable, !*Files, !*File, !*File, !*File, !Optional *File, !*Heaps)
frontEndInterface opt_file_dir_time options mod_ident search_paths cached_dcl_modules cached_dcl_macros list_inferred_types predef_symbols hash_table modtimefunction files error io out tcl_file heaps
	| case opt_file_dir_time of No -> True; _ -> False
		# error = moduleCouldNotBeImportedError True mod_ident NoPos error
		= (No,{},{},0,predef_symbols, hash_table, files, error, io, out, tcl_file, heaps)
	# (Yes (mod_file,mod_dir,mod_time)) = opt_file_dir_time
	# (ok,dynamic_type_used,mod,hash_table,error,files)
		= wantModule mod_file mod_time cWantIclFile mod_ident NoPos options.feo_generics hash_table error files
	| not ok
		= (No,{},{},0,predef_symbols, hash_table, files, error, io, out, tcl_file, heaps)
	# cached_module_idents = [dcl_mod.dcl_name \\ dcl_mod<-:cached_dcl_modules]
	#! support_dynamics = case tcl_file of Yes _ -> True ; No -> False
	# (ok, mod, global_fun_range, mod_functions, optional_dcl_mod, modules, dcl_module_n_in_cache,hash_table, error, files)
		= scanModule mod cached_module_idents options.feo_generics support_dynamics hash_table error search_paths modtimefunction files

//	# hash_table = {hash_table & hte_entries={}}
	# hash_table = remove_icl_symbols_from_hash_table hash_table

	| not ok
		= (No,{},{},0,predef_symbols, hash_table, files, error, io, out, tcl_file, heaps)
  	# symbol_table = hash_table.hte_symbol_heap
  	#! n_cached_dcl_modules=size cached_dcl_modules


  	# (ok, icl_mod, dcl_mods, groups, cached_dcl_macros,main_dcl_module_n,heaps, predef_symbols, symbol_table, error, directly_imported_dcl_modules)
  	  	= checkModule mod global_fun_range mod_functions support_dynamics dynamic_type_used dcl_module_n_in_cache optional_dcl_mod modules cached_dcl_modules cached_dcl_macros predef_symbols symbol_table error heaps

	  hash_table = { hash_table & hte_symbol_heap = symbol_table}

	| not ok
		= (No,{},dcl_mods,main_dcl_module_n,predef_symbols, hash_table, files, error, io, out, tcl_file, heaps)

	#! (icl_functions,icl_mod) = select_and_remove_icl_functions_from_record icl_mod
		with
			select_and_remove_icl_functions_from_record :: !*IclModule -> (!.{#FunDef},!.IclModule)
			select_and_remove_icl_functions_from_record icl_mod=:{icl_functions} = (icl_functions,{icl_mod & icl_functions={}})

	# {icl_common,icl_function_indices,icl_name,icl_import,icl_qualified_imports,icl_imported_objects,icl_foreign_exports,icl_used_module_numbers} = icl_mod
/*
	  (_,f,files) = fopen "components" FWriteText files
	  (groups, icl_functions, f) = showGroups groups 0 True icl_functions f
	/*	
	  (n_functions,icl_functions) = usize icl_functions
	  (icl_functions,f) = showFunctions {ir_from=0,ir_to=n_functions} icl_functions f
	  (cached_dcl_macros,f) = showMacros cached_dcl_macros f
	*/
	  (ok,files) = fclose f files
	| ok<>ok
		= abort "";
*/

//	# dcl_mods = {{dcl_mod & dcl_declared={dcls_import={},dcls_local=[],dcls_local_for_import={},dcls_explicit={}}}\\ dcl_mod<-:dcl_mods}

	# type_heaps = heaps.hp_type_heaps
	  fun_defs = icl_functions

	| options.feo_up_to_phase == FrontEndPhaseCheck
		# array_instances = {ali_array_first_instance_indices=[],ali_list_first_instance_indices=[],ali_tail_strict_list_first_instance_indices=[],ali_instances_range={ir_from=0,ir_to=0}}
		=	frontSyntaxTree cached_dcl_macros dcl_mods main_dcl_module_n
							predef_symbols hash_table files error io out tcl_file icl_mod dcl_mods fun_defs (groups_to_components groups) array_instances heaps

	# error_admin = {ea_file = error, ea_loc = [], ea_ok = True }
/*
	# (ti_common_defs, dcl_mods) = get_common_defs dcl_mods
	  ti_common_defs = { ti_common_defs & [main_dcl_module_n] = icl_common }
*/

	# (cached_dcl_mods, dcl_mods) = copy_dcl_modules dcl_mods
	
	# (type_groups, ti_common_defs, td_infos, icl_common, dcl_mods, type_heaps, error_admin)
			= partionateAndExpandTypes icl_used_module_numbers main_dcl_module_n icl_common dcl_mods type_heaps error_admin
//	  ti_common_defs = { ti_common_defs & [main_dcl_module_n] = icl_common }
//	# (td_infos, th_vars, error_admin) = analyseTypeDefs ti_common_defs type_groups td_infos type_heaps.th_vars error_admin
	  ({com_type_defs}, ti_common_defs) = replace ti_common_defs main_dcl_module_n icl_common

	# {hp_var_heap,hp_expression_heap} = heaps
	#! n_types_with_type_functions = size ti_common_defs.[main_dcl_module_n].com_type_defs
	#! n_constructors_with_type_functions = size ti_common_defs.[main_dcl_module_n].com_cons_defs
	#! ea_ok = error_admin.ea_ok
	# (fun_defs, predef_symbols, hp_var_heap, type_heaps)
		= if (support_dynamics && ea_ok)
			(buildTypeFunctions main_dcl_module_n fun_defs ti_common_defs predef_symbols hp_var_heap type_heaps)
			(fun_defs, predef_symbols, hp_var_heap, type_heaps)
	# (td_infos, th_vars, error_admin)
		= analyseTypeDefs ti_common_defs type_groups com_type_defs main_dcl_module_n td_infos type_heaps.th_vars error_admin
	# (class_infos, td_infos, th_vars, error_admin)
		= determineKindsOfClasses icl_used_module_numbers ti_common_defs td_infos th_vars error_admin

	# icl_global_functions=icl_function_indices.ifi_global_function_indices

	# (fun_defs, dcl_mods, td_infos, th_vars, hp_expression_heap, gen_heap, error_admin)
		= checkKindsOfCommonDefsAndFunctions n_cached_dcl_modules main_dcl_module_n icl_used_module_numbers
				(icl_global_functions++[icl_function_indices.ifi_local_function_indices])
				ti_common_defs fun_defs dcl_mods td_infos class_infos th_vars hp_expression_heap heaps.hp_generic_heap error_admin

      type_heaps = { type_heaps & th_vars = th_vars }

	# heaps = { heaps & hp_type_heaps = type_heaps, hp_expression_heap = hp_expression_heap, hp_generic_heap = gen_heap, hp_var_heap=hp_var_heap }

	| not error_admin.ea_ok
		= (No,{},dcl_mods,main_dcl_module_n,predef_symbols, hash_table, files, error_admin.ea_file, io, out, tcl_file, heaps)

	# (saved_main_dcl_common, ti_common_defs) = replace {#dcl_common \\ {dcl_common}<-:dcl_mods} main_dcl_module_n icl_common

	#! (ti_common_defs, groups, fun_defs, td_infos, heaps, hash_table, predef_symbols, dcl_mods, cached_dcl_macros, error_admin)
		= case options.feo_generics of
			True
				-> convertGenerics main_dcl_module_n icl_used_module_numbers ti_common_defs groups fun_defs
									td_infos heaps hash_table predef_symbols dcl_mods cached_dcl_macros error_admin
			False
				-> (ti_common_defs, groups, fun_defs, td_infos, heaps, hash_table, predef_symbols, dcl_mods, cached_dcl_macros, error_admin)

	# (icl_common, ti_common_defs) = replace {#x \\ x<-:ti_common_defs} main_dcl_module_n saved_main_dcl_common		

	# dcl_mods = { {dcl_mod & dcl_common = common} \\ dcl_mod <-: dcl_mods & common <-: ti_common_defs }

	# icl_mod = {icl_mod & icl_common = icl_common} 
		
	# error = error_admin.ea_file
/*
	# (_,f,files) = fopen "modules" FWriteText files
	# (dcl_mods, f) = showDclModules dcl_mods f
	  (ok,files) = fclose f files
	| ok<>ok
		= abort "";
*/
/*
	# (_,genout,files) = fopen "genout" FWriteText files
	# (n_fun_defs,fun_defs) = usize fun_defs
	# genout = show_class_members icl_mod.icl_common genout 
	# (groups, fun_defs, genout) = showGroups groups 0 True fun_defs genout
	# (ok,files) = fclose genout files
	| not ok = abort "could not write genout"
*/
	#! ok = error_admin.ea_ok
	| not ok
		= (No,{},{},main_dcl_module_n,predef_symbols, hash_table, files, error, io, out, tcl_file, heaps)

	# (ok, fun_defs, array_instances, common_defs, imported_funs, type_def_infos, heaps, predef_symbols, error,out)
		= typeProgram groups main_dcl_module_n fun_defs icl_function_indices.ifi_specials_indices list_inferred_types icl_common icl_import icl_qualified_imports dcl_mods icl_used_module_numbers td_infos heaps predef_symbols error out

	| not ok
		= (No,{},{},main_dcl_module_n,predef_symbols, hash_table, files, error, io, out, tcl_file, heaps)

	# icl_gencase_indices = icl_function_indices.ifi_gencase_indices
	# icl_function_indices = {icl_function_indices & ifi_gencase_indices = icl_gencase_indices }

	# (fun_def_size, fun_defs) = usize fun_defs
	# (components, fun_defs)
		= partitionateFunctions fun_defs (icl_global_functions++icl_function_indices.ifi_instance_indices
											++[icl_function_indices.ifi_specials_indices
											  : icl_gencase_indices++icl_function_indices.ifi_type_function_indices])
		
	| options.feo_up_to_phase == FrontEndPhaseTypeCheck
		=	frontSyntaxTree cached_dcl_macros cached_dcl_mods main_dcl_module_n
							predef_symbols hash_table files error io out tcl_file icl_mod dcl_mods fun_defs components array_instances heaps

	# (dcl_types, components, fun_defs, predef_symbols, var_heap, type_heaps, expression_heap, tcl_file)
  		= convertDynamicPatternsIntoUnifyAppls common_defs main_dcl_module_n dcl_mods icl_mod directly_imported_dcl_modules
			n_types_with_type_functions n_constructors_with_type_functions
				  components fun_defs predef_symbols heaps.hp_var_heap heaps.hp_type_heaps heaps.hp_expression_heap tcl_file

	| options.feo_up_to_phase == FrontEndPhaseConvertDynamics
		# heaps = {hp_var_heap=var_heap, hp_type_heaps=type_heaps, hp_expression_heap=expression_heap, hp_generic_heap=newHeap}
		=	frontSyntaxTree cached_dcl_macros cached_dcl_mods main_dcl_module_n
							predef_symbols hash_table files error io out tcl_file icl_mod dcl_mods fun_defs components array_instances heaps

	#  (stdStrictLists_module_n,predef_symbols) = get_StdStrictLists_module_n predef_symbols

	# (cleanup_info, acc_args, components, fun_defs, var_heap, expression_heap)
		 = analyseGroups common_defs imported_funs array_instances.ali_instances_range main_dcl_module_n stdStrictLists_module_n components fun_defs var_heap expression_heap

	# (def_max, acc_args)		= usize acc_args
	# (def_min, fun_defs)		= usize fun_defs

	  (components, fun_defs, dcl_types, used_conses, var_heap, type_heaps, expression_heap, error, predef_symbols)
	  	= transformGroups cleanup_info main_dcl_module_n stdStrictLists_module_n def_min def_max components fun_defs acc_args common_defs imported_funs dcl_types type_def_infos var_heap type_heaps expression_heap options.feo_fusion error predef_symbols

	# error_admin = {ea_file = error, ea_loc = [], ea_ok = True }
	# {dcl_instances,dcl_specials,dcl_gencases,dcl_type_funs} = dcl_mods.[main_dcl_module_n]
	# (start_function_index,predef_symbols) = get_index_of_start_rule main_dcl_module_n predef_symbols

	# (error_admin,predef_symbols,fun_defs)
		= checkForeignExportedFunctionTypes icl_foreign_exports error_admin predef_symbols fun_defs	
	
	# [icl_exported_global_functions,icl_not_exported_global_functions:_] = icl_global_functions
	# exported_global_functions = case start_function_index of
				NoIndex	-> [icl_exported_global_functions]
				sri		-> [{ir_from=sri,ir_to=inc sri},icl_exported_global_functions]
	# exported_functions = exported_global_functions ++  [dcl_instances,dcl_specials,dcl_gencases,dcl_type_funs]
	# (components, fun_defs, predef_symbols, var_heap, expression_heap, error_admin) 
		= case options.feo_strip_unused of
			True -> partitionateFunctions` fun_defs exported_functions
						main_dcl_module_n def_min def_max predef_symbols var_heap expression_heap error_admin
			_ 
				-> case options.feo_fusion of
					True
						# (fun_defs,predef_symbols,var_heap,expression_heap,error_admin)
								= stripStrictLets fun_defs predef_symbols var_heap expression_heap error_admin
						-> (components, fun_defs, predef_symbols, var_heap, expression_heap, error_admin)
					_	-> (components, fun_defs, predef_symbols, var_heap, expression_heap, error_admin)

	# error = error_admin.ea_file
	| not error_admin.ea_ok
		# heaps = {hp_var_heap=var_heap, hp_type_heaps=type_heaps, hp_expression_heap=expression_heap,hp_generic_heap=heaps.hp_generic_heap}
		= (No,{},{},main_dcl_module_n,predef_symbols, hash_table, files, error, io, out, tcl_file, heaps)

	| options.feo_up_to_phase == FrontEndPhaseTransformGroups
		# heaps = {hp_var_heap=var_heap, hp_type_heaps=type_heaps, hp_expression_heap=expression_heap,hp_generic_heap=heaps.hp_generic_heap}
		=	frontSyntaxTree cached_dcl_macros cached_dcl_mods main_dcl_module_n
							predef_symbols hash_table files error io out tcl_file icl_mod dcl_mods fun_defs components array_instances heaps

	# (dcl_types, used_conses, var_heap, type_heaps) = convertIclModule main_dcl_module_n common_defs dcl_types used_conses var_heap type_heaps
	# (dcl_types, used_conses, var_heap, type_heaps) = convertDclModule main_dcl_module_n dcl_mods common_defs dcl_types used_conses var_heap type_heaps

//	  (components, fun_defs, out) = showComponents components 0 False fun_defs out

	| options.feo_up_to_phase == FrontEndPhaseConvertModules
		# heaps = {hp_var_heap=var_heap, hp_type_heaps=type_heaps, hp_expression_heap=expression_heap,hp_generic_heap=heaps.hp_generic_heap}
		=	frontSyntaxTree cached_dcl_macros cached_dcl_mods main_dcl_module_n
							predef_symbols hash_table files error io out tcl_file icl_mod dcl_mods fun_defs components array_instances heaps

//	# (components, fun_defs, out) = showComponents components 0 False fun_defs out
	# (used_funs, components, fun_defs, dcl_types, used_conses, var_heap, type_heaps, expression_heap)
	  		= convertCasesOfFunctions components main_dcl_module_n imported_funs common_defs fun_defs dcl_types used_conses
					var_heap type_heaps expression_heap
	#!  (dcl_types, type_heaps, var_heap)
			= convertImportedTypeSpecifications main_dcl_module_n dcl_mods imported_funs common_defs used_conses used_funs dcl_types type_heaps var_heap		
//	# (components, fun_defs, error)	= showTypes components 0 fun_defs error
//	# (dcl_mods, out) = showDclModules dcl_mods out
//	# (components, fun_defs, out) = showComponents components 0 False fun_defs out

/*
	# (_,f,files) = fopen "components2" FWriteText files
	  (components, fun_defs, f) = showComponents components 0 False fun_defs f
	  (ok,files) = fclose f files
	| ok<>ok
		= abort "";
*/

	# heaps = {hp_var_heap = var_heap, hp_expression_heap=expression_heap, hp_type_heaps=type_heaps,hp_generic_heap=heaps.hp_generic_heap}
	# 	fe ={	fe_icl = {icl_functions=fun_defs, icl_function_indices=icl_function_indices, icl_common=icl_common,
						 icl_import=icl_import, icl_qualified_imports=icl_qualified_imports, icl_imported_objects=icl_imported_objects,
						 icl_foreign_exports=icl_foreign_exports,icl_name=icl_name,icl_used_module_numbers=icl_used_module_numbers,
						 icl_modification_time=icl_mod.icl_modification_time }
			,	fe_dcls = dcl_mods
			,	fe_components = components
			,	fe_arrayInstances = array_instances
			}

	# cached_dcl_macros = clear_group_indices_of_macros cached_dcl_macros
	= (Yes fe,cached_dcl_macros,cached_dcl_mods,main_dcl_module_n,predef_symbols,hash_table,files,error,io,out,tcl_file,heaps)
	where
		copy_dcl_modules :: !*{#DclModule} -> *(!*{#DclModule},!*{#DclModule})
		copy_dcl_modules dcl_mods
			#! nr_of_dcl_mods = size dcl_mods
			= arrayCopyBegin dcl_mods nr_of_dcl_mods

		clear_group_indices_of_macros :: !*{#*{#FunDef}} -> *{#*{#FunDef}}
		clear_group_indices_of_macros cached_dcl_macros
			= clear_group_indices1 0 cached_dcl_macros
		where
			clear_group_indices1 :: !Int !*{#*{#u:FunDef}} -> *{#*{#FunDef}}
			clear_group_indices1 i cached_dcl_macros
				| i==size cached_dcl_macros
					= cached_dcl_macros
					# (cached_dcl_macros_i,cached_dcl_macros) = cached_dcl_macros![i]
					# cached_dcl_macros_i = clear_group_indices2 0 cached_dcl_macros_i
					# cached_dcl_macros = {cached_dcl_macros & [i]=cached_dcl_macros_i}
					= clear_group_indices1 (i+1) cached_dcl_macros

			clear_group_indices2 j cached_dcl_macros_i
				| j==size cached_dcl_macros_i
					= cached_dcl_macros_i
					# cached_dcl_macros_i = {cached_dcl_macros_i & [j].fun_info.fi_group_index= (-1)}
					= clear_group_indices2 (j+1) cached_dcl_macros_i	

		get_StdStrictLists_module_n predef_symbols
  			# (pre_mod,predef_symbols) = predef_symbols![PD_StdStrictLists]
			| pre_mod.pds_def<>NoIndex
				= (pre_mod.pds_def,predef_symbols)
				= (-1,predef_symbols)

		get_index_of_start_rule main_dcl_module_n predef_symbols
			# ({pds_def, pds_module}, predef_symbols) = predef_symbols![PD_Start]
			| pds_def <> NoIndex && pds_module == main_dcl_module_n
				= (pds_def, predef_symbols)
				= (NoIndex, predef_symbols)

	groups_to_components groups
		= {{component_members=group_members_to_component_members group_members} \\ {group_members}<-:groups}
	where
		group_members_to_component_members [e:l] = ComponentMember e (group_members_to_component_members l)
		group_members_to_component_members [] = NoComponentMembers

newSymbolTable :: !Int -> *{# SymbolTableEntry}
newSymbolTable size
	= createArray size {  ste_index = NoIndex, ste_def_level = -1, ste_kind = STE_Empty, ste_previous = abort "PreviousPlaceholder", ste_doc = No}

showFunctions :: !IndexRange !*{# FunDef} !*File  -> (!*{# FunDef},!*File)
showFunctions {ir_from, ir_to} fun_defs file
	= iFoldSt show_function ir_from ir_to (fun_defs, file)
where
	show_function fun_index (fun_defs, file)
		# (fd, fun_defs) = fun_defs![fun_index]
		= (fun_defs, file <<< fun_index <<< fd <<< '\n')

showMacros :: !*{#*{#FunDef}} !*File -> (!*{#*{#FunDef}},!*File)
showMacros macro_defs file
	#! n_dcl_modules=size macro_defs
	= iFoldSt showMacrosInModule 0 n_dcl_modules (macro_defs,file)

showMacrosInModule :: !Int (!*{#*{#FunDef}},!*File) -> (!*{#*{#FunDef}},!*File)
showMacrosInModule dcl_index (macro_defs,file)
	# file=file <<< dcl_index <<< '\n'
	#! n_macros=size macro_defs.[dcl_index]
	= iFoldSt show_macro 0 n_macros (macro_defs,file)
	where
		show_macro macro_index (macro_defs, file)
			# (macro,macro_defs) = macro_defs![dcl_index,macro_index]
			= (macro_defs, file <<< macro_index <<< macro <<< '\n')

showGroups :: !u:{! Group} !Int !Bool !*{# FunDef} !*File  -> (!u:{! Group}, !*{# FunDef},!*File)
showGroups comps comp_index show_types fun_defs file
	| comp_index >= size comps
		= (comps, fun_defs, file)
		# (comp, comps) = comps![comp_index]
		# (fun_defs, file) = show_group comp.group_members show_types fun_defs (file <<< "component " <<< comp_index <<< '\n')
		= showGroups comps (inc comp_index) show_types fun_defs file
where
	show_group [] show_types fun_defs file
		= (fun_defs, file <<< '\n')
	show_group [fun:funs] show_types fun_defs file
		# (fun_def, fun_defs) = fun_defs![fun]
		# file=file<<<fun<<<'\n'
		| show_types
			= show_group funs show_types fun_defs (file <<< fun_def.fun_type <<< '\n' <<< fun_def)
			= show_group funs show_types fun_defs (file <<< fun_def)
	//		= show_group funs show_types fun_defs (file <<< fun_def.fun_ident)

showComponents :: !u:{!Component} !Int !Bool !*{#FunDef} !*File  -> (!u:{!Component}, !*{#FunDef},!*File)
showComponents comps comp_index show_types fun_defs file
	| comp_index >= size comps
		= (comps, fun_defs, file)
		# (comp, comps) = comps![comp_index]
		# (fun_defs, file) = show_component comp.component_members show_types fun_defs (file <<< "component " <<< comp_index <<< '\n')
		= showComponents comps (inc comp_index) show_types fun_defs file
where
	show_component (ComponentMember fun funs) show_types fun_defs file
		# (fun_def, fun_defs) = fun_defs![fun]
		# file=file<<<"function "<<<fun<<<'\n'
		| show_types
			= show_component funs show_types fun_defs (file <<< fun_def.fun_type <<< '\n' <<< fun_def)
			= show_component funs show_types fun_defs (file <<< fun_def)
	show_component (GeneratedComponentMember fun _ funs) show_types fun_defs file
//		# file=file<<<"generated function "<<<fun<<<'\n'
//		= show_component funs show_types fun_defs file

		# (fun_def, fun_defs) = fun_defs![fun]
		# file=file<<<"generated function "<<<fun<<<'\n'
		| show_types
			= show_component funs show_types fun_defs (file <<< fun_def.fun_type <<< '\n' <<< fun_def)
			= show_component funs show_types fun_defs (file <<< fun_def)

	show_component NoComponentMembers show_types fun_defs file
		= (fun_defs, file <<< '\n')

show_class_members :: !CommonDefs !*File -> *File
show_class_members {com_member_defs} file
	= show_class_members 0 com_member_defs file
	where
	show_class_members i member_a file
		| i<size member_a
		# ({me_ident,me_type},member_a) = member_a![i]
		# properties = { form_properties = cAttributed bitor cAnnotated, form_attr_position = No }
		# file = file <<< me_ident <<< " :: " <:: (properties, me_type, No) <<< '\n'
		= show_class_members (i+1) member_a file
		= file 

showTypes :: !*{! Group} !Int !*{# FunDef} !*File  -> (!*{! Group}, !*{# FunDef},!*File)
showTypes comps comp_index fun_defs file
	| comp_index >= size comps
		= (comps, fun_defs, file)
		# (comp, comps) = comps![comp_index]
		# (fun_defs, file) = show_types comp.group_members fun_defs (file <<< "component " <<< comp_index <<< '\n')
		= showTypes comps (inc comp_index) fun_defs file
where
	show_types [] fun_defs file
		= (fun_defs, file <<< '\n')
	show_types [fun:funs] fun_defs file
		# (fun_def, fun_defs) = fun_defs![fun]
		# properties = { form_properties = cAttributed bitor cAnnotated, form_attr_position = No }
		  (Yes ftype) = fun_def.fun_type
		= show_types funs fun_defs (file <<< fun_def.fun_ident <<< " :: " <:: (properties, ftype, No) <<< '\n' )

showDclModules :: !u:{#DclModule} !*File -> (!u:{#DclModule}, !*File)
showDclModules dcl_mods file
	= show_dcl_mods 0 dcl_mods file
where
	show_dcl_mods mod_index dcl_mods file
		# (size_dcl_mods, dcl_mods) = usize dcl_mods
		| mod_index == size_dcl_mods
			= (dcl_mods, file)
			# (dcl_mod, dcl_mods) = dcl_mods![mod_index]
			# file =  show_dcl_mod dcl_mod file
			= show_dcl_mods (mod_index+1) dcl_mods file

	show_dcl_mod {dcl_name, dcl_functions, dcl_common} file
		# file = file <<< dcl_name <<< ":\n"
		# file = show_class_members dcl_common file
		# file = show_dcl_functions 0 dcl_functions file
		= file <<< "\n"

	show_dcl_functions fun_index dcl_functions file					 				
		| fun_index == size dcl_functions
			= file
		| otherwise
			# file = show_dcl_function dcl_functions.[fun_index] file
			= show_dcl_functions (inc fun_index) dcl_functions file 

	show_dcl_function {ft_ident, ft_type} file
		= file <<< ft_ident <<< " :: " <<< ft_type <<< "\n"			
		
instance == ListTypesKind where
	(==) ListTypesNone ListTypesNone
		=	True
	(==) ListTypesInferred ListTypesInferred
		=	True
	(==) ListTypesStrictExports ListTypesStrictExports
		=	True
	(==) ListTypesAll ListTypesAll
		=	True
	(==) _ _
		=	False
