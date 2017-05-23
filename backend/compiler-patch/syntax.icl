implementation module syntax

import StdEnv, compare_constructor
import scanner, general, Heap, typeproperties, utilities, compare_types
import IndexType
from containers import ::NumberSet
from convertcases import :: LetVarInfo, :: LetExpressionInfo, :: RefCountsInCase, :: SplitsInCase

instance toString Ident
where toString {id_name} = id_name

instance == GenericDependency
	where
		(==) gen_dep1 gen_dep2
			= gen_dep1.gd_index == gen_dep2.gd_index && gen_dep1.gd_vars == gen_dep2.gd_vars

instance toString Import
where toString {import_module} = toString import_module

instance == FunctionOrMacroIndex
	where
		(==) (FunctionOrIclMacroIndex f1) (FunctionOrIclMacroIndex f2) = f1==f2
		(==) (DclMacroIndex m1 f1) (DclMacroIndex m2 f2) = m1==m2 && f1==f2
		(==) _ _ = False

/* Used for hashing identifiers */

class needs_brackets a :: a -> Bool

instance == BoundVar
where
	(==) varid1 varid2
		= varid1.var_ident == varid2.var_ident

instance == Ident
where
	(==) id1 id2
		= id1.id_info == id2.id_info

instance needs_brackets AType
where
	needs_brackets {at_type}
		= needs_brackets at_type

instance needs_brackets Type
where
	needs_brackets (TA {type_arity} _)
		= type_arity > 0
	needs_brackets (TAS {type_arity} _ _)
		= type_arity > 0
	needs_brackets (_ --> _)
		= True
	needs_brackets (_ :@: _)
		= True
	needs_brackets (TArrow1 _)
		= True		
/*	needs_brackets (TFA _ _)
		= True
*/	needs_brackets _
		= False

instance needs_brackets Expression
where
	needs_brackets (App {app_args})
		= not (isEmpty app_args)
	needs_brackets (_ @ _)
		= True
	needs_brackets (Let _)
		= True
	needs_brackets (Case _)
		= True
	needs_brackets (Selection _ _ _)
		= True
	needs_brackets _
		= False

instance needs_brackets a
where
	needs_brackets _ = False

instance toString BasicType  where
	toString BT_Int = "Int"
	toString BT_Char = "Char"
	toString BT_Real = "Real"
	toString BT_Bool = "Bool"
	toString (BT_String _) = "String"
	toString BT_Dynamic = "Dynamic"
	toString BT_File = "File"
	toString BT_World = "World"

instance <<< BasicType
where
	(<<<) file bt			= file <<< toString bt

instance <<< TypeVar
where
	(<<<) file varid = file <<< varid.tv_ident 
//	(<<<) file varid = file <<< varid.tv_ident <<< "<" <<< varid.tv_info_ptr <<< ">"

instance <<< AttributeVar
where
	(<<<) file {av_ident,av_info_ptr} = file <<< av_ident <<< "[" <<< av_info_ptr <<< "]"
//	(<<<) file {av_ident,av_info_ptr} = file <<< av_ident

instance toString AttributeVar
where
//	toString {av_ident,av_info_ptr} = toString av_ident + "[" + toString (ptrToInt av_info_ptr) + "]"
	toString {av_ident} = toString av_ident

instance <<< AType
where
	(<<<) file {at_attribute,at_type}
		= file <<< at_attribute <<< at_type

instance <<< TypeCons 
where
	(<<<) file (TypeConsSymb name) = file <<< name
	(<<<) file (TypeConsBasic basic_type) = file <<< basic_type
	(<<<) file TypeConsArrow = file <<< "(->)"
	(<<<) file (TypeConsVar tv) = file <<< tv

instance <<< TypeAttribute
where
	(<<<) file ta
		= file <<< toString ta

instance toString TypeAttribute
where
	toString (TA_Unique)
		= "*"
	toString (TA_TempVar tav_number)
		= "u" + toString tav_number + ":"
	toString (TA_Var avar)
		= toString avar + ":"
	toString (TA_RootVar avar)
		= toString avar + ":)"
	toString (TA_Anonymous)
		= "."
	toString TA_None
		= ""
	toString TA_Multi
		= "o "
	toString TA_MultiOfPropagatingConsVar
		= "@@ "
	toString (TA_List _ _)
		= "??? "

instance <<< Annotation
where
	(<<<) file an = file <<< toString an

instance toString Annotation
where
	toString AN_Strict	= "!" 
	toString _			= "" 

instance <<< ATypeVar
where
	(<<<) file {atv_attribute,atv_variable}
		= file <<< atv_attribute <<< atv_variable

instance <<< ConsVariable
where
	(<<<) file (CV tv)
		= file <<< tv
	(<<<) file (TempCV tv)
		= file <<<  "v" <<< tv <<< ' ' 
	(<<<) file (TempQCV tv)
		= file <<<  "E." <<< tv <<< ' ' 
	(<<<) file (TempQCDV tv)
		= file <<<  "E." <<< tv <<< ' ' 

instance <<< StrictnessList
where
	(<<<) file (NotStrict)
		= file <<< 0
	(<<<) file (Strict s)
		= file <<< s
	(<<<) file (StrictList s l)
		= file <<< s <<< ' ' <<< l

instance <<< Type
where
	(<<<) file (TV varid)
		= file <<< varid
	(<<<) file (TempV tv_number)
		= file  <<< 'v' <<< tv_number <<< ' ' 
	(<<<) file (TA consid types)
		= file  <<< consid <<< " " <<< types
	(<<<) file (TAS consid types strictness)
		= file  <<< consid <<< ' ' <<< strictness <<< ' ' <<< types
	(<<<) file (arg_type --> res_type)
		= file <<< arg_type <<< " -> " <<< res_type
	(<<<) file (TB tb)
		= file <<< tb
	(<<<) file (type :@: types)
		= file <<< type <<< " @" <<< types
	(<<<) file TArrow
		= file <<< "(->)"	
	(<<<) file (TArrow1 t)
		= file <<< "(->) " <<< t	
	(<<<) file (TFA vars types)
		= file <<< "A." <<< vars <<< ':' <<< types
	(<<<) file (TFAC vars types contexts)
		= file <<< "A." <<< vars <<< ':' <<< types <<< " | " <<< contexts
	(<<<) file (TempQV tv_number)
		= file  <<< "E.#" <<< tv_number <<< ' ' 
	(<<<) file (TempQDV tv_number)
		= file  <<< "E.#" <<< tv_number <<< ' ' 
	(<<<) file (TGenericFunctionInDictionary _ _ {gi_module,gi_index})
		= file <<< "TGenericFunctionInDictionary " <<< gi_module <<< ' ' <<< gi_index
	(<<<) file TE
		= file <<< "### EMPTY ###"
/*
instance <<< [a] | <<< , needs_brackets a
where
	(<<<) file [] 		= file
	(<<<) file [x:xs]
		| needs_brackets x
			= file <<< " (" <<< x <<< ')' <<< xs
			= file <<< ' ' <<< x <<< xs
*/

instance <<< SymbolType
where
	(<<<) file st=:{st_vars,st_attr_vars}
		| st.st_arity == 0
			= write_inequalities st.st_attr_env (write_contexts st.st_context (file <<< '[' <<< st_vars <<< ',' <<< st_attr_vars <<< ']' <<< st.st_args_strictness <<< ' ' <<< st.st_result))
			= write_inequalities st.st_attr_env (write_contexts st.st_context (file <<< '[' <<< st_vars <<< ',' <<< st_attr_vars <<< ']' <<< st.st_args_strictness <<< ' ' <<< st.st_args <<< " -> " <<< st.st_result))

write_contexts [] file
	= file
write_contexts [tc : tcs] file
	= write_contexts2 tcs (file <<< " | " <<< tc) 
where
	write_contexts2 [] file
		= file
	write_contexts2 [tc : tcs] file
		= write_contexts2 tcs (file <<< " & " <<< tc)

instance <<< AttrInequality
where
	(<<<) file {ai_demanded,ai_offered}
		= file <<< ai_offered <<< " <= " <<< ai_demanded
	
write_inequalities [] file
	= file
write_inequalities [ineq:ineqs] file
	= write_remaining_inequalities ineqs (file <<< ',' <<< ineq)
where
	write_remaining_inequalities [] file
		= file
	write_remaining_inequalities [ineq:ineqs] file
		= write_remaining_inequalities ineqs (file <<< ' ' <<< ineq)

instance <<< TypeContext
where
	(<<<) file co = file <<< co.tc_class <<< " " <<< co.tc_types <<< " <" <<< co.tc_var <<< '>'

instance <<< TCClass 
where
	(<<<) file (TCClass glob) = file <<< glob
	(<<<) file (TCGeneric {gtc_generic,gtc_kind})
		= file <<< gtc_generic <<< gtc_kind

instance toString TCClass
where
	toString (TCClass clazz) = clazz.glob_object.ds_ident.id_name
	toString (TCGeneric {gtc_generic,gtc_kind})
		= gtc_generic.glob_object.ds_ident.id_name +++ toString gtc_kind
	 
instance <<< SymbIdent
where
	(<<<) file symb=:{symb_kind = SK_Function symb_index }
		= file <<< symb.symb_ident <<<  '@' <<< symb_index
	(<<<) file symb=:{symb_kind = SK_LocalMacroFunction symb_index }
		= file <<< symb.symb_ident <<<  "[lm]@" <<< symb_index
	(<<<) file symb=:{symb_kind = SK_GeneratedFunction _ symb_index }
		= file <<< symb.symb_ident <<<  "[g]@" <<< symb_index
	(<<<) file symb=:{symb_kind = SK_LocalDclMacroFunction symb_index }
		= file <<< symb.symb_ident <<<  "[ldm]@" <<< symb_index
	(<<<) file symb=:{symb_kind = SK_OverloadedFunction symb_index }
		= file <<< symb.symb_ident <<<  "[o]@" <<< symb_index
	(<<<) file symb
		= file <<< symb.symb_ident 

instance <<< TypeSymbIdent
where
	(<<<) file symb	= file <<< symb.type_ident <<< '.' <<< symb.type_index
/*
instance <<< ClassSymbIdent
where
	(<<<) file symb	= file <<< symb.cs_name 
*/

instance <<< BoundVar
where
	(<<<) file {var_ident,var_info_ptr,var_expr_ptr}
		= file <<< var_ident <<< "<I" <<< var_info_ptr <<< ", E" <<< var_expr_ptr <<< '>'

instance <<< (Bind a b) | <<< a & <<< b 
where
	(<<<) file {bind_src,bind_dst} = file <<< bind_dst <<<  " = " <<< bind_src 


instance <<< AlgebraicPattern
where
	//(<<<) file g = file <<< g.ap_symbol <<< g.ap_vars <<< " -> " <<< g.ap_expr
	(<<<) file g = file <<< '\n' <<< g.ap_symbol <<< g.ap_vars <<< "\n\t-> " <<< g.ap_expr

instance <<< BasicPattern
where
	//(<<<) file g = file <<< g.bp_value <<< " -> " <<< g.bp_expr
	(<<<) file g = file <<< '\n' <<< g.bp_value <<< "\n\t-> " <<< g.bp_expr

instance <<< CasePatterns
where
	(<<<) file (BasicPatterns type patterns) = file <<< " " <<<patterns
	(<<<) file (AlgebraicPatterns type patterns) = file <<< patterns
	(<<<) file (DynamicPatterns patterns) = file <<< patterns
	(<<<) file (OverloadedListPatterns type decons_expr patterns) = file <<< ' ' <<< decons_expr <<< " " <<< patterns
	(<<<) file (NewTypePatterns type patterns) = file <<< patterns
	(<<<) file NoPattern = file 

instance <<< CheckedAlternative
where
	(<<<) file {ca_rhs} = file <<< ca_rhs

instance <<< Qualifier
where
	(<<<) file {qual_generators,qual_filter = Yes qual_filter} = file <<< qual_generators <<< "| " <<< qual_filter
	(<<<) file {qual_generators,qual_filter = No} = file <<< qual_generators

instance <<< Generator
where
	(<<<) file {gen_kind,gen_pattern,gen_expr}
		= file <<< gen_pattern <<< (gen_kind_to_string gen_kind) <<< gen_expr
	where
		gen_kind_to_string IsListGenerator = "<-"
		gen_kind_to_string IsOverloadedListGenerator = "<|-"
		gen_kind_to_string IsArrayGenerator = "<-:"

instance <<< BasicValue
where
	(<<<) file (BVI int)	= file <<< int
	(<<<) file (BVInt int)	= file <<< int
	(<<<) file (BVC char)	= file <<< char
	(<<<) file (BVB bool)	= file <<< bool
	(<<<) file (BVR real)	= file <<< real
	(<<<) file (BVS string)	= file <<< string
	
instance <<< Sequence
where
	(<<<) file (SQ_From _ expr) = file <<< expr
	(<<<) file (SQ_FromTo _ from_expr to_expr) = file <<< from_expr <<< ".."  <<< to_expr
	(<<<) file (SQ_FromThen _ from_expr then_expr) = file <<< from_expr  <<< ',' <<< then_expr <<< ".."
	(<<<) file (SQ_FromThenTo _ from_expr then_expr to_expr) = file <<< from_expr  <<< ',' <<< then_expr <<< ".." <<< to_expr

instance <<< Expression
where
	(<<<) file (Var ident) = file <<< ident
	(<<<) file (App {app_symb, app_args, app_info_ptr})
		= case app_symb.symb_kind of
			SK_Generic _ kind 
				->  file <<< app_symb <<< kind <<< ' ' <<< app_args
			_ 	-> file <<< app_symb <<< ' ' <<< app_args
	(<<<) file (f_exp @ a_exp) = file <<< '(' <<< f_exp <<< ") @ (" <<< a_exp <<< ')'
	(<<<) file (Let {let_info_ptr, let_strict_binds, let_lazy_binds, let_expr}) 
			= write_binds "" (write_binds "!" (file <<< "let" <<< '\n') let_strict_binds) let_lazy_binds <<< "in\n" <<< let_expr
	where
		write_binds x file []
			= file
		write_binds x file [bind : binds]
			= write_binds x (file <<< x <<< " " <<< bind) binds
 	(<<<) file (Case {case_expr,case_guards,case_default=No,case_explicit})
		//= file <<< "case " <<< case_expr <<< " of\n" <<< case_guards
		| case_explicit
			= file <<< "case " <<< case_expr <<< " of" <<< case_guards
			= file <<< "match " <<< case_expr <<< " of" <<< case_guards
	(<<<) file (Case {case_expr,case_guards,case_default= Yes def_expr,case_explicit})
		//= file <<< "case " <<< case_expr <<< " of\n" <<< case_guards <<< "\n\t->" <<< def_expr
		| case_explicit
			= file <<< "case " <<< case_expr <<< " of" <<< case_guards <<< "\n\t->" <<< def_expr
			= file <<< "match " <<< case_expr <<< " of" <<< case_guards <<< "\n\t->" <<< def_expr
	(<<<) file (BasicExpr basic_value) = file <<< basic_value
	(<<<) file (Conditional {if_cond,if_then,if_else}) =
			else_part (file <<< "IF " <<< if_cond <<< "\nTHEN\n" <<< if_then) if_else
	where
		else_part file No = file <<< '\n'
		else_part file (Yes else) = file <<< "\nELSE\n" <<< else <<< '\n'

/*	(<<<) file (Conditional {if_cond = {con_positive,con_expression},if_then,if_else}) =
			else_part (file <<< (if con_positive "IF " "IFNOT ") <<< con_expression <<< "\nTHEN\n" <<< if_then) if_else
	where
		else_part file No = file <<< '\n'
		else_part file (Yes else) = file <<< "\nELSE\n" <<< else <<< '\n'
*/
 	(<<<) file (Selection selector_kind expr selectors) = file <<< expr <<< selector_kind <<< selectors
	(<<<) file (Update expr1 selections expr2) =  file <<< '{' <<< expr1  <<< " & " <<<  selections <<< " = " <<< expr2 <<< '}'
	(<<<) file (RecordUpdate cons_symbol expression expressions) = file <<< '{' <<< cons_symbol <<< ' ' <<< expression <<< " & " <<< expressions <<< '}'
	(<<<) file (TupleSelect field field_nr expr) = file <<< expr <<<'.' <<< field_nr
	(<<<) file (MatchExpr cons expr) = file <<< cons <<< " (M)=: " <<< expr
	(<<<) file (IsConstructor expr cons_symbol cons_arity global_type_index case_ident position)
		= file <<< expr <<< " (I)=: " <<< cons_symbol
	(<<<) file EE = file <<< "** E **"
	(<<<) file (NoBind _) = file <<< "** NB **"
	(<<<) file (DynamicExpr {dyn_expr,dyn_type_code})     = file <<< "dynamic " <<< dyn_expr <<< " :: " <<< dyn_type_code 
//	(<<<) file (DynamicExpr {dyn_expr,dyn_uni_vars,dyn_type_code})     = writeVarPtrs (file <<< "dynamic " <<< dyn_expr <<< " :: dyn_uni_vars") dyn_uni_vars <<< "dyn_type_code=" <<< dyn_type_code 
	(<<<) file (TypeCodeExpression type_code)      = file <<< type_code
	(<<<) file (Constant symb _ _)         = file <<<  "** Constant **" <<< symb

	(<<<) file (ABCCodeExpr code_sequence do_inline)      = file <<< (if do_inline "code inline\n" "code\n") <<< code_sequence
	(<<<) file (AnyCodeExpr input output code_sequence)   = file <<< "code\n" <<< input <<< "\n" <<< output <<< "\n" <<< code_sequence

	(<<<) file (FreeVar {fv_ident})         	= file <<< fv_ident
	(<<<) file (ClassVariable info_ptr)         	= file <<< "ClassVariable " <<< info_ptr

	(<<<) file (FailExpr _) = file <<< "** FAIL **"
	(<<<) file (TypeSignature array_kind expr) = file <<< "TypeSignature " <<< '(' <<< expr <<< ')'
	(<<<) file (DictionariesFunction dictionaries expr expr_type)
		= file <<< "DictionariesFunction " <<< dictionaries <<< expr <<< expr_type
	(<<<) file expr         				= abort ("<<< (Expression)" )
	
instance <<< LetBind
where
	(<<<) file {lb_dst, lb_src}
		= file <<< lb_dst <<< " = " <<< lb_src <<< "\n"

instance <<< DynamicPattern
where
	(<<<) file {dp_var,dp_rhs,dp_type_code}
			= file <<< dp_var <<< " :: " <<< dp_type_code <<< " = " <<< dp_rhs

writeVarPtrs file []
	= file
writeVarPtrs file vars
	= write_var_ptrs (file <<< '<') vars <<< '>'
	where
		write_var_ptrs file [var]
			= file <<< var
		write_var_ptrs file [var : vars]
			= write_var_ptrs (file <<< var <<< '.') vars	
		
instance <<< TypeCodeExpression
where
	(<<<) file TCE_Empty
		= file
	(<<<) file (TCE_Var info_ptr)
		= file <<< "TCE_Var " <<< info_ptr
	(<<<) file (TCE_TypeTerm info_ptr)
		= file <<< "TCE_TypeTerm " <<< info_ptr
	(<<<) file (TCE_Constructor cons exprs)
		= file <<< "TCE_Constructor " <<< ' ' <<< exprs
	(<<<) file (TCE_Selector selectors info_ptr)
		= file <<< "TCE_Selector " <<< selectors <<< "VAR " <<< info_ptr
	(<<<) file (TCE_UniType vars type_code)
		= file <<< "TCE_UniType " <<< vars <<< " " <<< type_code
	(<<<) file (TCE_UnqType type_code)
		= file <<< "TCE_UnqType " <<< type_code

instance <<< (Ptr a)
where
	(<<<) file ptr
		= file <<< ptrToInt ptr

instance <<< SelectorKind
where
 	(<<<) file NormalSelector = file <<< "."
 	(<<<) file UniqueSelector = file <<< "!"
 	(<<<) file UniqueSelectorUniqueElementResult = file <<< "!*"
 	(<<<) file UniqueSingleArraySelector = file <<< "!"
 	(<<<) file UniqueSingleArraySelectorUniqueElementResult = file <<< "!*"

instance <<< Selection
where
	(<<<) file (RecordSelection selector _) = file <<< selector
	(<<<) file (ArraySelection {glob_object={ds_index}} _ index_expr) = file <<< '<' <<< ds_index <<< '>' <<< '[' <<< index_expr <<< ']'
	(<<<) file (DictionarySelection var selections _ index_expr) = file <<< '(' <<< var <<< '.' <<< selections <<< ')' <<< '[' <<< index_expr <<< ']'

instance <<< LocalDefs
where
	(<<<) file (LocalParsedDefs defs) = file <<< defs
	(<<<) file (CollectedLocalDefs defs) = file <<< defs

instance <<< (NodeDef dst) | <<< dst 
where
	(<<<) file {nd_dst,nd_alts,nd_locals} = file <<< nd_dst <<< nd_alts <<< nd_locals


instance <<< CollectedLocalDefs
where
	(<<<) file {loc_functions,loc_nodes}
		= file <<< loc_functions <<< loc_nodes
/*
	(<<<) file {def_types,def_constructors,def_selectors,def_macros,def_classes,def_members,def_instances}
		= file <<< def_types <<< def_constructors <<< def_selectors <<< def_macros <<< def_classes <<< def_members <<< def_instances
*/

instance <<< ParsedExpr
where
	(<<<) file (PE_List exprs) = file <<< exprs
	(<<<) file (PE_Tuple args) = file <<< '(' <<< args <<< ')'
	(<<<) file (PE_Basic basic_value) = file <<< basic_value
	(<<<) file (PE_Selection selector_kind expr selectors) =  file <<< expr <<< selector_kind <<< selectors
	(<<<) file (PE_Update expr1 selections expr2) =  file <<< '{' <<< expr1  <<< " & " <<<  selections <<< " = " <<< expr2 <<< '}'
	(<<<) file (PE_Record PE_Empty _ fields) = file <<< '{' <<< fields <<< '}'
	(<<<) file (PE_Record rec _ fields) = file <<< '{' <<< rec <<< " & " <<< fields <<< '}'
	(<<<) file (PE_ListCompr _ _ expr quals) = file <<< '[' <<< expr <<< " \\ " <<< quals <<< ']'
	(<<<) file (PE_ArrayCompr _ expr quals) = file <<< '{' <<< expr <<< " \\ " <<< quals <<< '}'
	(<<<) file (PE_Sequ seq) = file <<< '[' <<< seq <<< ']'
	(<<<) file PE_Empty = file <<< "** E **"
	(<<<) file (PE_Ident symb) = file <<< symb
	(<<<) file PE_WildCard = file <<< '_'
	(<<<) file (PE_Lambda _ exprs rhs _) = file <<< '\\' <<< exprs <<< rhs
	(<<<) file (PE_Bound bind) = file <<< bind
	(<<<) file (PE_Case _ expr alts) = file <<< "case " <<< expr <<< " of\n" <<< alts
	(<<<) file (PE_Let defs expr) = file <<< "let " <<< defs <<< " in\n" <<< expr
	(<<<) file (PE_DynamicPattern expr type) = file <<< expr <<< "::" <<< type
	(<<<) file (PE_Dynamic expr maybetype)
		= case maybetype of
			Yes type
				-> file <<< "dynamic " <<< expr <<< "::" <<< type
			No
				-> file <<< "dynamic " <<< expr
	(<<<) file _ = file <<< "some expression"

instance <<< ParsedSelectorKind
where
	(<<<) file ParsedNormalSelector			= file <<< "."
	(<<<) file (ParsedUniqueSelector False)	= file <<< "!"
	(<<<) file (ParsedUniqueSelector True)	= file <<< "!*"
	
instance <<< ParsedSelection
where
	(<<<) file (PS_Record selector _)	= file <<< selector
	(<<<) file (PS_Array index_expr)	= file <<< '[' <<< index_expr <<< ']'
	(<<<) file PS_Erroneous				= file <<< "Erroneous selector" // PK

instance <<< FieldNameOrQualifiedFieldName
where
	(<<<) file (FieldName ident) = file <<< ident
	(<<<) file (QualifiedFieldName module_ident field_name) = file <<< '.' <<< module_ident <<< "'." <<< field_name

instance <<< CaseAlt
where
	(<<<) file {calt_pattern,calt_rhs} = file <<< calt_pattern <<< " -> " <<< calt_rhs

instance <<< ParsedBody
where
	(<<<) file {pb_args,pb_rhs} = file <<< pb_args <<< " = " <<< pb_rhs

instance <<< FunKind
where
	(<<<) file (FK_Function False) = file <<< "FK_Function"
	(<<<) file (FK_Function True) = file <<< "Lambda"
	(<<<) file FK_NodeDefOrFunction = file <<< "FK_NodeDefOrFunction"
	(<<<) file FK_Macro = file <<< "FK_Macro"
	(<<<) file FK_Caf = file <<< "FK_Caf"
	(<<<) file FK_Unknown = file <<< "FK_Unknown"

instance <<< FunType 
where
	(<<<) file {ft_ident,ft_type} = file <<< ft_ident <<< "::" <<< ft_type

instance <<< FunDef
where
	(<<<) file {fun_ident,fun_body=ParsedBody bodies} = file <<< fun_ident <<< '.' <<< ' ' <<< bodies 
	(<<<) file {fun_ident,fun_body=CheckedBody {cb_args,cb_rhs},fun_info={fi_free_vars,fi_def_level,fi_calls}}
		= file <<< fun_ident <<< '.'
			<<< "C " <<< cb_args <<< " = " <<< cb_rhs <<< '\n'
//			<<< '.' <<< fi_def_level <<< ' ' <<< '[' <<< fi_free_vars <<< ']' <<< cb_args <<< " = " <<< cb_rhs 
	(<<<) file {fun_ident,fun_body=TransformedBody {tb_args,tb_rhs},fun_info={fi_free_vars,fi_local_vars,fi_def_level,fi_calls}}
		= file <<< fun_ident <<< '.' <<< "T "
//			<<< '[' <<< fi_free_vars <<< "]  [" <<< fi_local_vars <<< ']'
			<<< tb_args <<< '[' <<< fi_calls <<< ']' <<< "\n\t= " <<< tb_rhs <<< '\n'
//			<<< '.' <<< fi_def_level <<< ' ' <<< '[' <<< fi_free_vars <<< ']' <<< tb_args <<< " = " <<< tb_rhs 
	(<<<) file {fun_ident,fun_body=NoBody,fun_type=Yes type} = file // <<< type <<< '\n'
			<<< fun_ident <<< '.' <<< "Array function\n"

	(<<<) file {fun_ident} = file <<< fun_ident <<< "???" <<< '\n'

instance <<< FunctionBody
where
	(<<<) file (ParsedBody bodies) = file <<< bodies 
	(<<<) file (CheckedBody {cb_args,cb_rhs}) = file <<< "C " <<< cb_args <<< " = " <<< cb_rhs <<< '\n'
	(<<<) file (TransformedBody {tb_args,tb_rhs}) = file <<< "T "  <<< tb_args <<< " = " <<< tb_rhs <<< '\n'
	(<<<) file (Expanding vars) = file <<< "E " <<< vars 
	(<<<) file GeneratedBody = file <<< "Generic function\n"
	(<<<) file NoBody = file <<< "Array function\n"
	

instance <<< FunCall
where
	(<<<) file (FunCall fc_index fc_level)
			= file <<< fc_index <<< '.' <<< fc_level
	(<<<) file (MacroCall module_index fc_index fc_level)
			= file <<< "MacroCall "<<< module_index <<<" "<<<fc_index <<< '.' <<< fc_level
	(<<<) file (DclFunCall module_index fc_index)
			= file <<< "DclFunCall "<<< module_index <<<" "<<<fc_index
	(<<<) file (GeneratedFunCall fc_index fun_ptr)
			= file <<< "GeneratedFunCall "<<< fc_index

instance <<< FreeVar
where
	(<<<) file {fv_ident,fv_info_ptr,fv_count} = file <<< fv_ident <<< '#' <<< fv_count <<< '<' <<< fv_info_ptr <<< '>'

instance <<< DynamicType
where
	(<<<) file {dt_uni_vars,dt_type}
		| isEmpty dt_uni_vars
			= file <<< "DynamicType" <<< dt_type
			= file <<< "DynamicType" <<< "A." <<< dt_uni_vars <<< ":" <<< dt_type
			

instance <<< SignClassification
where
	(<<<) file {sc_pos_vect,sc_neg_vect} = write_signs file sc_pos_vect sc_neg_vect 0
	where
		write_signs file sc_pos_vect sc_neg_vect index
			| sc_pos_vect == 0 && sc_neg_vect == 0
				= file
			#	index_bit = (1 << index)
			| sc_pos_vect bitand index_bit == 0
				| sc_neg_vect bitand index_bit == 0
					= write_signs (file <<< 'O') sc_pos_vect sc_neg_vect (inc index)
					= write_signs (file <<< '-') sc_pos_vect (sc_neg_vect bitand (bitnot index_bit)) (inc index)
				| sc_neg_vect bitand index_bit == 0
					= write_signs (file <<< '+') (sc_pos_vect bitand (bitnot index_bit)) sc_neg_vect (inc index)
					= write_signs (file <<< 'T') (sc_pos_vect bitand (bitnot index_bit)) (sc_neg_vect bitand (bitnot index_bit)) (inc index)

instance toString TypeKind
where
	toString (KindVar _) 		= "**"
	toString KindConst			= "*"
	toString (KindArrow args) 	= "{" +++ (to_string args) +++ "->*}" 
	where
		to_string []		= "??????" 
		to_string [k]		= toString k  
		to_string [k:ks]	= (toString k) +++ "->" +++ (to_string ks)

				
instance <<< TypeKind
where
	(<<<) file kind 		= file <<< (toString kind)

instance == TypeKind 
where
	(==) KindConst KindConst = True
	(==) (KindArrow xs) (KindArrow ys) = xs == ys
	(==) _ _ = False

instance toString KindInfo
where
	toString (KI_Var ptr) 				= "*" // +++ toString (ptrToInt ptr)
	toString (KI_Const) 				= "*"
	toString (KI_Arrow kind1 kind2)		= withBrackets kind1 (toString kind1) +++ " -> " +++ toString kind2
	where
		withBrackets (KI_Arrow _ _) kind_str	= "(" +++ kind_str +++ ")"
		withBrackets _				kind_str	= kind_str

instance <<< TypeDefInfo
where
	(<<<) file {tdi_group,tdi_group_vars,tdi_cons_vars}
//		= file <<< '[' <<< tdi_group <<< '=' <<< tdi_group_vars <<< '=' <<< tdi_cons_vars <<< ']'
		= file <<< '[' <<< tdi_group_vars <<< '=' <<< tdi_cons_vars <<< ']'

instance <<< DefinedSymbol
where
	(<<<) file {ds_ident}
		= file <<< ds_ident 

instance <<< (TypeDef a) | <<< a
where
	(<<<) file {td_ident, td_args, td_rhs}
		= file <<< ":: " <<< td_ident <<< ' ' <<< td_args <<< td_rhs

instance <<< TypeRhs
where
	(<<<) file (SynType type)
		= file <<< " :== " <<< type 
	(<<<) file (AlgType data)
		= file <<< " = " <<< data 
	(<<<) file (RecordType record)
		= file <<< " = " <<< '{' <<< record <<< '}'
	(<<<) file _
		= file 


instance <<< RecordType
where
	(<<<) file {rt_fields} = iFoldSt (\index file -> file <<< rt_fields.[index]) 0 (size rt_fields) file

instance <<< FieldSymbol
where
	(<<<) file {fs_ident} = file <<< fs_ident

/*
	where
		write_data_defs file []
			= file
		write_data_defs file [d:ds]
			= write_data_defs (file <<< d <<< '\n') ds
*/

instance <<< GenericClassInfo 
where
	(<<<) file {gci_kind, gci_class} = file <<< gci_kind <<< ":" <<< gci_class 

instance <<< InstanceType
where
	(<<<) file it = write_contexts it.it_context (file <<< it.it_types) 

instance <<< RhsDefsOfType
where
	(<<<) file (TypeSpec type) = file <<< type
	(<<<) file (ConsList cons_defs) = file <<< cons_defs
	(<<<) file (SelectorList _ _ _ sel_defs) = file <<< sel_defs
	(<<<) file (NewTypeCons cons_def) = file <<< cons_def
	(<<<) file _ = file

instance <<< ParsedConstructor
where
	(<<<) file {pc_cons_ident,pc_arg_types} = file <<< pc_cons_ident <<< pc_arg_types

instance <<< ParsedSelector
where
	(<<<) file {ps_field_ident,ps_field_type} = file <<< ps_field_ident <<< ps_field_type

instance <<< ModuleKind
where
	(<<<) file kind 		= file

instance <<< ConsDef
where
	(<<<) file {cons_ident,cons_type} = file <<< cons_ident <<< " :: " <<< cons_type

instance <<< SelectorDef
where
	(<<<) file {sd_ident} = file <<< sd_ident

instance <<< ClassDef
where
	(<<<) file {class_ident} = file <<< class_ident

instance <<< ClassInstance
where
	(<<<) file {ins_class_ident,ins_type} = file <<< ins_class_ident.ci_ident <<< " :: " <<< ins_type

instance <<< (Optional a) | <<< a
where
	(<<<) file (Yes x) = file <<< x
	(<<<) file No = file
	
instance <<< (Module a) | <<< a
where
	(<<<) file {mod_ident,mod_type,mod_defs} = file <<< mod_type <<< mod_ident <<< mod_defs

instance <<< (CollectedDefinitions a) | <<< a
where
	(<<<) file {def_types,def_constructors,def_selectors,def_macros,def_classes,def_members,def_instances}
		= file

instance <<< ParsedDefinition
where
	(<<<) file (PD_Function _ name _ exprs rhs _ ) = file <<< name <<< exprs <<< " = " <<< rhs
	(<<<) file (PD_NodeDef  _ pattern rhs) = file <<< pattern <<< " =: " <<< rhs
	(<<<) file (PD_TypeSpec _ name prio st sp) = file <<< name <<< st
	(<<<) file (PD_Type td) = file <<< td
	(<<<) file (PD_Generic {gen_ident}) = file <<< "generic " <<< gen_ident
	(<<<) file (PD_GenericCase {gc_gcf=GCF gc_ident _,gc_type_cons} _) = file <<< gc_ident <<< "{|" <<< gc_type_cons <<< "|}"
	(<<<) file _ = file

instance <<< Rhs
where
	(<<<) file {rhs_alts,rhs_locals} = file <<< rhs_alts <<< rhs_locals <<< '\n'

instance <<< OptGuardedAlts
where
	(<<<) file (GuardedAlts guarded_exprs def_expr) = file <<< guarded_exprs <<< def_expr
	(<<<) file (UnGuardedExpr unguarded_expr) = file <<< unguarded_expr

instance <<< ExprWithLocalDefs
where
	(<<<) file {ewl_expr,ewl_locals,ewl_nodes=[]} = file <<< ewl_expr <<< ewl_locals
	(<<<) file {ewl_expr,ewl_locals,ewl_nodes} = file <<< ewl_nodes <<< '\n' <<< ewl_expr <<< ewl_locals

instance <<< NodeDefWithLocals
where
	(<<<) file {ndwl_strict,ndwl_def,ndwl_locals}
		| ndwl_strict
			= file <<< "\n#! " <<< ndwl_def <<< ndwl_locals;
			= file <<< "\n# " <<< ndwl_def <<< ndwl_locals;

instance <<< GuardedExpr
where
	(<<<) file {alt_nodes,alt_guard,alt_expr} = file <<< '|' <<< alt_guard <<< alt_expr


instance <<< IndexRange
where
	(<<<) file {ir_from,ir_to}
		| ir_from == ir_to
			= file
			= file <<< ir_from <<< "---" <<< ir_to

instance <<< Ident
where
//	(<<<) file {id_name,id_index} = file <<< id_name <<< '.' <<< id_index
	(<<<) file {id_name} = file <<< id_name

instance <<< (Global a) | <<< a
where
	(<<<) file {glob_object,glob_module} = file <<< glob_object <<< "M:" <<< glob_module

instance <<< Position
where
	(<<<) file (FunPos file_name line func) = file <<< '[' <<< file_name <<< ',' <<< line <<< ',' <<< func <<< ']'
	(<<<) file (LinePos file_name line) = file <<< '[' <<< file_name <<< ',' <<< line <<< ']'
	(<<<) file _ = file


instance <<< TypeVarInfo
where
	(<<<) file TVI_Empty				= file <<< "TVI_Empty"
	(<<<) file (TVI_Type _)				= file <<< "TVI_Type"
	(<<<) file (TVI_TypeVar ptr)		= file <<< (ptrToInt ptr) 
	(<<<) file (TVI_Forward	_) 			= file <<< "TVI_Forward"
	(<<<) file (TVI_SignClass _ _ _) 	= file <<< "TVI_SignClass"
	(<<<) file (TVI_AttrAndRefCount ta rc) 	= file <<< "TVI_AttrAndRefCount " <<< ta <<< " " <<< rc
	(<<<) file (TVI_CorrespondenceNumber n) = file <<< "TVI_CorrespondenceNumber " <<< n
	(<<<) file (TVI_AType at) 			= file <<< "TVI_AType " <<< at
	(<<<) file TVI_Used					= file <<< "TVI_Used"
	(<<<) file (TVI_TypeCode	_)		= file <<< "TVI_TypeCode"
	(<<<) file (TVI_CPSLocalTypeVar	_)	= file <<< "TVI_CPSLocalTypeVar"		
	(<<<) file (TVI_Kinds _)		= file <<< "TVI_Kinds"
	(<<<) file (TVI_PropClass _ _ _) 	= file <<< "TVI_PropClass"
	(<<<) file (TVI_TypeKind kind_info_ptr) = file <<< "TVI_TypeKind " <<< (ptrToInt kind_info_ptr)
	(<<<) file (TVI_Kind kind) 			= file <<< "TVI_Kind" <<< kind
	(<<<) file (TVI_Expr is_bimap_id expr) = file <<< "TVI_Expr " <<< is_bimap_id <<< ' ' <<< expr

instance <<< AttrVarInfo
where
	(<<<) file AVI_Empty				= file <<< "AVI_Empty"
	(<<<) file (AVI_Attr attr)			= file <<< "AVI_Attr " <<< attr
	(<<<) file (AVI_AttrVar av_info_ptr) = file <<< "AVI_AttrVar " <<< ptrToInt av_info_ptr
	(<<<) file (AVI_Forward temp_attr_id) = file <<< "AVI_Forward " <<< temp_attr_id 
	(<<<) file (AVI_CorrespondenceNumber n) = file <<< "AVI_CorrespondenceNumber " <<< n
	(<<<) file AVI_Used					= file <<< "AVI_Used"
	(<<<) file AVI_CountZero 			= file <<< "AVI_CountZero"
	(<<<) file AVI_CountOne 			= file <<< "AVI_CountOne"
	(<<<) file AVI_CountMany 			= file <<< "AVI_CountMany"
	(<<<) file (AVI_CountVar _) 		= file <<< "AVI_CountVar"
	(<<<) file (AVI_SequenceNumber n) 	= file <<< "AVI_SequenceNumber " <<< n
	(<<<) file AVI_Collected 			= file <<< "AVI_Collected"
	
instance <<< Import
where
	(<<<) file {import_module, import_symbols=ImportSymbolsAll}
		= file <<< "import " <<< import_module
	(<<<) file {import_module, import_symbols=ImportSymbolsOnly import_symbols}
		= file <<< "import " <<< import_module <<< import_symbols

instance <<< ImportDeclaration
where
	(<<<) file (ID_Function ident)			= file <<< ident
	(<<<) file (ID_Class ident optIdents)	= file <<< "class " <<< ident <<< optIdents
	(<<<) file (ID_Type ident optIdents)	= file <<< ":: " <<< ident <<< optIdents
	(<<<) file (ID_Record ident optIdents)	= file <<< ident <<< " { " <<< optIdents <<< " } "
	(<<<) file (ID_Instance i1 i2 tup)		= file <<< "instance " <<< i1 <<< i2 <<< tup // !ImportedIdent !Ident !(![Type],![TypeContext])

instance <<< CoercionPosition
where
	(<<<) file (CP_FunArg fun_name arg_nr)
		= file <<< "argument " <<< arg_nr <<< " of " <<< readable fun_name
	(<<<) file (CP_SymbArgAndExpression fun_name arg_nr expression)
		= show_expression (file <<< "argument " <<< arg_nr <<< " of " <<< readable fun_name.symb_ident <<< " : ") expression 
	(<<<) file (CP_LiftedFunArg fun_name arg_name)
		= file <<< "lifted argument " <<< arg_name <<< " of " <<< readable fun_name
	(<<<) file (CP_Expression expression) = show_expression file expression

show_expression file (Var {var_ident})
	= file <<< var_ident
show_expression file (FreeVar {fv_ident})
	= file <<< fv_ident
show_expression file (App {app_symb={symb_ident}, app_args})
	| symb_ident.id_name=="_dummyForStrictAlias"
		= show_expression file (hd app_args)
	= file <<< readable symb_ident
show_expression file (fun @ fun_args)
	= show_expression file fun
show_expression file (Case {case_ident=No})
	= file <<< "(case ... )"
show_expression file (Selection _ expr selectors)
	= file <<< "selection"
show_expression file (Update expr1 selectors expr2)
	= file <<< "update"
show_expression file (TupleSelect {ds_arity} elem_nr expr)
	= file <<< "argument " <<< (elem_nr + 1) <<< " of " <<< ds_arity <<< "-tuple"
show_expression file (BasicExpr bv)
	= file <<< bv
show_expression file (RecordUpdate _ _ _)
	= file <<< "update of record"
show_expression file (MatchExpr _ expr)
	= file <<< "match expression"
show_expression file (IsConstructor _ _ _ _ _ _)
	= file <<< "is constructor expression"
show_expression file (Let _)
	= file <<< "(let ... ) or #"
show_expression file _
	= file

instance <<< Declaration
  where
	(<<<) file (Declaration { decl_ident, decl_kind })
		= file <<< decl_ident <<< '<' <<< decl_ident.id_info <<< '>' <<< '(' <<< decl_kind <<< ')'

instance <<< STE_Kind
where
	(<<<) file
		(STE_FunctionOrMacro _)
			= file <<< "STE_FunctionOrMacro"
	(<<<) file
		STE_Type
			= file <<< "STE_Type"
	(<<<) file
		STE_Constructor
			= file <<< "STE_Constructor"
	(<<<) file
		(STE_Selector _)
			= file <<< "STE_Selector"
	(<<<) file
		STE_Class
			= file <<< "STE_Class"
	(<<<) file
		(STE_Field _)
			= file <<< "STE_Field"
	(<<<) file
		STE_Member
			= file <<< "STE_Member"
	(<<<) file
		STE_Instance
			= file <<< "STE_Instance"
	(<<<) file
		(STE_Variable _) 
			= file <<< "STE_Variable"
	(<<<) file
		(STE_TypeVariable _) 
			= file <<< "STE_TypeVariable"
	(<<<) file
		(STE_TypeAttribute _)
			= file <<< "STE_TypeAttribute"
	(<<<) file
		(STE_BoundTypeVariable _)
			= file <<< "STE_BoundTypeVariable"
	(<<<) file
		(STE_Imported a b)
			= file <<< "STE_Imported (" <<< a <<< ")" <<< b 
	(<<<) file
		STE_DclFunction
			= file <<< "STE_DclFunction"
	(<<<) file (STE_Generic _) = file <<< "STE_Generic"
	(<<<) file STE_GenericCase = file <<< "STE_GenericCase"
	(<<<) file STE_GenericDeriveClass = file <<< "STE_GenericDeriveClass"
	(<<<) file
		(STE_Module _)
			= file <<< "STE_Module"
	(<<<) file
		STE_ClosedModule
			= file <<< "STE_ClosedModule"
	(<<<) file
		STE_Empty 
			= file <<< "STE_Empty"
	(<<<) file
		_ 
			= file <<< "STE_???"

instance <<< IdentOrQualifiedIdent
where
	(<<<) file (Ident ident)
		= file <<< ident
	(<<<) file (QualifiedIdent module_ident name)
		= file<<<'\''<<<module_ident<<<"'."<<<name

readable :: !Ident -> String // somewhat hacky
readable {id_name}
	| size id_name>0 && id_name.[0]=='_'
		| id_name=="_Cons" || id_name=="_Nil"
			= "list constructor"
		| id_name=="_!Cons" || id_name=="_!Nil"
			= "! list constructor"
		| id_name=="_#Cons" || id_name=="_#Nil"
			= "# list constructor"
		| id_name=="_Cons!" || id_name=="_Nil!"
			= "list constructor !"
		| id_name=="_!Cons!" || id_name=="_!Nil!"
			= "! list constructor !"
		| id_name=="_#Cons!" || id_name=="_#Nil!"
			= "# list constructor !"
			= id_name%(1, size id_name-1)
	= id_name

instance == ModuleKind
where
	(==) mk1 mk2 = equal_constructor mk1 mk2

instance == TypeAttribute
where
	(==) attr1 attr2 = equal_constructor attr1 attr2

instance == GlobalIndex
where
	(==) gi1 gi2 = gi1.gi_module == gi2.gi_module && gi1.gi_index == gi2.gi_index

instance == Annotation
where
	(==) a1 a2 = equal_constructor a1 a2

instance == OverloadedListType
where
	(==) a1 a2 = equal_constructor a1 a2
	
EmptySymbolTableEntry :== EmptySymbolTableEntryCAF.boxed_symbol_table_entry

EmptySymbolTableEntryCAF :: BoxedSymbolTableEntry
EmptySymbolTableEntryCAF =: {boxed_symbol_table_entry = { ste_kind = STE_Empty, ste_index = NoIndex, ste_def_level = NotALevel, ste_previous = abort_empty_SymbolTableEntry, ste_doc = No } }

abort_empty_SymbolTableEntry :: a
abort_empty_SymbolTableEntry = abort "empty SymbolTableEntry"


newTypeSymbIdentCAF :: TypeSymbIdent;
newTypeSymbIdentCAF =: MakeTypeSymbIdentMacro { glob_object = NoIndex, glob_module = NoIndex } {id_name="",id_info=nilPtr} 0

MakeTypeSymbIdentMacro type_index name arity
	:== {	type_ident = name, type_arity = arity, type_index = type_index,
			type_prop = { tsp_sign = BottomSignClass, tsp_propagation = NoPropClass, tsp_coercible = True }}
