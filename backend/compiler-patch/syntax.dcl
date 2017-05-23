definition module syntax

import StdEnv

import scanner, general, typeproperties, Heap
import IndexType
from containers import ::NumberSet
from convertcases import :: LetVarInfo, :: LetExpressionInfo, :: RefCountsInCase, :: SplitsInCase

::	Ident =
	{ 	id_name		:: !String
	,	id_info 	:: !SymbolPtr
	}

instance toString Ident

/*	Each Identifier is equipped with a pointer to a SymbolTableEntry that is
	used for binding the identifier with its definition.
*/

::	SymbolTable			:== Heap SymbolTableEntry
::	SymbolPtr 			:== Ptr SymbolTableEntry

::	SymbolTableEntry = 
	{	ste_kind		:: !STE_Kind
	,	ste_index		:: !Index
	,	ste_def_level	:: !Level
	,	ste_previous	:: SymbolTableEntry
	}

:: FunctionOrMacroIndex = FunctionOrIclMacroIndex !Int | DclMacroIndex /*module_n*/ !Int /*macro_n_in_module*/ !Int;

instance == FunctionOrMacroIndex

::	STE_BoundTypeVariable	= { stv_attribute :: !TypeAttribute, stv_info_ptr :: !TypeVarInfoPtr }

::	STE_Kind	= STE_FunctionOrMacro ![FunctionOrMacroIndex]
				| STE_DclMacroOrLocalMacroFunction ![FunctionOrMacroIndex]
				| STE_Type
				| STE_Constructor
				| STE_Selector ![Global Index]
				| STE_Field !Ident
				| STE_Class
				| STE_Member
				| STE_Generic !Int /*arity*/
				| STE_GenericCase
				| STE_GenericDeriveClass
				| STE_Instance
				| STE_Variable !VarInfoPtr
				| STE_TypeVariable !TypeVarInfoPtr
				| STE_FunDepTypeVariable !TypeVarInfoPtr
				| STE_TypeAttribute !AttrVarInfoPtr
				| STE_BoundTypeVariable !STE_BoundTypeVariable
				| STE_Imported !STE_Kind !ModuleN
				| STE_DclFunction
				| STE_Module !(Module (CollectedDefinitions ClassInstance))
				| STE_ClosedModule
				| STE_ModuleQualifiedImports !SortedQualifiedImports
				| STE_Empty
					/* for creating class dictionaries */
				| STE_DictType !CheckedTypeDef
				| STE_DictCons !ConsDef
				| STE_DictField !SelectorDef
				| STE_Called ![FunctionOrMacroIndex] /* used during macro expansion to indicate that this function is called */
				| STE_ExplImpSymbol !Int
				| STE_ExplImpComponentNrs ![ComponentNrAndIndex]
					/*	stores the numbers of all module components that import the symbol from
						the "actual" dcl module.
					*/
				| STE_BelongingSymbol !Int
				| STE_ExplImpSymbolNotImported !ModuleN !STE_Kind
				| STE_ImportedQualified !Declaration !STE_Kind

				| STE_UsedType !ModuleN !STE_Kind
					/* used during binding of types to mark types that have been applied. */
				| STE_UsedQualifiedType !ModuleN !Index !STE_Kind
				| STE_BelongingSymbolExported
				| STE_BelongingSymbolForExportedSymbol
				| STE_TypeExtension

::	ModuleN:==Int;

::	SortedQualifiedImports	= SortedQualifiedImports !Declaration !SortedQualifiedImports !SortedQualifiedImports
							| EmptySortedQualifiedImports

::	Declaration = Declaration !DeclarationRecord

::	DeclarationRecord =
	{	decl_ident	:: !Ident
	,	decl_pos	:: !Position
	,	decl_kind	:: !STE_Kind
	,	decl_index	:: !Index
	}

::	ComponentNrAndIndex =
	{	cai_component_nr	:: !Int
	,	cai_index			:: !Int // points into ExplImpInfos
	}

::	Global object =
	{	glob_object	:: !object
	,	glob_module	:: !Index
	}

::	Module defs = 
	{	mod_ident		:: !Ident
	,	mod_modification_time		:: {#Char}
	,	mod_type		:: !ModuleKind
	, 	mod_imports		:: ![ParsedImport]
	,	mod_imported_objects :: ![ImportedObject]
	,	mod_foreign_exports :: ![ParsedForeignExport]
	,	mod_defs		:: !defs
	}

::	ParsedForeignExport =
	{	pfe_ident	:: !Ident
	,	pfe_line	:: !Int
	,	pfe_file	:: !FileName
	,	pfe_stdcall	:: !Bool
	}

::	ParsedModule	:== Module  [ParsedDefinition]
::	ScannedModule 	:== Module  (CollectedDefinitions (ScannedInstanceAndMembersR FunDef))
	
::	ModuleKind		= MK_Main | MK_Module | MK_System | MK_None | MK_NoMainDcl

::	FunDefIndex:==Int

::	IclFunctionIndices =
	{	ifi_global_function_indices	:: ![IndexRange]
	,	ifi_local_function_indices	:: !IndexRange
	,	ifi_instance_indices		:: ![IndexRange]
	,	ifi_specials_indices		:: !IndexRange
	,	ifi_gencase_indices			:: ![IndexRange]
	,	ifi_type_function_indices	:: ![IndexRange]
	}

::	IclModule  =
	{	icl_name				:: !Ident
	,	icl_functions			:: !.{# FunDef }
	,	icl_function_indices	:: !IclFunctionIndices
	,	icl_common				:: !.CommonDefs
	,	icl_import				:: !{!Declaration}
	,	icl_qualified_imports	:: ![QualifiedDeclaration]
	,	icl_imported_objects	:: ![ImportedObject]
	,	icl_foreign_exports		:: ![ForeignExport]
	,	icl_used_module_numbers :: !NumberSet
	,	icl_modification_time	:: !{#Char}
	}

::	DclModule =
	{	dcl_name			:: !Ident
	,	dcl_functions		:: !{# FunType }
	,	dcl_instances		:: !IndexRange
	,	dcl_macros			:: !IndexRange
	,	dcl_specials		:: !IndexRange
	,	dcl_gencases		:: !IndexRange
	,	dcl_type_funs		:: !IndexRange
	,	dcl_common			:: !CommonDefs
	,	dcl_sizes			:: !{# Int}
	,	dcl_dictionary_info	:: !DictionaryInfo
	,	dcl_declared		:: !Declarations
	,	dcl_has_macro_conversions :: !Bool
	,	dcl_module_kind		:: !ModuleKind
	,	dcl_modification_time:: !{#Char}
	,	dcl_imported_module_numbers :: !NumberSet
	}

:: ForeignExport = {fe_fd_index :: !FunDefIndex, fe_stdcall :: !Bool}

::	CopiedDefinitions =
	{	copied_type_defs	:: {#Bool}
	,	copied_class_defs	:: {#Bool}
	,	copied_generic_defs :: {#Bool}
	}

::	Declarations = {
		dcls_import	::!{!Declaration}
	,	dcls_local		::![Declaration]
	,	dcls_local_for_import ::!{!Declaration}
	}

::	QualifiedDeclaration :== ([Declaration], ModuleIdent, Position)

::	DictionaryInfo = { n_dictionary_types :: !Int, n_dictionary_constructors :: !Int, n_dictionary_selectors :: !Int }

::	RhsDefsOfType	= ConsList ![ParsedConstructor]
					| SelectorList !Ident ![ATypeVar] !Bool /*is_boxed_record*/ ![ParsedSelector]
					| TypeSpec !AType
					| NewTypeCons !ParsedConstructor
					| EmptyRhs !BITVECT
					| AbstractTypeSpec !BITVECT !AType
					| ExtensibleConses ![ParsedConstructor]
					| MoreConses !Ident ![ParsedConstructor]

::	CollectedDefinitions instance_kind =
	{	def_types 			:: ![TypeDef TypeRhs]
	,	def_constructors	:: ![ConsDef]
	,	def_selectors		:: ![SelectorDef]
	,	def_macros			:: ![FunDef]
	,	def_macro_indices	:: !IndexRange
	,	def_classes			:: ![ClassDef]
	,	def_members			:: ![MemberDef]
	,	def_funtypes		:: ![FunType]
	,	def_instances		:: ![instance_kind]
	,	def_generics		:: ![GenericDef]
	, 	def_generic_cases 	:: ![GenericCaseDef]
	}

::	CommonDefs =
	{	com_type_defs 		:: !.{# CheckedTypeDef}
	,	com_cons_defs		:: !.{# ConsDef}
	,	com_selector_defs	:: !.{# SelectorDef}
	,	com_class_defs		:: !.{# ClassDef}
	,	com_member_defs		:: !.{# MemberDef}
	,	com_instance_defs	:: !.{# ClassInstance}
	,	com_generic_defs	:: !.{# GenericDef}
	,	com_gencase_defs 	:: !.{# GenericCaseDef}
	}

::	LocalDefs	= LocalParsedDefs [ParsedDefinition]
				| CollectedLocalDefs CollectedLocalDefs

::	IndexRange	= { ir_from :: !Index, ir_to :: !Index }

::	ArrayAndListInstances = {
		ali_array_first_instance_indices :: ![Int],
		ali_list_first_instance_indices :: ![Int],
		ali_tail_strict_list_first_instance_indices :: ![Int],
		ali_instances_range :: !IndexRange
	}

NoIndex		:== -1


::  Level	:== Int
NotALevel 	:==  -1

::	CollectedLocalDefs =
	{	loc_functions	:: !IndexRange
	,	loc_nodes		:: ![NodeDef ParsedExpr]
	,	loc_in_icl_module :: !Bool // False for local functions in macros in dcl modules, otherwise True
	}

::	NodeDef dst =
	{	nd_dst		::!dst,
		nd_alts		::!OptGuardedAlts,
		nd_locals	::!LocalDefs,
		nd_position	::!Position		
	}

::	Rhs =
	{	rhs_alts	:: !OptGuardedAlts
	,	rhs_locals	:: !LocalDefs
	}


cIsAFunction	:== True
cIsNotAFunction :== False

::	ParsedDefinition 
	=	PD_Function Position Ident Bool [ParsedExpr] Rhs FunKind
	|	PD_NodeDef Position ParsedExpr Rhs
	|	PD_Type ParsedTypeDef
	|	PD_TypeSpec Position Ident Priority (Optional SymbolType) FunSpecials
	|	PD_Class ClassDef [ParsedDefinition]
	|	PD_Instance ParsedInstanceAndMembers
	|	PD_Instances [ParsedInstanceAndMembers]
	|	PD_Import [ParsedImport]
	|	PD_ImportedObjects [ImportedObject]
	|	PD_ForeignExport !Ident !{#Char} !Int !Bool /* if stdcall */
	|	PD_Generic GenericDef
	| 	PD_GenericCase GenericCaseDef Ident
	|	PD_Derive [GenericCaseDef]
	|	PD_Erroneous

::	FunKind = FK_Function !Bool | FK_Macro | FK_Caf | FK_NodeDefOrFunction | FK_Unknown

::	StrictnessList = NotStrict | Strict !Int | StrictList !Int StrictnessList

cNameNotLocationDependent :== False
cNameLocationDependent :== True

::	ParsedSelector =
	{	ps_field_ident		:: !Ident
	,	ps_selector_ident	:: !Ident
	,	ps_field_annotation	:: !Annotation
	,	ps_field_type		:: !AType
	,	ps_field_var		:: !Ident
	,	ps_field_pos		:: !Position
	}

::	ParsedConstructor =
	{	pc_cons_ident 	:: !Ident
	,	pc_cons_arity	:: !Int
	,	pc_exi_vars		:: ![ATypeVar]
	,	pc_arg_types	:: ![AType]
	,	pc_args_strictness	:: !StrictnessList
	,	pc_context		:: ![TypeContext]
	,	pc_cons_prio	:: !Priority
	,	pc_cons_pos		:: !Position
	}

::	ParsedInstance =
	{	pi_class 	:: !IdentOrQualifiedIdent
	,	pi_ident	:: !Ident
	,	pi_types	:: ![Type]
	,	pi_context	:: ![TypeContext]
	,	pi_pos		:: !Position
	,	pi_specials	:: !Specials
	}

::	ParsedInstanceAndMembers =
	{	pim_pi 		:: !ParsedInstance
	,	pim_members	:: ![ParsedDefinition]
	}

::	ScannedInstanceAndMembersR icl_member =
	{	sim_pi 				:: !ParsedInstance
	,	sim_members			:: ![icl_member]	// for .icl
	,	sim_member_types	:: ![FunType]	// for .dcl
	}

::	IdentOrQualifiedIdent
	= Ident !Ident
	| QualifiedIdent /*module*/!Ident !String

/*
	Objects of type Specials are used to specify specialized instances of overloaded functions.
	These can only occur in definition modules. After parsing the SP_ParsedSubstitutions alternative
	is used to indicate the specific instantiation. The SP_Substitutions alternative is used to deduce
	the type of the specialized version. Finally the SP_ContextTypes alternative is set and used during 
	the typing to check whether this instance has been used. The auxiliary FSP_FunIndex alternative is used
	to store the index of the function that has been specialized.
*/

::	Specials
	= SP_ParsedSubstitutions 	![Env Type TypeVar]
	| SP_Substitutions 		 	![SpecialSubstitution]
	| SP_ContextTypes			![Special]
	| SP_TypeOffset				!Int					// index in SP_Substitutions for specialized instance
	| SP_None

::	FunSpecials
	= FSP_ParsedSubstitutions 	![Env Type TypeVar]
	| FSP_Substitutions			![SpecialSubstitution]
	| FSP_ContextTypes			![Special]
	| FSP_FunIndex				!Index
	| FSP_None

::	SpecialSubstitution =
	{	ss_environ	:: !Env Type TypeVar
	,	ss_context	:: ![TypeContext]
	,	ss_vars		:: ![TypeVar]
	,	ss_attrs	:: ![AttributeVar]
	}

::	Special =
	{	spec_index	:: !Global Index
	,	spec_types	:: ![[Type]]
	,	spec_vars	:: ![TypeVar]
	, 	spec_attrs	:: ![AttributeVar]
	}

::	AttrInequality =
	{	ai_demanded :: !AttributeVar
	,	ai_offered	:: !AttributeVar
	}

::	DefinedSymbol = 
	{	ds_ident		:: !Ident
	,	ds_arity		:: !Int
	,	ds_index		:: !Index
	}

::	ClassDef =
 	{	class_ident			:: !Ident
	,	class_arity			:: !Int
	,	class_args			:: ![TypeVar]
	,	class_context		:: ![TypeContext]
	,	class_members		:: !{# DefinedSymbol}
	,	class_dictionary	:: !DefinedSymbol
	,	class_pos			:: !Position
	,	class_cons_vars		:: !BITVECT
	,	class_fun_dep_vars	:: !BITVECT
	,	class_lazy_members	:: !BITVECT
	}

::	ClassDefInfos :== {# .{! [TypeKind]}}

::	MemberDef =
	{	me_ident		:: !Ident
	,	me_class		:: !Global Index
	,	me_offset		:: !Index
	,	me_type			:: !SymbolType
	,	me_type_ptr		:: !VarInfoPtr
	,	me_class_vars	:: ![ATypeVar]
	,	me_pos			:: !Position
	,	me_priority 	:: !Priority
	}

:: GenericDef = 
	{	gen_ident		:: !Ident		// the generics name in IC_Generic
	,	gen_member_ident	:: !Ident	// the generics name in IC_Expression
	, 	gen_pos			:: !Position
	,	gen_type		:: !SymbolType	// Generic type (st_vars include generic type vars)
	,	gen_vars		:: ![TypeVar]	// Generic type variables
	,	gen_deps		:: ![GenericDependency]	   // Generic function dependencies
	,	gen_info_ptr	:: !GenericInfoPtr
	}

:: GenericDependency =
	{	gd_ident		:: !IdentOrQualifiedIdent
	, 	gd_index		:: !GlobalIndex
	, 	gd_vars			:: ![TypeVar]
	,	gd_nums			:: ![Int] // Mapping from dependency variable to generic type variable
	}

instance == GenericDependency

:: GenericClassInfo = 
	{	gci_kind	:: !TypeKind	// the kind
	,	gci_module 	:: !Index		// filled with main_module_index
	,	gci_class	:: !Index		// class_index in the main module
	,	gci_member	:: !Index 		// the class member index 
	}
:: GenericClassInfos :== {[GenericClassInfo]}

:: GenericInfo = 
	{	gen_classes		:: !GenericClassInfos
	,	gen_var_kinds	:: ![TypeKind]  	// kinds of all st_vars of the gen_type
	,	gen_rep_conses	:: !{!GenericRepresentationConstructor}
	//	OBJECT, CONS, RECORD, FIELD, PAIR, EITHER, UNIT
	}

::	GenericRepresentationConstructor =
	{	grc_module	:: !Int
	,	grc_index	:: !GenericCaseBody // GCB_FunIndex, GCB_FunAndMacroIndex or GCB_None
	,	grc_local_fun_index :: !Int
	,	grc_ident	:: !Ident
	,	grc_generic_info :: !Int
	,	grc_generic_instance_deps :: !GenericInstanceDependencies
	,	grc_optional_fun_type :: !Optional SymbolType
	}

:: GenericInfoPtr :== Ptr GenericInfo	
:: GenericHeap :== Heap GenericInfo

:: TypeCons 
	= TypeConsSymb TypeSymbIdent 
	| TypeConsBasic BasicType 
	| TypeConsArrow
	| TypeConsVar TypeVar 

:: GenericCaseDef =
	{	gc_pos			:: !Position			// position in the source file
	,	gc_type			:: !Type				// the instance type
	,   gc_type_cons	:: !TypeCons			// type constructor of the type argument
	,	gc_gcf			:: !GenericCaseFunctions
	}

::	GenericCaseFunctions
	= GCF !Ident !GCF
	| GCFS ![!GCF!]
	| GCFC !Ident !Ident	// IC_GenericDeriveClass IC_Class

::	GCF = {
		gcf_gident	:: !Ident,	  			// name in IC_Generic namespace
		gcf_generic	:: !GlobalIndex,		// index of the generic
		gcf_arity	:: !Int,				// arity of the function
		gcf_generic_info :: !Int,			// 0 = no, -1 = all, generic info for CONS, OBJECT, RECORD or FIELD
		gcf_body	:: !GenericCaseBody,	// the body function or NoIndex
		gcf_kind	:: !TypeKind,			// kind of the instance type
		gcf_generic_instance_deps :: !GenericInstanceDependencies
	}

:: GenericCaseBody 
	= GCB_None 									// to be generated
	| GCB_FunIndex !Index
	| GCB_FunAndMacroIndex !Index !Index
	| GCB_MacroIndex !Index
	| GCB_FunDef !FunDef
	| GCB_ParsedBody ![ParsedExpr] !Rhs

::	GenericInstanceDependencies
	= AllGenericInstanceDependencies
	| GenericInstanceDependencies !Int /*n_deps*/ !Int /*deps*/
	| GenericInstanceUsedArgs !Int /*n_deps*/ !Int /*deps*/

::	InstanceType =
	{	it_vars			:: [TypeVar]
	,	it_types		:: ![Type]
	,	it_attr_vars	:: [AttributeVar]
	,	it_context		:: ![TypeContext]
	}

::	ClassInstance =
 	{	ins_class_index	:: !GlobalIndex
	,	ins_class_ident	:: !ClassIdent
	,	ins_ident		:: !Ident
	,	ins_type		:: !InstanceType
	,	ins_member_types :: ![FunType]	// for .dcl
	,	ins_members		:: !{#ClassInstanceMember}
	,	ins_specials	:: !Specials
	,	ins_pos			:: !Position
	}

::	ClassIdent =
	{	ci_ident		:: !IdentOrQualifiedIdent
	,	ci_arity		:: !Int
	}

::	ClassInstanceMember = 
	{	cim_ident		:: !Ident
	,	cim_arity		:: !Int		// module number if cim_index<0
	,	cim_index		:: !Index	// or -1-index
	}

::	Import =
	{	import_module		:: !Ident
	,	import_symbols		:: !ImportSymbols [ImportDeclaration]
	,	import_file_position:: !Position	// for error messages
	,	import_qualified	:: !ImportQualified
	}

::	ImportSymbols import_declarations
	= ImportSymbolsAll
	| ImportSymbolsOnly !import_declarations

::	ImportQualified = NotQualified | Qualified | QualifiedAs !Ident

instance toString Import, AttributeVar, TypeAttribute, Annotation

::	ParsedImport		:== Import

::	ImportDeclaration	= ID_Function !Ident
						| ID_Class !Ident !(Optional [Ident])
						| ID_Type !Ident !(Optional [Ident])
						| ID_Record !Ident !(Optional [Ident])
						| ID_Instance !Ident !Ident !(![Type],![TypeContext])
						| ID_Generic !Ident !Ident

cIsImportedLibrary :== True
cIsImportedObject :== False

:: ImportedObject =
	{	io_is_library :: !Bool
	,	io_name    :: !{#Char}
	}

::	RecordType =
	{	rt_constructor	:: !DefinedSymbol
	,	rt_fields		:: !{# FieldSymbol}
	,	rt_is_boxed_record :: !Bool
	}

::	FieldSymbol =
	{	fs_ident		:: !Ident
	,	fs_var			:: !Ident
	,	fs_index		:: !Index
	}

::	TypeRhs	= AlgType ![DefinedSymbol]
			| SynType !AType
			| RecordType !RecordType
			| NewType !DefinedSymbol
			| AbstractType !BITVECT
			| AbstractSynType !BITVECT !AType
			| ExtensibleAlgType ![DefinedSymbol]
			| AlgConses ![DefinedSymbol] !GlobalIndex
			| UncheckedAlgConses !Ident ![DefinedSymbol]
			| UnknownType

::	ParsedTypeDef	:== TypeDef RhsDefsOfType
::	CheckedTypeDef	:== TypeDef TypeRhs

cAllBitsClear			:== 0
cIsHyperStrict			:== 1
cIsNonCoercible			:== 2
cIsAnalysed				:== 4

::	GlobalIndex =
	{	gi_module	::!Int
	,	gi_index	::!Int
	}
NoGlobalIndex :== {gi_module=NoIndex,gi_index=NoIndex}	

::	TypeDef type_rhs =
	{	td_ident		:: !Ident
	,	td_index		:: !Int
	,	td_arity		:: !Int
	,	td_args			:: ![ATypeVar]
	,	td_attrs		:: ![AttributeVar]
	,	td_rhs			:: !type_rhs
	,	td_attribute	:: !TypeAttribute
	,	td_pos			:: !Position
	,	td_used_types	:: ![GlobalIndex]
	,	td_fun_index	:: !Index
	}

::	TypeDefInfo =
	{	tdi_kinds			:: ![TypeKind]
	,	tdi_properties		:: !BITVECT
	,	tdi_group			:: ![GlobalIndex]
	,	tdi_group_nr		:: !Int
	,	tdi_group_vars		:: ![Int]
	,	tdi_cons_vars		:: ![Int]
	,	tdi_index_in_group	:: !Index
	,	tdi_classification	:: !TypeClassification
	,	tdi_gen_rep 		:: !Optional GenericTypeRep
	}

// type structure is used to specialize a generic to a type
:: GenTypeStruct 
	= GTSAppCons TypeKind [GenTypeStruct]
	| GTSAppVar TypeVar [GenTypeStruct] 
	| GTSVar TypeVar
 	| GTSCons !DefinedSymbol !GlobalIndex !DefinedSymbol !DefinedSymbol !GenTypeStruct
 	| GTSRecord !DefinedSymbol !GlobalIndex !DefinedSymbol !DefinedSymbol !GenTypeStruct
 	| GTSField !DefinedSymbol !GlobalIndex !DefinedSymbol !GenTypeStruct
 	| GTSObject !DefinedSymbol !GlobalIndex !DefinedSymbol !GenTypeStruct
	| GTSE
	// the following constructors are used for optimizing bimaps
	| GTSPair !GenTypeStruct !GenTypeStruct
	| GTSEither !GenTypeStruct !GenTypeStruct
	| GTSUnit
	| GTSArrow GenTypeStruct GenTypeStruct
 	| GTSAppConsBimapKindConst
	| GTSAppBimap TypeKind [GenTypeStruct]
	| GTSAppConsSimpleType !GlobalIndex !TypeKind ![GenTypeStruct]

:: GenericTypeRep = 
	{ gtr_type :: GenTypeStruct		// generic structure type
	, gtr_iso  :: !DefinedSymbol	// the conversion isomorphism
	, gtr_to   :: !DefinedSymbol
	, gtr_from :: !DefinedSymbol
	}

::	TypeDefInfos :== {# .{# TypeDefInfo}}

::	FunType =
	{	ft_ident		:: !Ident
	,	ft_arity		:: !Int
	,	ft_priority		:: !Priority
	,	ft_type			:: !SymbolType
	,	ft_pos			:: !Position
	,	ft_specials		:: !FunSpecials
	,	ft_type_ptr		:: !VarInfoPtr
	}

::	FreeVar =
	{	fv_def_level	:: !Level
	,	fv_ident		:: !Ident
	,	fv_info_ptr		:: !VarInfoPtr
	,	fv_count		:: !Int
	}

::	ModuleIndex:==Index;
::	DclFunctionIndex:==Index;

::	FunCall	= FunCall !Index !Level
			| MacroCall !ModuleIndex !Index Level
			| DclFunCall !ModuleIndex !DclFunctionIndex
			| GeneratedFunCall !Index !FunctionInfoPtr;

FI_IsMacroFun	:== 1			// whether the function is a local function of a macro
FI_HasTypeSpec	:== 2			// whether the function has u user defined type
FI_IsNonRecursive :== 4			// used in trans.icl and partition.icl
FI_IsUnboxedListOfRecordsConsOrNil :== 8
FI_MemberInstanceRequiresTypeInDefMod :== 16
FI_GenericFun :== 32
FI_Unused :== 64				// used in module trans
FI_UnusedUsed :== 128			// used in module trans
FI_HasTypeCodes :== 256

::	FunInfo =
	{	fi_calls			:: ![FunCall]
	,	fi_group_index		:: !Index
	,	fi_def_level		:: !Level
	,	fi_free_vars		:: ![FreeVar]
	,	fi_local_vars		:: ![FreeVar]
	,	fi_dynamics			:: ![ExprInfoPtr]
	,	fi_properties		:: !BITVECT
	}

::	ParsedBody =
	{	pb_args		:: ![ParsedExpr]
	,	pb_rhs		:: !Rhs
	,	pb_position	:: !Position
	}

::	CheckedBody =
	{	cb_args		:: ![FreeVar]
	,	cb_rhs		:: ![CheckedAlternative]
	}

::	CheckedAlternative =
	{	ca_rhs		:: !Expression
	,	ca_position	:: !Position	// the position is NoPos iff the position information for this
	}								// alternative is already stored in a case alternative
									// (in ap_position, bp_position or dp_position)

::	TransformedBody =
	{	tb_args			:: ![FreeVar]
	,	tb_rhs			:: !Expression
	}

::	FunctionBody	= ParsedBody ![ParsedBody]
					| CheckedBody !CheckedBody
	/* The next three constructors are used during macro expansion (module transform) */
					| PartitioningMacro
					| PartitioningFunction !CheckedBody !Int
					| RhsMacroBody !CheckedBody
	/* macro expansion transforms a CheckedBody into a TransformedBody */
					| TransformedBody !TransformedBody
					| Expanding ![FreeVar] // the parameters of the newly generated function
					| GeneratedBody // the body will be generated automatically - for generics
					| NoBody

::	FunDef =
	{	fun_ident		:: !Ident
	,	fun_arity		:: !Int
	,	fun_priority	:: !Priority
	,	fun_body		:: !FunctionBody
	,	fun_type		:: !Optional SymbolType
	,	fun_pos			:: !Position
	,	fun_kind		:: !FunKind
	,	fun_lifted		:: !Int
	,	fun_info		:: !FunInfo
	}

cIsAGlobalVar	:== True
cIsALocalVar	:== False

::	ConsClasses =
	{	cc_size			::!Int
	,	cc_args			::![ConsClass]
	,	cc_linear_bits	::![#Bool!]
	,	cc_producer		::!ProdClass
	}

::	ConsClass	:== Int

::	ProdClass	:== Bool

pIsSafe			:== True

::	ImportedConstructors	:== [Global Index]
::	ImportedFunctions		:== [Global Index]
::	ImportedTypes			:== {#{# CheckedTypeDef}}

::	OptionalVariable :== Optional (Bind Ident VarInfoPtr)

:: 	AuxiliaryPattern
		= AP_Algebraic !(Global DefinedSymbol) !GlobalIndex ![AuxiliaryPattern] !OptionalVariable
		| AP_Variable !Ident !VarInfoPtr OptionalVariable
		| AP_Basic !BasicValue OptionalVariable
		| AP_NewType !(Global DefinedSymbol) !Index AuxiliaryPattern OptionalVariable
		| AP_Dynamic !AuxiliaryPattern !DynamicType !OptionalVariable
		| AP_Constant !AP_Kind !(Global DefinedSymbol) !Priority
		| AP_WildCard !OptionalVariable
		| AP_Empty

:: AP_Kind = APK_Constructor !GlobalIndex | APK_NewTypeConstructor !Index | APK_Macro !Bool // is_dcl_macro

::	VI_TypeInfo	= VITI_Empty
				| VITI_Coercion		CoercionPosition
				| VITI_PatternType	[AType] /*module*/!Index /*constructor*/!Index VI_TypeInfo

//::	VarInfo  =	VI_Empty | VI_Type !AType !(Optional CoercionPosition) | VI_FAType ![ATypeVar] !AType !(Optional CoercionPosition) |
::	VarInfo  =	VI_Empty | VI_Type !AType !VI_TypeInfo |
				VI_FAType ![ATypeVar] !AType !VI_TypeInfo |
				VI_FATypeC ![ATypeVar] !AType ![TypeContext] !VI_TypeInfo | VI_FPC |
				VI_Occurrence !Occurrence | VI_UsedVar !Ident |
				VI_Expression !Expression | VI_Variable !Ident !VarInfoPtr | VI_LiftedVariable !VarInfoPtr |
				VI_Count !Int /* the reference count of a variable */ !Bool /* true if the variable is global, false otherwise */ |
				VI_AccVar !ConsClass !ArgumentPosition /* used during fusion to determine accumulating parameters of functions */ |
				VI_Alias !BoundVar /* used for resolving aliases just before type checking (in transform) */ |
				 /* used during elimination and lifting of cases */
				VI_RefFromTupleSel0 !Int |
				VI_RefFromArrayUpdate !Int ![Selection] |
				VI_RefFromArrayUpdateToTupleSelector2 !Int ![Selection] !VarInfoPtr |
				VI_RefFromArrayUpdateOfTupleElem2 !Int ![Selection] |
				VI_FreeVar !Ident !VarInfoPtr !Int !AType | VI_BoundVar !AType | VI_LocalVar |
				VI_ClassVar !Ident !VarInfoPtr !Int | /* to hold dictionary variables during overloading */
				VI_EmptyConstructorClassVar |
				VI_ForwardClassVar !VarInfoPtr | /* to hold the dictionary variable generated during overloading */
				VI_Forward !BoundVar | VI_LetVar !LetVarInfo | VI_LetExpression !LetExpressionInfo |
				VI_CaseOrStrictLetVar !VarInfoPtr | VI_StrictLetVar |
				VI_CorrespondenceNumber !Int | /* it is assumed that this alternative is _only_ used in module comparedefimp */
				VI_SequenceNumber !Int | VI_AliasSequenceNumber !BoundVar |
				VI_Used | /* for indicating that an imported function has been used */
				VI_PropagationType !SymbolType | /* for storing the type with propagation environment of an imported function */
				VI_ExpandedType !SymbolType | /* for storing the (expanded) type of an imported function */
				VI_Record ![AuxiliaryPattern] |
				VI_Pattern !AuxiliaryPattern |
				VI_TypeCodeVariable !TypeCodeVariableInfo |
				VI_DynamicValueAlias !BoundVar |
				VI_Body !SymbIdent !TransformedBody ![FreeVar] ![TypeVar] ![TypeVar] | /* used during fusion */
				VI_ExpressionOrBody !Expression !SymbIdent !TransformedBody ![FreeVar] ![TypeVar] ![TypeVar] | /* used during fusion */
				VI_Dictionary !SymbIdent ![Expression] !Type | /* used during fusion */
				VI_Extended !ExtendedVarInfo !VarInfo |
				VI_NotUsed |
// MdM
				VI_CPSExprVar !CheatCompiler /* a pointer to a variable in CleanProverSystem is stored here, using a cast */
// ... MdM
				| VI_Labelled_Empty !{#Char} // RWS debugging
				| VI_LocalLetVar // RWS, mark Let vars during case transformation

::	TypeCodeVariableInfo
	= TCI_TypeVar !Expression
	| TCI_TypePatternVar !Expression
	| TCI_SelectionsTypePatternVar ![(Expression,[Selection])]

::	ExtendedVarInfo = EVI_VarType !AType

::	ArgumentPosition :== Int

::	VarHeap :== Heap VarInfo
::	VarInfoPtr	:== Ptr VarInfo

cNotVarNumber :== -1

::	BoundVar = 
	{	var_ident		:: !Ident
	,	var_info_ptr	:: !VarInfoPtr
	,	var_expr_ptr	:: !ExprInfoPtr
	}

::	TypeSymbIdent =
	{	type_ident		:: !Ident
	,	type_arity		:: !Int
	,	type_index		:: !Global Index
	,	type_prop		:: !TypeSymbProperties
	}

::	TypeSymbProperties =
	{	tsp_sign		:: !SignClassification
	,	tsp_propagation	:: !PropClassification
	,	tsp_coercible	:: !Bool
	}

::	SymbKind	= SK_Unknown
				| SK_Function !(Global Index)
				| SK_IclMacro !Index
				| SK_LocalMacroFunction !Index
				| SK_DclMacro !(Global Index)
				| SK_LocalDclMacroFunction !(Global Index)
				| SK_OverloadedFunction !(Global Index)
				| SK_GeneratedFunction !FunctionInfoPtr !Index
				| SK_Constructor !(Global Index)
				| SK_NewTypeConstructor !GlobalIndex
				| SK_Generic !(Global Index) !TypeKind
				| SK_TypeCode
				| SK_OverloadedConstructor !(Global Index)
				| SK_TFACVar !ExprInfoPtr
				| SK_VarContexts !(VarContexts TypeContext)
				| SK_TypeCodeAndContexts !(VarContexts TypeContext)

/*	Some auxiliary type definitions used during fusion. Actually, these definitions
	should have been given in seperate module. Unfortunately, Clean's module system
	forbids cyclic dependencies between def modules.
*/

::	FunctionHeap 	:== Heap FunctionInfo

::	FunctionInfoPtr	:== Ptr FunctionInfo

::	FunctionInfo	= FI_Empty | FI_Function !GeneratedFunction

::	Producer	= PR_Empty
				| PR_Function !SymbIdent !Int !Index
				| PR_Class !App ![(BoundVar, Type)] !Type
				| PR_Constructor !SymbIdent !Int ![Expression]
				| PR_GeneratedFunction !SymbIdent !Int !Index
				| PR_Curried !SymbIdent !Int
				| PR_Unused
				| PR_CurriedFunction !SymbIdent !Int !Index
				| PR_String !{#Char}
				| PR_Int !Int
				| PR_Equal !Int
				| PR_EqualRemove !Int

::	InstanceInfo = II_Empty | II_Node !{! Producer} !FunctionInfoPtr !InstanceInfo !InstanceInfo

::	GeneratedFunction = 
	{	gf_fun_def			:: !FunDef
	,	gf_instance_info	:: !InstanceInfo
	,	gf_cons_args		:: !ConsClasses
	,	gf_fun_index		:: !Index
	}
	
/*	... main type definitions continued .... */

::	ExpressionHeap 	:== Heap ExprInfo

::	ExprInfoPtr		:== Ptr ExprInfo

::	TempLocalVar	:== Int

::	DynamicPtr		:== ExprInfoPtr

::	ExprInfo		= EI_Empty

		/* For handling overloading */

					| EI_Overloaded !OverloadedCall 						/* initial, set by the type checker */
					| EI_OverloadedWithVarContexts !OverloadedCallWithVarContexts /* initial, set by the type checker */
					| EI_Instance 	!(Global DefinedSymbol) ![Expression]	/* intermedediate, used during resolving of overloading */ 
					| EI_Selection 	![Selection] !VarInfoPtr ![Expression]	/* intermedediate, used during resolving of overloading */
					| EI_Context 	![Expression]							/* intermedediate, used during resolving of overloading */
					| EI_ContextWithVarContexts ![Expression] !(VarContexts DictionaryAndClassType) /* intermedediate, used during resolving of overloading */
					| EI_FPContext	![Expression] !ExprInfoPtr				/* intermedediate, used during resolving of overloading */

		/* For handling dynamics */

					| EI_UnmarkedDynamic 		!(Optional DynamicType) ![DynamicPtr]	// in expression
					| EI_Dynamic 				!(Optional DynamicType) ![DynamicPtr]	// in expression
					| EI_DynamicType			!DynamicType ![DynamicPtr]				// in pattern

		/* Auxiliary, was EI_DynamicType before checking */

					| EI_DynamicTypeWithVars	![TypeVar] !DynamicType ![DynamicPtr]				

		/* Auxiliary, used during type checking */

					| EI_TempDynamicType 		!(Optional DynamicType) ![DynamicPtr] !AType ![TypeContext] ![TypeContext] !ExprInfoPtr !SymbIdent
					| EI_TempDynamicPattern 	![TypeVar] !DynamicType ![DynamicPtr] ![TempLocalVar] !AType ![TypeContext] !ExprInfoPtr !SymbIdent

					| EI_TypeOfDynamic 			!TypeCodeExpression						/* Final */
					| EI_TypeOfDynamicPattern 	![VarInfoPtr] !TypeCodeExpression !Bool	/* Final */
					| EI_TypeOfDynamicWithContexts !TypeCodeExpression !(VarContexts DictionaryAndClassType)

					| EI_TypeCode 		!TypeCodeExpression
					| EI_TypeCodes 		![TypeCodeExpression]
					| EI_TypeCodesWithContexts ![TypeCodeExpression] !(VarContexts DictionaryAndClassType)

					| EI_Attribute !Int

		/* EI_DictionaryType is used to store the instance type of a class. This type is used during fusion to generate proper types for 
		   the fusion result (i.e. the resulting function after elimination of dictionaries) */

					| EI_DictionaryType !Type
					| EI_CaseType !CaseType
					| EI_LetType ![AType]
					| EI_CaseTypeWithContexts !CaseType ![(DefinedSymbol,[TypeContext])]
					| EI_CaseTypeAndRefCounts !CaseType !RefCountsInCase
					| EI_CaseTypeAndSplits !CaseType !SplitsInCase
					| EI_LetTypeAndRefCounts ![AType] ![Int]

		/* for converting case into function patterns the following auxiliary constuctors are used */


					| EI_Default !Expression !AType !ExprInfoPtr
					| EI_DefaultFunction !SymbIdent ![Expression]
					| EI_Extended !ExtendedExprInfo !ExprInfo

::	ExtendedExprInfo
					= EEI_ActiveCase !ActiveCaseInfo

::	ActiveCaseInfo =
	{	aci_params					:: ![FreeVar]
	,	aci_opt_unfolder			:: !(Optional SymbIdent)
	,	aci_free_vars				:: !Optional [BoundVar]
	,	aci_linearity_of_patterns	:: ![![#Bool!]!]
	,	aci_safe					:: !Bool
	}

/*
	OverloadedCall contains (type) information about functions that are overloaded. This structure is built during type checking
	and used after (standard) unification to insert the proper instances of the corresponding functions.
*/

::	OverloadedCall = 
	{	oc_symbol	:: !SymbIdent
	,	oc_context	:: ![TypeContext]
	,	oc_specials	:: ![Special]
	}

::	OverloadedCallWithVarContexts = 
	{	ocvc_symbol       :: !SymbIdent
	,	ocvc_context      :: ![TypeContext]
	,	ocvc_var_contexts :: !VarContexts TypeContext
	}

::	DictionaryAndClassType =
	{	dc_var			:: !VarInfoPtr
	,	dc_class_type	:: !AType
	}

::	VarContexts type_contexts
	= VarContext !Int /*arg_n*/ ![type_contexts] !AType !(VarContexts type_contexts)
	| NoVarContexts

/*
	CaseType contains the type information needed to type the corresponding case construct:
		ct_pattern_type : the type of the pattern
		ct_result_type  : the type of the result (of each pattern)
		ct_cons_types   : the types of the arguments of each pattern constructor
*/

::	CaseType =
	{	ct_pattern_type	:: !AType
	,	ct_result_type	:: !AType
	,	ct_cons_types 	:: ![[AType]]
	}
		
::	SymbIdent =
	{	symb_ident		:: !Ident
	,	symb_kind		:: !SymbKind
	}

::	ConsDef =
	{	cons_ident			:: !Ident
	,	cons_type			:: !SymbolType
	,	cons_priority		:: !Priority
	,	cons_number			:: !Index // -2 for newtype constructor, -3 for added constructor
	,	cons_type_index		:: !Index
	,	cons_exi_vars		:: ![ATypeVar]
	,	cons_type_ptr		:: !VarInfoPtr
	,	cons_pos			:: !Position
	}

::	SelectorDef =
	{	sd_ident		:: !Ident
	,	sd_field		:: !Ident
	,	sd_type			:: !SymbolType
	,	sd_exi_vars		:: ![ATypeVar]
	,	sd_field_nr		:: !Int
	,	sd_type_index	:: !Int
	,	sd_type_ptr		:: !VarInfoPtr
	,	sd_pos			:: !Position
	}

::	SymbolType =
	{	st_vars			:: ![TypeVar]
	,	st_args			:: ![AType]
	,	st_args_strictness :: !StrictnessList
	,	st_arity		:: !Int
	,	st_result		:: !AType
	,	st_context		:: ![TypeContext]
	,	st_attr_vars	:: ![AttributeVar]
	,	st_attr_env		:: ![AttrInequality]
	}

::	TypeContext =
	{	tc_class	:: !TCClass
	,	tc_types	:: ![Type]
	,	tc_var		:: !VarInfoPtr
	}

:: TCClass 	= TCClass 		!(Global DefinedSymbol) // Normal class
			| TCGeneric 	!GenericTypeContext		// Generic class
			| TCQualifiedIdent !Ident !String

:: GenericTypeContext =
	{ gtc_generic 	:: !Global DefinedSymbol
	, gtc_kind		:: !TypeKind 
	, gtc_class		:: !Global DefinedSymbol	// generated class
	, gtc_generic_dict :: !GlobalIndex			// HACK: dictionary different from the one contained in the class
	}

::	AType =
	{	at_attribute	:: !TypeAttribute
	,	at_type			:: !Type
	}
	
::	TempAttrId		:== Int
::	TempVarId		:== Int

::	Type	=	TA !TypeSymbIdent ![AType]
			|	TAS !TypeSymbIdent ![AType] !StrictnessList
			|	(-->) infixr 9 !AType !AType
			| 	TArrow							/* (->) */
			| 	TArrow1	!AType					/* ((->) a) */
			|	(:@:) infixl 9 !ConsVariable ![AType]
			|	TB !BasicType

			|	TFA ![ATypeVar] !Type			/* Universally quantified types */

			| 	GTV !TypeVar
			| 	TV !TypeVar

			|	TFAC ![ATypeVar] !Type ![TypeContext]	// Universally quantified function argument type with contexts

			|	TempV !TempVarId				/* Auxiliary, used during type checking */
			|	TempQV !TempVarId				/* Auxiliary, used during type checking */
			|	TempQDV !TempVarId				// Auxiliary, used during type checking, existential type variable in dynamic pattern

			|	TLifted !TypeVar				/* Auxiliary, used during type checking of lifted arguments */
			|	TQualifiedIdent !Ident !String ![AType]

			|	TGenericFunctionInDictionary !(Global DefinedSymbol) !TypeKind !GlobalIndex /*GenericDict*/

			|	TLiftedSubst !Type				// Auxiliary, used during fusion when generating a new function type

			|	TE

::	ConsVariable = CV 		!TypeVar
				 | TempCV 	!TempVarId
				 | TempQCV 	!TempVarId
				 | TempQCDV !TempVarId	// existential type variable in dynamic pattern

::	DynamicType =
	{	dt_uni_vars 	:: ![ATypeVar]
	,	dt_global_vars	:: ![TypeVar]
	,	dt_type			:: !AType
	,	dt_contexts		:: ![TypeContext]
	}

::	KindHeap	:== Heap KindInfo
::	KindInfoPtr	:== Ptr KindInfo

::	KindInfo	= KI_Var !KindInfoPtr
				| KI_Arrow !KindInfo !KindInfo
				| KI_Const
				
				| KI_ConsVar
				
				| KI_VarBind !KindInfoPtr
				| KI_NormVar !Int

::	TypeVarInfo  	= TVI_Empty
					| TVI_Type !Type
					| TVI_TypeVar !TypeVarInfoPtr // Sjaak: to collect and check universally quantified type variables
					| TVI_Forward !TempVarId | TVI_TypeKind !KindInfoPtr
					| TVI_SignClass !Index !SignClassification !TypeVarInfo | TVI_PropClass !Index !PropClassification !TypeVarInfo
					| TVI_AttrAndRefCount !TypeAttribute !Int
					| TVI_CorrespondenceNumber !Int /* auxiliary used in module comparedefimp */
					| TVI_AType !AType /* auxiliary used in module comparedefimp */
					| TVI_Reify !Int
					| TVI_Used /* to administer that this variable is encountered (in checkOpenTypes) */
					| TVI_TypeCode !TypeCodeExpression
					| TVI_CPSLocalTypeVar !Int /* MdM - the index of the variable as generated by the theorem prover */
					| TVI_Kinds ![TypeKind] // AA: used to collect kinds during checking 
					| TVI_Kind !TypeKind
					| TVI_ConsInstance !DefinedSymbol //AA: generic cons instance function 
					| TVI_Normalized !Int /* MV - position of type variable in its definition */
					| TVI_Expr !Bool !Expression	/* AA: Expression corresponding to the type var during generic specialization */
					| TVI_Exprs ![(GlobalIndex, Expression)] /* List of expressions corresponding to the type var during generic specialization */
					| TVI_Iso !DefinedSymbol !DefinedSymbol !DefinedSymbol
					| TVI_GenTypeVarNumber !Int
					| TVI_CPSTypeVar !CheatCompiler /* MdM: a pointer to a variable in CleanProverSystem is stored here, using a cast */
					| TVI_Attr !TypeAttribute
					| TVI_TypeAttribute !TypeAttribute

::	TypeVarInfoPtr	:== Ptr TypeVarInfo
::	TypeVarHeap 	:== Heap TypeVarInfo

::	AttrVarInfo  	= AVI_Empty
					| AVI_Attr !TypeAttribute
					| AVI_AttrVar !AttrVarInfoPtr // Sjaak: to collect universally quantified attribute variables
					| AVI_Forward !TempAttrId 
					| AVI_CorrespondenceNumber !Int /* auxiliary used in module comparedefimp */
					| AVI_Used
					/* auxiliary constructors used in anonymizeAttrVars in module typesupport: */
					| AVI_CountZero
					| AVI_CountOne
					| AVI_CountMany
					| AVI_CountVar !TypeVarInfoPtr
					/* */
					| AVI_SequenceNumber !Int // RWS
					| AVI_Collected // RWS

::	AttrVarInfoPtr	:== Ptr AttrVarInfo
::	AttrVarHeap 	:== Heap AttrVarInfo

::	TypeHeaps =
	{	th_vars		:: ! .TypeVarHeap
	,	th_attrs	:: ! .AttrVarHeap
	}

::	TypeVar =
	{	tv_ident				:: !Ident
	,	tv_info_ptr			:: !TypeVarInfoPtr
	}

::	ATypeVar =
	{	atv_attribute		:: !TypeAttribute
	,	atv_variable		:: !TypeVar
	}

::	TypeAttribute = TA_Unique | TA_Multi | TA_Var !AttributeVar | TA_RootVar AttributeVar | TA_TempVar !Int // | TA_TempExVar !Int
					| TA_Anonymous | TA_None
					| TA_List !Int !TypeAttribute | TA_Locked !TypeAttribute
					| TA_MultiOfPropagatingConsVar // only filled in after type checking, semantically equal to TA_Multi

::	AttributeVar =
	{	av_ident			:: !Ident
	,	av_info_ptr		:: !AttrVarInfoPtr
	}

::	Annotation	=  AN_Strict | AN_None

::	BasicType	= BT_Int | BT_Char | BT_Real | BT_Bool | BT_Dynamic
				| BT_File | BT_World
				| BT_String !Type /* the internal string type synonym only used to type string denotations */

::	BasicValue	= BVI !String | BVInt !Int |BVC !String | BVB !Bool | BVR !String | BVS !String

::	TypeKind = KindVar !KindInfoPtr | KindConst | KindArrow ![TypeKind] | KindCycle | KindError

instance toString 	TypeKind
instance <<< 		TypeKind
instance == 		TypeKind
instance toString 	KindInfo

/* A few obscure type definitions */

::	PatternVar =
	{	pv_var		:: !FreeVar
	,	pv_arg_nr	:: !Int
	}

::	Occurrence =
	{	occ_ref_count		:: !ReferenceCount
	,	occ_bind			:: !OccurrenceBinding
	,	occ_pattern_vars	:: ![[PatternVar]]
	,	occ_observing		:: (Bool, Ptr ExprInfo)
	,	occ_previous 		:: ![ReferenceCount]
	}

::	ReferenceCount = RC_Used !RC_Used | RC_Unused 

::	SelectiveUse =
	{	su_field	:: !Int
	,	su_multiply :: ![ExprInfoPtr]
	,	su_uniquely :: ![ExprInfoPtr]
	}

::	RC_Used =
	{ 	rcu_multiply	:: ![ExprInfoPtr]
	,	rcu_selectively :: ![SelectiveUse]
	,	rcu_uniquely	:: ![ExprInfoPtr]
	}

::	CountedFreeVar =
	{	cfv_var		:: !FreeVar
	,	cfv_is_let	:: !Bool
	,	cfv_count	:: !ReferenceCount
	}

::	OccurrenceBinding	= OB_Empty 
						| OB_OpenLet	!FreeVar !(Optional RefMarkResult)
						| OB_LockedLet	!OccurrenceBinding
						| OB_MarkedLet	!OccurrenceBinding

::	RefMarkResult :== ([CountedFreeVar], [FreeVar])

::	OptGuardedAlts	= GuardedAlts ![GuardedExpr] !(Optional ExprWithLocalDefs)
				 	| UnGuardedExpr !ExprWithLocalDefs

::	GuardedExpr =
	{	alt_nodes	:: ![NodeDefWithLocals]
	,	alt_guard	:: !ParsedExpr
	,	alt_expr	:: !OptGuardedAlts
	,	alt_ident	:: !Ident
	,	alt_position:: !Position
	}

::	ExprWithLocalDefs = 
	{	ewl_nodes	:: ![NodeDefWithLocals]
	,	ewl_expr	:: !ParsedExpr
	,	ewl_locals	:: !LocalDefs
	,	ewl_position:: !Position
	}

::	NodeDefWithLocals =
	{	ndwl_strict		:: !Bool
	,	ndwl_def		:: !Bind ParsedExpr ParsedExpr
	,	ndwl_locals		:: !LocalDefs
	,	ndwl_position	:: !Position
	}

::	CaseAlt =
	{	calt_pattern	:: !ParsedExpr
	,	calt_rhs		:: !Rhs
	,	calt_position	:: !Position
	}
	
:: LocalDef		:== ParsedDefinition

::	ParsedSelectorKind
		=	ParsedNormalSelector	// .
		|	ParsedUniqueSelector	// !
				!Bool					// 	is result element unique?

::	ParsedExpr	= PE_List ![ParsedExpr]
				| PE_Ident !Ident
				| PE_Basic !BasicValue
				| PE_Bound !BoundExpr
				| PE_Lambda !Ident ![ParsedExpr] !Rhs !Position
				| PE_Tuple ![ParsedExpr]
				| PE_Record !ParsedExpr !OptionalRecordName ![FieldAssignment]
				| PE_ArrayPattern ![ElemAssignment]
				| PE_UpdateComprehension !ParsedExpr !ParsedExpr !ParsedExpr ![Qualifier]
				| PE_ArrayDenot !ArrayKind ![ParsedExpr]
				| PE_Selection !ParsedSelectorKind !ParsedExpr ![ParsedSelection]
				| PE_Update !ParsedExpr [ParsedSelection] ParsedExpr
				| PE_Case !Ident !ParsedExpr [CaseAlt]
				| PE_If !Ident !ParsedExpr !ParsedExpr !ParsedExpr
				| PE_Let !LocalDefs !ParsedExpr
				| PE_ListCompr /*predef_cons_index:*/ !Int /*predef_nil_index:*/ !Int !ParsedExpr ![Qualifier]
				| PE_ArrayCompr !ArrayKind !ParsedExpr ![Qualifier]
				| PE_Sequ Sequence
				| PE_WildCard
				| PE_Matches !Ident /*expr*/!ParsedExpr /*pattern*/!ParsedExpr !Position

				| PE_QualifiedIdent !Ident !String

				| PE_ABC_Code ![String] !Bool
				| PE_Any_Code !(CodeBinding Ident) !(CodeBinding Ident) ![String]

				| PE_DynamicPattern !ParsedExpr !DynamicType
				| PE_Dynamic !ParsedExpr !(Optional DynamicType)
				
				| PE_Generic !Ident !TypeKind	/* AA: For generics, kind indexed identifier */

				| PE_TypeSignature !ArrayKind !ParsedExpr
				
				| PE_Empty

::	ParsedSelection	= PS_Record !Ident !OptionalRecordName
					| PS_QualifiedRecord !ModuleIdent !String !OptionalRecordName
					| PS_Array  !ParsedExpr
					| PS_Erroneous

::	OptionalRecordName
	= NoRecordName
	| RecordNameIdent !Ident
	| RecordNameQualifiedIdent !ModuleIdent !String
	
::	ModuleIdent:==Ident

::	ArrayKind = OverloadedArray | StrictArray | UnboxedArray;

::	GeneratorKind = IsListGenerator | IsOverloadedListGenerator | IsArrayGenerator
			
:: LineAndColumn = {lc_line :: !Int, lc_column :: !Int}

::	Generator =
	{	gen_kind	:: !GeneratorKind
	,	gen_pattern :: !ParsedExpr
	,	gen_expr	:: !ParsedExpr
	,	gen_position :: !LineAndColumn
	}

::	Qualifier	=
	{	qual_generators	:: ![Generator]
	,	qual_let_defs	:: !LocalDefs
	,	qual_filter		:: !Optional ParsedExpr
	,	qual_position	:: !LineAndColumn
	,	qual_filename	:: !FileName
	}

::	Sequence	= SQ_FromThen !Int ParsedExpr ParsedExpr
				| SQ_FromThenTo !Int ParsedExpr ParsedExpr ParsedExpr
				| SQ_From !Int ParsedExpr
				| SQ_FromTo !Int ParsedExpr ParsedExpr

::	BoundExpr	:== Bind ParsedExpr Ident

::	FieldAssignment :== Bind ParsedExpr FieldNameOrQualifiedFieldName

::	FieldNameOrQualifiedFieldName = FieldName !Ident | QualifiedFieldName !Ident !String
 
::	ElemAssignment :== Bind ParsedExpr [ParsedExpr]


cIsStrict		:== True
cIsNotStrict	:== False

::	SelectorKind
		=	NormalSelector
		|	UniqueSelector			// !
		|	UniqueSelectorUniqueElementResult
		|	UniqueSingleArraySelector
		|	UniqueSingleArraySelectorUniqueElementResult

::	Expression	= Var !BoundVar 
				| App !App
				| (@) infixl 9  !Expression ![Expression]
				| Let !Let
				| Case !Case
				| Selection !SelectorKind !Expression ![Selection]
				| Update !Expression ![Selection] Expression
				| RecordUpdate !(Global DefinedSymbol) !Expression ![Bind Expression (Global FieldSymbol)]
				| TupleSelect !DefinedSymbol !Int !Expression
				| BasicExpr !BasicValue
				| Conditional !Conditional

				| AnyCodeExpr !(CodeBinding BoundVar) !(CodeBinding FreeVar) ![String]
				| ABCCodeExpr ![String] !Bool

				| MatchExpr !(Global DefinedSymbol) !Expression
				| IsConstructor !Expression !(Global DefinedSymbol) /*arity*/!Int !GlobalIndex !Ident !Position
				| FreeVar FreeVar 
				| DictionariesFunction ![(FreeVar,AType)] !Expression !AType

				| Constant !SymbIdent !Int !Priority		/* auxiliary clause used during checking */
				| ClassVariable !VarInfoPtr					/* auxiliary clause used during overloading */

				| DynamicExpr !DynamicExpr
				| TypeCodeExpression !TypeCodeExpression

				| TypeSignature !(Int Int -> (AType,Int,Int)) !Expression

				| EE
				| NoBind ExprInfoPtr /* auxiliary, to store fields that are not specified in a record expression */ 
				| FailExpr !Ident // only allowed on (case) root positions

::	CodeBinding	variable :== Env String variable

::	App =
	{	app_symb 		:: !SymbIdent
	,	app_args 		:: ![Expression]
	,	app_info_ptr	:: !ExprInfoPtr
	}

::	Case =
	{	case_expr		:: !Expression
	,	case_guards		:: !CasePatterns
	,	case_default	:: !Optional Expression
	,	case_ident		:: !Optional Ident
	,	case_info_ptr	:: !ExprInfoPtr
	,	case_explicit	:: !Bool
	,	case_default_pos:: !Position
	}

::	Let =
	{	let_strict_binds	:: ![LetBind]
	,	let_lazy_binds		:: ![LetBind]
	,	let_expr			:: !Expression
	,	let_info_ptr		:: !ExprInfoPtr
	,	let_expr_position	:: !Position
	}

::	LetBind =
	{	lb_dst		:: !FreeVar
	,	lb_src		:: !Expression
	,	lb_position	:: !Position
	}

::	Conditional =
	{	if_cond		:: !Expression
	,	if_then		:: !Expression
	,	if_else		:: !Optional Expression
	}

::	DynamicExpr =
	{	dyn_expr		:: !Expression
	,	dyn_opt_type	:: !Optional DynamicType
	,	dyn_info_ptr	:: !ExprInfoPtr
	,	dyn_type_code	:: !TypeCodeExpression		/* filled after type checking */
	}	

::	CasePatterns= AlgebraicPatterns !GlobalIndex ![AlgebraicPattern]
				| BasicPatterns !BasicType [BasicPattern]
				| NewTypePatterns !GlobalIndex ![AlgebraicPattern]
				| DynamicPatterns [DynamicPattern]						/* auxiliary */
				| OverloadedListPatterns !OverloadedListType !Expression ![AlgebraicPattern]
				| NoPattern											/* auxiliary */

::	OverloadedListType	= UnboxedList !GlobalIndex !Index !Index !Index // list_type_symbol StdStrictLists module index, decons_u index, nil_u index
						| UnboxedTailStrictList !GlobalIndex !Index !Index !Index // list_type_symbol StdStrictLists module index, decons_uts index, nil_uts index
						| OverloadedList !GlobalIndex !Index !Index !Index // list_type_symbol StdStrictLists module index, decons index, nil index

instance == OverloadedListType

::	Selection	= RecordSelection !(Global DefinedSymbol) !Int
				| ArraySelection !(Global DefinedSymbol) !ExprInfoPtr !Expression
				| DictionarySelection !BoundVar ![Selection] !ExprInfoPtr !Expression

::	TypeCodeExpression	= TCE_Empty
						| TCE_Var 			!VarInfoPtr
						| TCE_TypeTerm		!VarInfoPtr
						| TCE_Constructor	!GlobalTCType		![TypeCodeExpression]
						| TCE_App			!TypeCodeExpression !TypeCodeExpression
						| TCE_Selector		![Selection]	!VarInfoPtr
						| TCE_UniType 		![VarInfoPtr] 	!TypeCodeExpression
						| TCE_UnqType		!TypeCodeExpression

::	GlobalTCType
	=	GTT_Basic !BasicType
	|	GTT_Constructor !SymbIdent !Bool/*is unique type*/
	|	GTT_PredefTypeConstructor !(Global Index)
	|	GTT_Function

::	AlgebraicPattern =
	{	ap_symbol	:: !(Global DefinedSymbol)
	,	ap_vars		:: ![FreeVar]
	,	ap_expr		:: !Expression
	,	ap_position	:: !Position
	}
	
::	BasicPattern =
	{	bp_value	:: !BasicValue
	,	bp_expr		:: !Expression
	,	bp_position	:: !Position
	}
	
::	DynamicPattern =
	{	dp_var					:: !FreeVar
	,	dp_type					:: !ExprInfoPtr
	,	dp_type_code			:: !TypeCodeExpression		/* filled after type checking */
	,	dp_rhs					:: !Expression
	,	dp_position				:: !Position
	}
	
/*
	error handling
*/

:: Position			= FunPos  FileName LineNr FunctName
					| LinePos FileName LineNr
					| PreDefPos Ident
					| NoPos

::	CoercionPosition
	=	CP_Expression !Expression
	|	CP_FunArg !Ident !Int // Function or constructor ident, argument position (>=1)
	|	CP_SymbArgAndExpression !SymbIdent !Int !Expression // Function or constructor symbol, argument position (>=1)
	|	CP_LiftedFunArg !Ident !Ident // Function symbol, lifted argument ident

::	IdentPos =
	{	ip_ident	:: !Ident
	,	ip_line		:: !Int
	,	ip_file		:: !FileName
	}

::	StringPos =
	{	sp_name		:: !String
	,	sp_line		:: !Int
	,	sp_file		:: !FileName
	}

:: FileName			:== String

:: FunctName		:== String

:: LineNr			:== Int

cNotALineNumber :== -1

/* Used for hashing identifiers */

instance == ModuleKind, Ident
instance <<< (Module a) | <<< a, ParsedDefinition, InstanceType, AttributeVar, TypeVar, SymbolType, Expression, Type, Ident, (Global object) | <<< object,
			 Position, CaseAlt, AType, FunDef, ParsedExpr, TypeAttribute, (Bind a b) | <<< a & <<< b,
			 FieldNameOrQualifiedFieldName, ParsedConstructor, (TypeDef a) | <<< a, TypeVarInfo, AttrVarInfo,
			 BasicValue, ATypeVar, TypeRhs, Import, ImportDeclaration, CasePatterns,
			 (Optional a) | <<< a, ConsVariable, BasicType, Annotation, SelectorKind, Selection, SelectorDef, ConsDef, LocalDefs, FreeVar, ClassInstance, SignClassification,
			 TypeCodeExpression, CoercionPosition, AttrInequality, LetBind, Declaration, STE_Kind, BoundVar,
			 TypeSymbIdent,
			 TypeCons,
			 IndexRange,
			 FunType,
			 GenericClassInfo,
			 TCClass, IdentOrQualifiedIdent

instance <<< FunctionBody

instance toString BasicType

instance == TypeAttribute
instance == Annotation
instance == GlobalIndex

instance toString TCClass

instance <<< FunCall

EmptySymbolTableEntry :== EmptySymbolTableEntryCAF.boxed_symbol_table_entry

::BoxedSymbolTableEntry = {boxed_symbol_table_entry::!SymbolTableEntry}

EmptySymbolTableEntryCAF :: BoxedSymbolTableEntry

cNotAGroupNumber :== -1

EmptyTypeDefInfo :== { tdi_kinds = [], tdi_properties = cAllBitsClear, tdi_group = [], tdi_group_vars = [], tdi_cons_vars = [],
					   tdi_classification = EmptyTypeClassification, tdi_group_nr = cNotAGroupNumber, tdi_index_in_group = NoIndex, tdi_gen_rep = No }

MakeTypeVar name	:== { tv_ident = name, tv_info_ptr = nilPtr }
MakeVar name		:== { var_ident = name, var_info_ptr = nilPtr, var_expr_ptr = nilPtr }

MakeAttributedType type :== { at_attribute = TA_None, at_type = type }
MakeAttributedTypeVar type_var :== { atv_attribute = TA_None, atv_variable = type_var }

EmptyFunInfo :== { fi_calls = [], fi_group_index = NoIndex, fi_def_level = NotALevel,
				   fi_free_vars = [], fi_local_vars = [], fi_dynamics = [], fi_properties=0 }

BottomSignClass		:== { sc_pos_vect = 0, sc_neg_vect = 0 }
PostiveSignClass	:== { sc_pos_vect = bitnot 0, sc_neg_vect = 0 }

NoPropClass			:== 0
PropClass			:== bitnot 0

newTypeSymbIdentCAF :: TypeSymbIdent;

MakeNewTypeSymbIdent name arity
	:== {newTypeSymbIdentCAF & type_ident=name, type_arity=arity }

MakeTypeSymbIdent type_index name arity
	:== {	newTypeSymbIdentCAF & type_ident = name, type_arity = arity, type_index = type_index }

ParsedSelectorToSelectorDef sd_type_index ps :==
	{	sd_ident = ps.ps_selector_ident, sd_field_nr = NoIndex, sd_pos =  ps.ps_field_pos, sd_type_index = sd_type_index,
		sd_exi_vars = [], sd_type_ptr = nilPtr, sd_field = ps.ps_field_ident,
		sd_type	= { st_vars = [], st_args = [], st_args_strictness=NotStrict, st_result = ps.ps_field_type, st_arity = 0, st_context = [],
				    st_attr_env = [], st_attr_vars = [] }}

ParsedConstructorToConsDef pc :==
	{	cons_ident = pc.pc_cons_ident, cons_pos = pc.pc_cons_pos, cons_priority = pc.pc_cons_prio, cons_number = NoIndex, cons_type_index = NoIndex,
		cons_type = { st_vars = [], st_args = pc.pc_arg_types, st_args_strictness=pc.pc_args_strictness, st_result = MakeAttributedType TE, 
				  st_arity = pc.pc_cons_arity, st_context = pc.pc_context, st_attr_env = [], st_attr_vars = []},
		cons_exi_vars = pc.pc_exi_vars, cons_type_ptr = nilPtr }

ParsedInstanceToClassInstance pi members member_types :==
	{	ins_class_index = {gi_module=NoIndex, gi_index=NoIndex},
		ins_class_ident = {ci_ident=pi.pi_class, ci_arity=length pi.pi_types}, ins_ident = pi.pi_ident,
		ins_type = { it_vars = [], it_types = pi.pi_types, it_attr_vars = [],
					 it_context = pi.pi_context }, 
		ins_members = members, ins_member_types = member_types, ins_specials = pi.pi_specials, ins_pos = pi.pi_pos}

MakeTypeDef name lhs rhs attr pos  :== 
	{	td_ident = name, td_index = -1, td_arity = length lhs, td_args = lhs, td_attrs = [], td_attribute = attr,
		td_pos = pos, td_rhs = rhs, td_used_types = [], td_fun_index = NoIndex
	}

MakeDefinedSymbol ident index arity :== { ds_ident = ident, ds_arity = arity, ds_index = index }

MakeNewFunctionType name arity prio type pos specials var_ptr
	:== { ft_ident = name, ft_arity = arity, ft_priority = prio, ft_type = type, ft_pos = pos, ft_specials = specials, ft_type_ptr = var_ptr  }

backslash :== '\\'
