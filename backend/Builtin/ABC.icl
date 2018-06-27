implementation module Builtin.ABC

import _SystemArray
import StdList
import StdMisc

import Text

import Cloogle.API
import Cloogle.DB

builtin_abc_instructions :: [ABCInstructionEntry]
builtin_abc_instructions =
	[ i_create
	: arith_instructions ++
	  stack_operations ++
	  branches ++
	  miscellaneous ++
	  directives ++
	  [{zero & aie_instruction=i} \\ i <- other_instructions]
	]

instance zero ABCInstructionEntry
where
	zero =
		{ aie_instruction = undef
		, aie_arguments   = []
		, aie_description = "There is no documentation for this ABC instruction yet."
		}

LABEL    :== ABCArgument ABCTypeLabel        False
LABEL_   :== ABCArgument ABCTypeLabel        True
A_OFFSET :== ABCArgument ABCTypeAStackOffset False
B_OFFSET :== ABCArgument ABCTypeBStackOffset False
STRING   :== ABCArgument ABCTypeString       False
STRING_  :== ABCArgument ABCTypeString       True
BOOL     :== ABCArgument ABCTypeBool         False
BOOL_    :== ABCArgument ABCTypeBool         True
CHAR     :== ABCArgument ABCTypeChar         False
CHAR_    :== ABCArgument ABCTypeChar         True
INT      :== ABCArgument ABCTypeInt          False
INT_     :== ABCArgument ABCTypeInt          True
REAL     :== ABCArgument ABCTypeReal         False
REAL_    :== ABCArgument ABCTypeReal         True

i_create :: ABCInstructionEntry
i_create =
	{ zero
	& aie_instruction = "create"
	, aie_description = "Creates a new empty node and pushes its address to the A-stack."
	}

arith_instructions :: [ABCInstructionEntry]
arith_instructions =
	[ arith1 "absR"    "Real" "absolute value"
	, arith1 "acosR"   "Real" "arccosine"
	, arith1 "asinR"   "Real" "arcsine"
	, arith1 "atanR"   "Real" "arctangent"
	, arith1 "cosR"    "Real" "cosine"
	, arith1 "entierR" "Real" "(Int) entier (floor)"
	, arith1 "expR"    "Real" "exponential function (e^r)"
	, arith1 "lnR"     "Real" "natural logarithm"
	, arith1 "log10R"  "Real" "base-10 logarithm"
	, arith1 "notB"    "Bool" "logical negation"
	, arith1 "sinR"    "Real" "sine"
	, arith1 "sqrtR"   "Real" "square root"
	, arith1 "tanR"    "Real" "tangent"

	, op1 "decI" "Int"  "Decrements"
	, op1 "incI" "Int"  "Increments"
	, op1 "negI" "Int"  "Negates"
	, op1 "negR" "Real" "Negates"
	, op1 "not%" "Int"  "Bitwise negates"

	, op2 "addI"    "Int"  "Sums"
	, op2 "addR"    "Real" "Sums"
	, op2 "andB"    "Bool" "Logically conjuncts"
	, op2 "and%"    "Int"  "Bitwise conjuncts"
	, op2 "divI"    "Int"  "Divides"
	, op2 "divR"    "Real" "Divides"
	, op2 "eqB"     "Bool" "Checks equality on"
	, op2 "eqC"     "Char" "Checks equality on"
	, op2 "eqI"     "Int"  "Checks equality on"
	, op2 "eqR"     "Real" "Checks equality on"
	, i_eqAC_a
	, op2 "gtC"     "Char" "Checks greater-than on"
	, op2 "gtI"     "Int"  "Checks greater-than on"
	, op2 "gtR"     "Real" "Checks greater-than on"
	, op2 "ltC"     "Char" "Checks less-than on"
	, op2 "ltI"     "Int"  "Checks less-than on"
	, op2 "ltR"     "Real" "Checks less-than on"
	, op2 "mulI"    "Int"  "Multiplies"
	, op2 "mulR"    "Real" "Multiplies"
	, op2 "orB"     "Bool" "Logically disjuncts"
	, op2 "or%"     "Int"  "Bitwise disjuncts"
	, op2 "powR"    "Real" "Raises to the power on"
	, op2 "remI"    "Int"  "Computes the remainder after division of"
	, op2 "rotl%"   "Int"  "Bitwise left-rotate on"
	, op2 "rotr%"   "Int"  "Bitwise right-rotate on"
	, op2 "shiftl%" "Int"  "Bitwise left-shift on"
	, op2 "shiftr%" "Int"  "Bitwise right-shift on"
	, op2 "subI"    "Int"  "Subtracts"
	, op2 "subR"    "Real" "Subtracts"
	, op2 "xor%"    "Int"  "Bitwise XOR on"

	, eq_arg "Bool" BOOL 'A'
	, eq_arg "Bool" BOOL 'B'
	, eq_arg "Char" CHAR 'A'
	, eq_arg "Char" CHAR 'B'
	, eq_arg "Int"  INT  'A'
	, eq_arg "Int"  INT  'B'
	, eq_arg "Real" REAL 'A'
	, eq_arg "Real" REAL 'B'

	, convert "CtoI"  "Char" "Int"
	, convert "CtoAC" "Char" "String"
	, convert "ItoC"  "Int"  "Char"
	, convert "ItoR"  "Int"  "Real"
	, convert "RtoI"  "Real" "Int"
	]
where
	arith1 :: !String !String !String -> ABCInstructionEntry
	arith1 instr type description =
		{ zero
		& aie_instruction = instr
		, aie_description = "Computes the " + description + " of the " + type + " on top of the B-stack."
		}

	op1 :: !String !String !String -> ABCInstructionEntry
	op1 instr type description =
		{ zero
		& aie_instruction = instr
		, aie_description = description + " the " + type + " on top of the B-stack."
		}

	op2 :: !String !String !String -> ABCInstructionEntry
	op2 instr type description =
		{ zero
		& aie_instruction = instr
		, aie_description = description + " the two " + type + "s on top of the B-stack."
		}

	eq_arg :: !String !ABCArgument !Char -> ABCInstructionEntry
	eq_arg type arg stack =
		{ zero
		& aie_instruction = {'e','q',type.[0],'_',toLower stack}
		, aie_arguments   = [if (stack == 'A') A_OFFSET B_OFFSET, arg]
		, aie_description = "Checks equality between the " + {stack} + "-stack element and the second argument."
		}

	convert :: !String !String !String -> ABCInstructionEntry
	convert instr fr to =
		{ zero
		& aie_instruction = instr
		, aie_description = "Converts the " + fr + " on top of the B-stack to " + to + "."
		}

	i_eqAC_a =
		{ zero
		& aie_instruction = "eqAC_a"
		, aie_arguments   = [STRING]
		, aie_description = "Checks that the string on top of the A-stack equals the argument string."
		}

stack_operations :: [ABCInstructionEntry]
stack_operations =
	[ push    "Bool" BOOL
	, push    "Char" CHAR
	, push    "Int"  INT
	, push    "Real" REAL
	, push_a  "Bool"
	, push_a  "Char"
	, push_a  "Int"
	, push_a  "Real"
	, build   "Bool" BOOL
	, build   "Char" CHAR
	, build   "Int"  INT
	, build   "Real" REAL
	, i_buildAC
	, build_b "Bool"
	, build_b "Char"
	, build_b "Int"
	, build_b "Real"
	, i_create_array
	, i_create_array_
	, i_eq_desc
	, i_eq_desc_b
	, i_eq_nulldesc
	, i_pop_a
	, i_pop_b
	, i_push_a
	, i_push_b
	]
where
	push :: !String !ABCArgument -> ABCInstructionEntry
	push type arg =
		{ zero
		& aie_instruction = "push" + {type.[0]}
		, aie_arguments   = [arg]
		, aie_description = "Pushes the " + type + " argument to the B-stack."
		}

	push_a :: !String -> ABCInstructionEntry
	push_a type =
		{ zero
		& aie_instruction = "push" + {type.[0]} + "_a"
		, aie_arguments   = [INT]
		, aie_description = "Pushes the " + type + " from the nth position on the A-stack to the B-stack."
		}

	build :: !String !ABCArgument -> ABCInstructionEntry
	build type arg =
		{ zero
		& aie_instruction = "build" + {type.[0]}
		, aie_arguments   = [arg]
		, aie_description = "Builds a " + type + "-node with the argument as value on the A-stack."
		}

	build_b :: !String -> ABCInstructionEntry
	build_b type =
		{ zero
		& aie_instruction = "build" + {type.[0]} + "_b"
		, aie_arguments   = [INT]
		, aie_description = "Builds a " + type + "-node with the value on the nth position of the B-stack on the A-stack."
		}

	i_buildAC =
		{ zero
		& aie_instruction = "buildAC"
		, aie_arguments   = [STRING]
		, aie_description = "Pushes the argument string to the A-stack."
		}

	i_create_array =
		{ zero
		& aie_instruction = "create_array"
		, aie_arguments   = [LABEL, A_OFFSET, B_OFFSET]
		, aie_description = join " "
			[ "Creates an array on the A-stack."
			, "The elements have type `label` (which can be `_` for any A-stack type)."
			, "The last two arguments indicate the stack sizes of the elements."
			, "The size and initial value are popped from the B and A stacks."
			]
		}
	i_create_array_ =
		{ zero
		& aie_instruction = "create_array_"
		, aie_arguments   = [LABEL, A_OFFSET, B_OFFSET]
		, aie_description = join " "
			[ "Creates an array on the A-stack."
			, "The elements have type `label` (which can be `_` for any A-stack type)."
			, "The last two arguments indicate the stack sizes of the elements."
			, "The size is popped from the B-stack; the elements are initialised as `_Nil` regardless of the type."
			]
		}

	i_eq_desc =
		{ zero
		& aie_instruction = "eq_desc"
		, aie_arguments   = [LABEL, INT, A_OFFSET]
		, aie_description = join " "
			[ "Checks that the indicated node on the A-stack matches the descriptor given by the label."
			, "The `int` argument is the arity of the descriptor."
			]
		}
	i_eq_desc_b =
		{ zero
		& aie_instruction = "eq_desc_b"
		, aie_arguments   = [LABEL, A_OFFSET]
		, aie_description = join " "
			[ "The indicated node on the A-stack is assumed to be an array."
			, "The instruction checks that the array is of the type indicated by `label`."
			]
		}
	i_eq_nulldesc =
		{ zero
		& aie_instruction = "eq_nulldesc"
		, aie_arguments   = [LABEL, A_OFFSET]
		, aie_description = "Checks that the indicated node on the A-stack matches the descriptor given by the label, ignoring arity."
		}

	i_pop_a =
		{ zero
		& aie_instruction = "pop_a"
		, aie_arguments   = [A_OFFSET]
		, aie_description = "Pops elements off the A-stack until the referenced element."
		}
	i_pop_b =
		{ zero
		& aie_instruction = "pop_b"
		, aie_arguments   = [B_OFFSET]
		, aie_description = "Pops elements off the B-stack until the referenced element."
		}
	i_push_a =
		{ zero
		& aie_instruction = "push_a"
		, aie_arguments   = [A_OFFSET]
		, aie_description = "Pushes the referenced A-stack element on the A-stack."
		}
	i_push_b =
		{ zero
		& aie_instruction = "push_b"
		, aie_arguments   = [B_OFFSET]
		, aie_description = "Pushes the referenced B-stack element on the B-stack."
		}

branches :: [ABCInstructionEntry]
branches =
	[ i_jmp
	, i_jmp_false
	, i_jmp_true
	, i_jsr
	, i_jsr_eval
	, i_rtn
	]
where
	i_jmp =
		{ zero
		& aie_instruction = "jmp"
		, aie_arguments   = [LABEL]
		, aie_description = "Unconditional jump to a label."
		}
	i_jmp_false =
		{ zero
		& aie_instruction = "jmp_false"
		, aie_arguments   = [LABEL]
		, aie_description = "Jump to a label if the Bool on top of the B-stack is false."
		}
	i_jmp_true =
		{ zero
		& aie_instruction = "jmp_true"
		, aie_arguments   = [LABEL]
		, aie_description = "Jump to a label if the Bool on top of the B-stack is true."
		}
	i_jsr =
		{ zero
		& aie_instruction = "jsr"
		, aie_arguments   = [LABEL]
		, aie_description = "Subroutine jump to a label. {{`rtn`}} returns to the instruction after this `jsr`."
		}
	i_jsr_eval =
		{ zero
		& aie_instruction = "jsr_eval"
		, aie_arguments   = [A_OFFSET, LABEL]
		, aie_description = "Subroutine jump to evaluate the indicated A-stack element. {{`rtn`}} returns to the instruction after this `jsr_eval`."
		}
	i_rtn =
		{ zero
		& aie_instruction = "rtn"
		, aie_description = "Returns from a subroutine call (e.g. {{`jsr`}})."
		}

miscellaneous :: [ABCInstructionEntry]
miscellaneous =
	[ i_ccall
	, i_centry
	, i_get_node_arity
	, i_halt
	, i_instruction
	, i_load_i
	, i_load_si16
	, i_load_si32
	, i_load_ui8
	, i_no_op
	, i_push_r_arg_t
	, i_push_t_r_a
	, i_push_t_r_args
	]
where
	i_ccall =
		{ zero
		& aie_instruction = "ccall"
		, aie_arguments   = [LABEL, STRING]
		, aie_description = join "\n"
			[ "Calls a C function."
			, "Some of this is documented in https://svn.cs.ru.nl/repos/clean-tools/trunk/htoclean/CallingCFromClean.html"
			, "The first argument is the name of the function, the second is the signature."
			, "\n"
			, "The signature has to be of the form `flags?input?sep output(sep state)?`, where"
			, "  - sep    can be either `-` or `:`."
			, "  - flags  can be `G` and/or `P`. `G` saves the state in global variables and is needed when the called C function will call Clean functions. `P` lets the callee pop arguments (necessary on 32-bit Windows)."
			, "  - input  is a number of input argument types (allowed: `IpRrSsAOF`)."
			, "  - output is a number of output argument types (allowed: `VIpRrSsAOF`)."
			, "  - state  is a carried state that is not passed to the C function, for example used to thread {{`World`}} in ccalls (allowed: `IpRSA`)."
			, "\n"
			, "Input, output and state argument types can be:"
			, "  - `I`    for integers"
			, "  - `p`    for pointers (e.g. from {{`System.Pointer`}}; on most systems this is identical to `I`)"
			, "  - [`Rr`] for reals"
			, "  - `S`    for Clean Strings (`{#Char}`). If used as input type this passes a pointer to the string's length (number of characters). The actual array is at offset 4/8 (32/64 bit). If this is used as output type, the C function has to return a pointer to the string's length, followed by the actual string. A copy of the string will be created in the Clean heap, and this copy will be used by Clean. If the string was allocated in C, for example using malloc, it should be deallocated in C when it no longer used, it is not automatically deallocated by Clean."
			, "  - `s`    for the characters of a Clean String (handy to use in conjuction with {{`packString`}}, as the string is not null-terminated). The length (number of characters) is at offset -4/-8. (32/64 bit)."
			, "  - `A`    for A-stack elements. A pointer to the third block of the node is passed. For arrays, this is a pointer to the elements. The size of the array is one word higher in the block."
			, "  - [`OF`] for function pointers"
			, "  - `V`    for `void`, packs the following argument types in a tuple (e.g. `VIR` means `(Int, Real)`)"
			]
		}
	i_centry =
		{ zero
		& aie_instruction = "centry"
		, aie_arguments   = [LABEL, LABEL, STRING]
		, aie_description = join "\n"
			[ "Adds code to call a Clean function from C."
			, "Usually it is not needed to write this instruction yourself."
			, "It is generated with the `foreign export` construct.\n"
			, "The first label is the name of the C function to generate."
			, "The second label is the Clean function to link it to.\n"
			, "The string argument indicates the type."
			, "For more information, see {{`ccall`}}."
			]
		}

	i_get_node_arity =
		{ zero
		& aie_instruction = "get_node_arity"
		, aie_arguments   = [A_OFFSET]
		, aie_description = "Pushes the arity of the descriptor of the referenced A-stack element to the B-stack."
		}

	i_halt =
		{ zero
		& aie_instruction = "halt"
		, aie_description = "Terminates the program immediately."
		}

	i_instruction =
		{ zero
		& aie_instruction = "instruction"
		, aie_arguments   = [INT]
		, aie_description = "Adds the raw argument as a word in the generated object file."
		}

	i_load_i =
		{ zero
		& aie_instruction = "load_i"
		, aie_arguments   = [INT]
		, aie_description = join "\n\n"
			[ "Take the top of the B-stack as a pointer and read an integer from that pointer with the argument as offset."
			, "See also {{`load_si16`}}, {{`load_si32`}}, {{`load_ui8`}}."
			]
		}
	i_load_si16 =
		{ zero
		& aie_instruction = "load_si16"
		, aie_arguments   = [INT]
		, aie_description = join "\n\n"
			[ "Take the top of the B-stack as a pointer and read a 16-bit signed integer from that pointer with the argument as offset."
			, "See also {{`load_i}}, {{`load_si32`}}, {{`load_ui8`}}."
			]
		}
	i_load_si32 =
		{ zero
		& aie_instruction = "load_si32"
		, aie_arguments   = [INT]
		, aie_description = join "\n\n"
			[ "Take the top of the B-stack as a pointer and read a 32-bit signed integer from that pointer with the argument as offset."
			, "This instruction is only available on 64-bit systems. On 32-bit systems, {{`load_i`}} has the same effect."
			, "See also {{`load_i`}}, {{`load_si16`}}, {{`load_ui8`}}."
			]
		}
	i_load_ui8 =
		{ zero
		& aie_instruction = "load_ui8"
		, aie_arguments   = [INT]
		, aie_description = join "\n\n"
			[ "Take the top of the B-stack as a pointer and read a 8-bit unsigned integer from that pointer with the argument as offset."
			, "See also {{`load_i`}}, {{`load_si16`}}, {{`load_si32`}}."
			]
		}

	i_no_op =
		{ zero
		& aie_instruction = "no_op"
		, aie_description = join "\n"
			[ "Do nothing. This is for example useful in the `cast` function:\n"
			, "```clean"
			, "cast :: .a -> .b"
			, "cast _ = code {"
			, "\tno_op"
			, "}"
			, "```"
			]
		}

	i_push_r_arg_t =
		{ zero
		& aie_instruction = "push_r_arg_t"
		, aie_description = join " "
			[ "Gets the *n*th element from the type string of a record."
			, "The type string is on top of the B-stack; *n* below that."
			]
		}
	i_push_t_r_a =
		{ zero
		& aie_instruction = "push_t_r_a"
		, aie_arguments   = [A_OFFSET]
		, aie_description = "Push the address of the type string of the referenced record to the B-stack."
		}
	i_push_t_r_args =
		{ zero
		& aie_instruction = "push_t_r_args"
		, aie_description = "Pops a record from the A-stack, pushes its members in reversed order to both of the stacks, then pushes the address of the type string to the B-stack."
		}

directives :: [ABCInstructionEntry]
directives =
	[ d_d
	, d_n
	, d_nu
	, d_o
	, d_export
	, d_inline
	, d_module
	, d_depend
	, d_end
	, d_endinfo
	, d_start
	]
where
	d_d =
		{ zero
		& aie_instruction = ".d"
		, aie_arguments   = [A_OFFSET, B_OFFSET, STRING_]
		, aie_description = concat
			[ "Indicates how many stack elements are on the stack when a jump follows."
			, "The first integer is the number of elements on the A-stack; the second that of B-stack elements."
			, "The optional third argument indicates the type of the B-stack elements, e.g. `bbi` for two booleans and an integer."
			]
		}
	d_n =
		{ zero
		& aie_instruction = ".n"
		, aie_arguments   = [A_OFFSET, LABEL]
		, aie_description = concat
			[ "Indicates the arity of node entry labels."
			, "The label is the label of the corresponding descriptor, or `_` if it does not exist."
			, "\n\nThere are some special cases:\n\n"
			, "- An arity of `-1` is for tuple selectors;\n"
			, "- An arity of `-2` is for indirection nodes;\n"
			, "- An arity of `-3` is for record selectors of basic types;\n"
			, "- An arity of `-4` is for record selectors of non-basic types.\n\n"
			, "See also {{`.nu`}}."
			]
		}
	d_nu =
		{ zero
		& aie_instruction = ".nu"
		, aie_arguments   = [A_OFFSET, B_OFFSET, LABEL]
		, aie_description = concat
			[ "Indicates the arity of node entry labels with arguments on the B-stack (otherwise, {{`.n`}} is used)."
			, "The first integer is the number of A-stack arguments; the second the number of B-stack arguments."
			, "The label is the label of the corresponding descriptor, or `_` if it does not exist."
			]
		}
	d_o =
		{ zero
		& aie_instruction = ".o"
		, aie_arguments   = [A_OFFSET, B_OFFSET, STRING_]
		, aie_description = concat
			[ "Indicates how many stack elements are 'given back' to a calling function when a {{`rtn`}} follows."
			, "The first integer is the number of elements on the A-stack; the second that of B-stack elements."
			, "The optional third argument indicates the type of the B-stack elements, e.g. `bbi` for two booleans and an integer."
			]
		}

	d_export =
		{ zero
		& aie_instruction = ".export"
		, aie_arguments   = [LABEL]
		, aie_description = "Exports a label (allows linking)."
		}
	d_inline =
		{ zero
		& aie_instruction = ".inline"
		, aie_arguments   = [LABEL]
		, aie_description = "Indicates that a label can (should) be inlined (usually for performance reasons)."
		}

	d_module =
		{ zero
		& aie_instruction = ".module"
		, aie_arguments   = [LABEL, STRING]
		, aie_description = "Indicates the name of the module, and its label in the data segment."
		}
	d_depend =
		{ zero
		& aie_instruction = ".depend"
		, aie_arguments   = [STRING]
		, aie_description = "Indicates a module that this module depends on."
		}
	d_end =
		{ zero
		& aie_instruction = ".end"
		, aie_description = "Indicates the end of the ABC file."
		}
	d_endinfo =
		{ zero
		& aie_instruction = ".endinfo"
		, aie_description = "Indicates the end of the metadata in the ABC file."
		}
	d_start =
		{ zero
		& aie_instruction = ".start"
		, aie_arguments   = [LABEL]
		, aie_description = "Indicates the label to start execution at."
		}

/**
 * Instructions without documentation yet
 */
other_instructions :: [String]
other_instructions =
	[ "add_args"
	, "addLU"
	, "build"
	, "buildF_b"
	, "buildh"
	, "build_r"
	, "build_u"
	, "catS"
	, "call"
	, "cmpS"
	, "ceilingR"
	, "copy_graph"
	, "code_channelP"
	, "create_channel"
	, "currentP"
	, "del_args"
	, "divLU"
	, "divU"
	, "eqD_b"
	, "eq_symbol"
	, "exit_false"
	, "fill"
	, "fill1"
	, "fill2"
	, "fill3"
	, "fill1_r"
	, "fill2_r"
	, "fill3_r"
	, "fillcaf"
	, "fillcp"
	, "fillcp_u"
	, "fill_u"
	, "fillh"
	, "fillB"
	, "fillB_b"
	, "fillC"
	, "fillC_b"
	, "fillF_b"
	, "fillI"
	, "fillI_b"
	, "fillR"
	, "fillR_b"
	, "fill_a"
	, "fill_r"
	, "floordivI"
	, "getWL"
	, "get_desc_arity"
	, "get_desc_flags_b"
	, "get_desc0_number"
	, "gtU"
	, "in"
	, "is_record"
	, "ItoP"
	, "jmp_ap"
	, "jmp_ap_upd"
	, "jmp_upd"
	, "jmp_eval"
	, "jmp_eval_upd"
	, "jmp_not_eqZ"
	, "jrsr"
	, "jsr_ap"
	, "ltU"
	, "modI"
	, "mulUUL"
	, "new_ext_reducer"
	, "new_int_reducer"
	, "newP"
	, "out"
	, "print"
	, "printD"
	, "print_char"
	, "print_int"
	, "print_real"
	, "print_r_arg"
	, "print_sc"
	, "print_symbol"
	, "print_symbol_sc"
	, "pushcaf"
	, "push_finalizers"
	, "pushA_a"
	, "pushD"
	, "pushD_a"
	, "pushF_a"
	, "pushL"
	, "pushLc"
	, "pushzs"
	, "push_a_b"
	, "push_arg"
	, "push_arg_b"
	, "push_args"
	, "push_args_u"
	, "push_array"
	, "push_arraysize"
	, "push_b_a"
	, "push_node"
	, "push_node_u"
	, "push_a_r_args"
	, "push_r_args"
	, "push_r_args_a"
	, "push_r_args_b"
	, "push_r_args_u"
	, "push_r_arg_D"
	, "push_r_arg_u"
	, "push_wl_args"
	, "pushZ"
	, "pushZR"
	, "putWL"
	, "randomP"
	, "release"
	, "remU"
	, "replace"
	, "repl_arg"
	, "repl_args"
	, "repl_args_b"
	, "repl_r_args"
	, "repl_r_args_a"
	, "select"
	, "send_graph"
	, "send_request"
	, "set_continue"
	, "set_defer"
	, "set_entry"
	, "set_finalizers"
	, "setwait"
	, "shiftrU"
	, "sincosR"
	, "sliceS"
	, "stop_reducer"
	, "subLU"
	, "addIo"
	, "mulIo"
	, "subIo"
	, "suspend"
	, "testcaf"
	, "truncateR"
	, "update_a"
	, "updatepop_a"
	, "update_b"
	, "updatepop_b"
	, "updateS"
	, "update"
	, ".algtype"
	, ".caf"
	, ".code"
	, ".comp"
	, ".a"
	, ".desc"
	, ".desc0"
	, ".descn"
	, ".descexp"
	, ".descs"
	, ".keep"
	, ".impdesc"
	, ".implab"
	, ".implib"
	, ".impmod"
	, ".impobj"
	, ".newlocallabel"
	, ".n_string"
	, ".pb"
	, ".pd"
	, ".pe"
	, ".pl"
	, ".pld"
	, ".pn"
	, ".pt"
	, ".record"
	, ".string"
	]
