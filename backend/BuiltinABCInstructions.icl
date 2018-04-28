implementation module BuiltinABCInstructions

import _SystemArray
import StdList
import StdMisc

import Text

import Cloogle

import CloogleDB

builtin_abc_instructions :: [ABCInstructionEntry]
builtin_abc_instructions =
	[ i_ccall
	, i_centry
	, i_halt
	, i_instruction
	, i_load_i
	, i_load_si16
	, i_load_si32
	, i_load_ui8
	, i_no_op
	, d_d
	, d_o
	: arith_instructions ++
	  pushes ++
	  [{zero & aie_instruction=i} \\ i <- other_instructions]
	]

instance zero ABCInstructionEntry
where
	zero =
		{ aie_instruction = undef
		, aie_arguments   = []
		, aie_description = "There is no documentation for this ABC instruction yet."
		}

LABEL   :== ABCArgument ABCTypeLabel  False
LABEL_  :== ABCArgument ABCTypeLabel  True
STRING  :== ABCArgument ABCTypeString False
STRING_ :== ABCArgument ABCTypeString True
BOOL    :== ABCArgument ABCTypeBool   False
BOOL_   :== ABCArgument ABCTypeBool   True
CHAR    :== ABCArgument ABCTypeChar   False
CHAR_   :== ABCArgument ABCTypeChar   True
INT     :== ABCArgument ABCTypeInt    False
INT_    :== ABCArgument ABCTypeInt    True
REAL    :== ABCArgument ABCTypeReal   False
REAL_   :== ABCArgument ABCTypeReal   True

i_ccall :: ABCInstructionEntry
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
		, "- sep    can be either `-` or `:`."
		, "- flags  can be `G` and/or `P`. `G` saves the state in global variables and is needed when the called C function will call Clean functions. `P` lets the callee pop arguments (necessary on 32-bit Windows)."
		, "- input  is a number of input argument types (allowed: `IpRrSsAOF`)."
		, "- output is a number of output argument types (allowed: `VIpRrSsAOF`)."
		, "- state  is a carried state that is not passed to the C function, for example used to thread {{`World`}} in ccalls (allowed: `IpRSA`)."
		, "\n"
		, "Input, output and state argument types can be:"
		, "- `I`    for integers"
		, "- `p`    for pointers (e.g. from {{`System.Pointer`}}; on most systems this is identical to `I`)"
		, "- [`Rr`] for reals"
		, "- `S`    for Clean Strings (`{#Char}`)."
		, "- `s`    for the characters of a Clean String (handy to use in conjuction with {{`packString`}})"
		, "- `A`    for A-stack elements (e.g. used for `*World`, a boxed integer under the hood)"
		, "- [`OF`] for function pointers"
		, "- `V`    for `void`, packs the following argument types in a tuple (e.g. `VIR` means `(Int, Real)`)"
		]
	}

i_centry :: ABCInstructionEntry
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

i_halt :: ABCInstructionEntry
i_halt =
	{ zero
	& aie_instruction = "halt"
	, aie_description = "Terminates the program immediately."
	}

i_instruction :: ABCInstructionEntry
i_instruction =
	{ zero
	& aie_instruction = "instruction"
	, aie_arguments   = [INT]
	, aie_description = "Adds the raw argument as a word in the generated object file."
	}

i_load_i :: ABCInstructionEntry
i_load_i =
	{ zero
	& aie_instruction = "load_i"
	, aie_arguments   = [INT]
	, aie_description = join "\n\n"
		[ "Take the top of the B-stack as a pointer and read an integer from that pointer with the argument as offset."
		, "See also {{`load_si16`}}, {{`load_si32`}}, {{`load_ui8`}}."
		]
	}

i_load_si16 :: ABCInstructionEntry
i_load_si16 =
	{ zero
	& aie_instruction = "load_si16"
	, aie_arguments   = [INT]
	, aie_description = join "\n\n"
		[ "Take the top of the B-stack as a pointer and read a 16-bit signed integer from that pointer with the argument as offset."
		, "See also {{`load_i}}, {{`load_si32`}}, {{`load_ui8`}}."
		]
	}

i_load_si32 :: ABCInstructionEntry
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

i_load_ui8 :: ABCInstructionEntry
i_load_ui8 =
	{ zero
	& aie_instruction = "load_ui8"
	, aie_arguments   = [INT]
	, aie_description = join "\n\n"
		[ "Take the top of the B-stack as a pointer and read a 8-bit unsigned integer from that pointer with the argument as offset."
		, "See also {{`load_i`}}, {{`load_si16`}}, {{`load_si32`}}."
		]
	}

i_no_op :: ABCInstructionEntry
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

d_d :: ABCInstructionEntry
d_d =
	{ zero
	& aie_instruction = ".d"
	, aie_arguments   = [INT, INT, STRING_]
	, aie_description = concat
		[ "Indicates how many stack elements are on the stack when a jump follows."
		, "The first integer is the number of elements on the A-stack; the second that of B-stack elements."
		, "The optional third argument indicates the type of the B-stack elements, e.g. `bbi` for two booleans and an integer."
		]
	}

d_o :: ABCInstructionEntry
d_o =
	{ zero
	& aie_instruction = ".o"
	, aie_arguments   = [INT, INT, STRING_]
	, aie_description = concat
		[ "Indicates how many stack elements are 'given back' to a calling function when a {{`rtn`}} follows."
		, "The first integer is the number of elements on the A-stack; the second that of B-stack elements."
		, "The optional third argument indicates the type of the B-stack elements, e.g. `bbi` for two booleans and an integer."
		]
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

	convert :: !String !String !String -> ABCInstructionEntry
	convert instr fr to =
		{ zero
		& aie_instruction = instr
		, aie_description = "Converts the " + fr + " on top of the B-stack to " + to + "."
		}

pushes :: [ABCInstructionEntry]
pushes =
	[ push "pushB"   "Char" BOOL 'B'
	, push "pushB_a" "Char" BOOL 'A'
	, push "pushC"   "Char" CHAR 'B'
	, push "pushC_a" "Char" CHAR 'A'
	, push "pushI"   "Int"  INT  'B'
	, push "pushI_a" "Int"  INT  'A'
	, push "pushR"   "Real" REAL 'B'
	, push "pushR_a" "Real" REAL 'A'
	]
where
	push :: !String !String !ABCArgument !Char -> ABCInstructionEntry
	push instr type arg stack =
		{ zero
		& aie_instruction = instr
		, aie_arguments   = [arg]
		, aie_description = "Pushes the " + type + " argument to the " + {stack} + "-stack."
		}

/**
 * Instructions without documentation yet
 */
other_instructions :: [String]
other_instructions =
	[ "add_args"
	, "addLU"
	, "build"
	, "buildB"
	, "buildC"
	, "buildI"
	, "buildR"
	, "buildAC"
	, "buildB_b"
	, "buildC_b"
	, "buildF_b"
	, "buildI_b"
	, "buildR_b"
	, "buildh"
	, "build_r"
	, "build_u"
	, "catS"
	, "call"
	, "cmpS"
	, "ceilingR"
	, "copy_graph"
	, "code_channelP"
	, "create"
	, "create_array"
	, "create_array_"
	, "create_channel"
	, "currentP"
	, "del_args"
	, "divLU"
	, "divU"
	, "eqB_a"
	, "eqB_b"
	, "eqC_a"
	, "eqC_b"
	, "eqD_b"
	, "eqI_a"
	, "eqI_b"
	, "eqR_a"
	, "eqR_b"
	, "eqAC_a"
	, "eq_desc"
	, "eq_desc_b"
	, "eq_nulldesc"
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
	, "get_node_arity"
	, "gtU"
	, "in"
	, "is_record"
	, "ItoP"
	, "jmp"
	, "jmp_ap"
	, "jmp_ap_upd"
	, "jmp_upd"
	, "jmp_eval"
	, "jmp_eval_upd"
	, "jmp_false"
	, "jmp_not_eqZ"
	, "jmp_true"
	, "jrsr"
	, "jsr"
	, "jsr_ap"
	, "jsr_eval"
	, "ltU"
	, "modI"
	, "mulUUL"
	, "new_ext_reducer"
	, "new_int_reducer"
	, "newP"
	, "out"
	, "pop_a"
	, "pop_b"
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
	, "push_a"
	, "push_b"
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
	, "push_t_r_a"
	, "push_t_r_args"
	, "push_r_args"
	, "push_r_args_a"
	, "push_r_args_b"
	, "push_r_args_u"
	, "push_r_arg_D"
	, "push_r_arg_t"
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
	, "rtn"
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
	, ".depend"
	, ".desc"
	, ".desc0"
	, ".descn"
	, ".descexp"
	, ".descs"
	, ".end"
	, ".endinfo"
	, ".export"
	, ".keep"
	, ".inline"
	, ".impdesc"
	, ".implab"
	, ".implib"
	, ".impmod"
	, ".impobj"
	, ".module"
	, ".n"
	, ".nu"
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
	, ".start"
	, ".string"
	]
