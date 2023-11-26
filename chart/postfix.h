# This file specifies the postfix stack structure.  The postfix stack is a
# stack of arithmetic operations to be executed in the arranged postfix
# sequence.

# The different elements of the structure contain different values depending
# on the operator stored in the PF_ACTION element.  The PF_ACTION(pf,ipf)
# element always contains the operation to perform (i.e. PUSH_IDENTIFIER,
# MULTPLY, LOG).  Below are enumerated the various types of PF_ACTIONs, and
# the associated values of the other structure elemnts for that action.
# combinations are enumberated below.
#
#   PUSH_IDENTIFIER
#   	PF_DTYPE1(pf, ipf)  = datatype of the identifier
#   	PF_ID(pf, ipf)	    = the identifier id (i.e. 3 for third db field)
#   	PF_VALP(pf, ipf)    = if identifier is a character string, the pointer
#   	    	    	      to the character array to hold its value
#
#   PUSH_CONSTANT
#   	PF_DTYPE1(pf, ipf)  = datatype of the constant
#   	PF_VAL$T(pf, ipf)   = if the constant is a character string, the
#   	    	    	      pointer to the string, else the value of the
#   	    	    	      constant
#
#   STORE_TOP
#   	PF_DTYPE1(pf, ipf)  = datatype of top element on the stack to store
#
#   RECALL_TOP
#   	PF_DTYPE1(pf, ipf)  = datatype of value to be recalled
#
#   END_OF_EXPRESSION
#   	PF_DTYPE1(pf, ipf)  = resultant datatype of the expression
#
#   Unary operator (i.e. LOG, UNDEFINED, UNARY, NOT)
#   	PF_DTYPE1(pf, ipf)  = datatype of top element of the stack (before op)
#
#   Binary operator (i.e. MULTIPLY, LESSTHAN, EQUAL, BOOL_AND, SUBSTRING
#   	PF_DTYPE1(pf, ipf)  = datatype of top element of the stack (before op)
#   	PF_DTYPE2(pf, ipf)  = datatype of next-to-the-top element of the stack
#   	    	    	      (before operation) --- this isn't used currently
#   	    	    	      since before binary operations the top and next-
#   	    	    	      to-the-top stack elements are forced to the same
#   	    	    	      datatype
#   CHTYPE1/CHTYPE2 	change the datatype of the top/next-to-the-top element
#   	PF_DTYPE1(pf, ipf)  = datatype to change from
#   	PF_DTYPE2(pf, ipf)  = datatype to change to
#
#   SEQUENCE
#   	PF_DTYPE1(pf, ipf)  = TY_INT (datatype of a sequence number)
#
#   PRINTF
#   	PF_DTYPE1(pf, ipf)  = datatype of top stack element (which is printed)
#   	PF_FD(pf, ipf)	    = file descriptor of file to print to
#   	PF_VALP(pf, ipf)    = pointer to format string
include "pointer.h"

define	SZ_POSTFIX  6	    	    # Size of postfix stack strucure
define  MAXDEPTH    300             # Maximum depth of postfix stack

define	PF_VALI	    Memi[($1)+SZ_POSTFIX*(($2)-1)]
define	PF_VALD	    Memd[P2D(($1)+SZ_POSTFIX*(($2)-1))]
define	PF_VALB	    Memb[($1)+SZ_POSTFIX*(($2)-1)]
define	PF_VALP	    Memp[($1)+SZ_POSTFIX*(($2)-1)]

define	PF_ACTION   Memi[($1)+SZ_POSTFIX*(($2)-1)+2]

define	PF_ID	    Memi[($1)+SZ_POSTFIX*(($2)-1)+3]
define	PF_FD	    Memi[($1)+SZ_POSTFIX*(($2)-1)+3]

define	PF_DTYPE1   Memi[($1)+SZ_POSTFIX*(($2)-1)+4]

define	PF_DTYPE2   Memi[($1)+SZ_POSTFIX*(($2)-1)+5]
