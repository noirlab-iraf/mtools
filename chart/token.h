include "pointer.h"

# ---------------------------------
# Token structure
# ---------------------------------

define	SZ_TOKEN    4	    	# Size of token structure

define	T_VALS	    Mems[P2S($1)]
define	T_VALI	    Memi[$1]
define	T_VALL	    Meml[P2L($1)]
define	T_VALR	    Memr[P2P($1)]
define	T_VALD	    Memd[P2D($1)]
define	T_VALB	    Memb[$1]

define	T_DTYPE	    Memi[($1)+2]
define	T_PREC	    Memi[($1)+2]

define	T_LEN	    Memi[($1)+3]

# --------------------------------
# Token types
# --------------------------------

define	CONSTANT		1
define	IDENTIFIER  	    	2
define	UNARY	    	    	3
define	BINARY	    	    	4

# -------------------------------
# Tokens / operators
# -------------------------------

# Binary-arithmetic operators
define	EXPONENTIATE	   1
define	MULTIPLY	   2
define	DIVIDE		   3
define	ADD		   4
define	SUBTRACT	   5
define	RIGHTPARENS 	   6
define	COMMA	    	   7

# Binary-logical operators
define	LESSEQUAL	   8
define	MOREEQUAL	   9
define	LESSTHAN	  10
define	MORETHAN	  11
define	EQUAL		  12
define	NOTEQUAL	  13
define	BOOL_AND	  14
define	BOOL_OR		  15
define	SUBSTRING   	  16
define	NOTSUBSTRING	  17

# Unary-logical operators
define	NOT		  18

# Unary-arithmetic operators
define	LEFTPARENS  	  19
define	UMINUS		  20
define	LOG		  21
define	LN		  22
define	DEXP		  23
define	EXP		  24
define	SQRT		  25
define	UNDEFINED    	  26
define	NINT	    	  27
define  ABS 	    	  28
define	SIGMA 	    	  29   # ERROR

define	MIN 	    	  30
define	MAX 	    	  31

define	PUSH_IDENTIFIER	  50
define	PUSH_CONSTANT	  51
define	STORE_TOP   	  52
define	RECALL_TOP  	  53
define	CHTYPE1	    	  54
define	CHTYPE2	    	  55
define	SEQUENCE    	  56
define	PRINTF	    	  57
define	QUESTION    	  58
define	COLON	    	  59
define	EPRINTF	    	  60
define	ERRORS_ON   	  61
define	ERRORS_OFF   	  62

define	END_OF_EXPRESSION 99
