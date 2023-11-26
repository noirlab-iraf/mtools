# GET_TOKEN -- Fetch and identify the next token in an expression.
# The current position in the expression to find the next token is
# passed in with "ip", which is updated after identifying a token to
# where the next token will be searched for.  The legal variable names
# are passed in with the "db" structure, and the identified token is
# passed out with the "tp" structure.
#
# The "tp" structure returns the following values depending on the token type:
#   IDENTIFIER
#	T_VALI(tp)  = identifier index (i.e. 3 for third database field)
#	T_DTYPE(tp) = identifier datatype
#	T_LEN(tp)   = maximum size of character string if datatype == TY_CHAR
#
#   CONSTANT
#	T_VAL$T(tp) = value of the constant for numeric constants, or
#		      array index for expr for starting char of string constant
#	T_DTYPE(tp) = datatype of constant (TY_INT, TY_DOUBLE, or TY_CHAR)
#	T_LEN(tp)   = length of string for string constant
#
#   UNARY or BINARY
#	T_VALI(tp)  = operator identifier
#	T_PREC(tp)  = operator precedence

include <ctype.h>
include <lexnum.h>
include "token.h"
include "database.h"

int procedure get_token (expr, ip, last_token, db, tp)
char	expr[ARB]	# Expression from which to fetch token
int	ip		# Current position in expr to find token
int	last_token	# The type of the last token read. Needed to determine
			# whether '-' is binary or unary.
pointer	db		# DATABASE pointer
pointer	tp		# Token structure pointer

char	ch, nextch, token[SZ_DBNAME+3]
int	ip_start, len, i, ctoi(), ctod(), strmatch(), lexnum(), strlen()
int	strncmp(), savelen, savei
int	ival
double	dval

begin
    # Skip leading white space
    while (IS_WHITE(expr[ip]))
	ip = ip + 1
    
    ch = expr[ip]
    nextch = expr[ip+1]

    switch (ch) {
    case EOS:
	T_VALI(tp) = END_OF_EXPRESSION
	T_PREC(tp) = 0
	return (BINARY)

    case '"':    # String constant
	ip_start = ip+1
	for (ip=ip_start; expr[ip] != ch; ip = ip + 1)
	    if (expr[ip] == EOS)
		call error (0, "missing closing quote in string constant")
	T_LEN(tp) = ip - ip_start
	T_DTYPE(tp) = TY_CHAR
	T_VALI(tp) = ip_start
	ip = ip + 1
	return (CONSTANT)

    case '.', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9': #Num const
    	switch (lexnum (expr, ip, len)) {
    	case LEX_OCTAL, LEX_HEX, LEX_DECIMAL:
	    # Don't allow octal or hexadecimal numbers.  Try reading it as
	    # a simple decimal number.
	    if (ctoi (expr, ip, ival) > 0) {
		T_DTYPE(tp) = TY_INT
	    	T_VALI(tp) = ival
	    	return (CONSTANT)
	    }
    	case LEX_REAL:
	    if (ctod (expr, ip, dval) > 0) {
	    	T_DTYPE(tp) = TY_DOUBLE
	    	T_VALD(tp) = dval
	    	return (CONSTANT)
	    }
    	default:
	    call error (0, "illegal numeric constant")
    	}

    # Maybe its an operator
    case '+':
	ip = ip + 1
	T_VALI(tp) = ADD
	T_PREC(tp) = 3
	return (BINARY)

    case '-':
	# Must determine whether a binary or unary minus
	ip = ip + 1
	if (last_token == IDENTIFIER || last_token == CONSTANT) {
	    T_VALI(tp) = SUBTRACT
	    T_PREC(tp) = 3
	    return (BINARY)
	} else {
	    T_VALI(tp) = UMINUS
	    T_PREC(tp) = 6
	    return (UNARY)
	}

    case '/':
	ip = ip + 1
	T_VALI(tp) = DIVIDE
	T_PREC(tp) = 4
	return (BINARY)

    case '*':
	ip = ip + 1
	if (nextch == '*') {
	    ip = ip + 1
	    T_VALI(tp) = EXPONENTIATE
	    T_PREC(tp) = 5
	} else {
	    T_VALI(tp) = MULTIPLY
	    T_PREC(tp) = 4
	}
	return (BINARY)

    case '<':
	ip = ip + 1
	if (nextch == '=') {
	    ip = ip + 1
	    T_VALI(tp) = LESSEQUAL
	} else {
	    T_VALI(tp) = LESSTHAN
	}
	T_PREC(tp) = 2
	return (BINARY)

    case '>':
	ip = ip + 1
	if (nextch == '=') {
	    ip = ip + 1
	    T_VALI(tp) = MOREEQUAL
	} else {
	    T_VALI(tp) = MORETHAN
	}
	T_PREC(tp) = 2
	return (BINARY)

    case '&':
	ip = ip + 1
	if (nextch == '&')
	    ip = ip + 1
	T_VALI(tp) = BOOL_AND
	T_PREC(tp) = 1
	return (BINARY)

    case '|':
	ip = ip + 1
	if (nextch == '|')
	    ip = ip + 1
	T_VALI(tp) = BOOL_OR
	T_PREC(tp) = 1
	return (BINARY)

    case '=':
	ip = ip + 1
	if (nextch != '=')
	    call error (0, "illegal operator: = should be ==")
	ip = ip + 1
	T_VALI(tp) = EQUAL
	T_PREC(tp) = 2
	return (BINARY)

    case '!':
	ip = ip + 1
	if (nextch == '=') {
	    ip = ip + 1
	    T_VALI(tp) = NOTEQUAL
	    T_PREC(tp) = 2
	    return (BINARY)
	} else if (nextch == '?') {
	    ip = ip + 1
	    T_VALI(tp) = NOTSUBSTRING
	    T_PREC(tp) = 2
	    return (BINARY)
	} else {
	    T_VALI(tp) = NOT
	    T_PREC(tp) = 6
	    return (UNARY)
	}

    case '?':
	ip = ip + 1
	if (nextch == '=') {
	    ip = ip + 1
	    T_VALI(tp) = SUBSTRING
	    T_PREC(tp) = 2
	} else {
	    T_VALI(tp) = QUESTION
	    T_PREC(tp) = 0
	}
	return (BINARY)

    case ':':
	ip = ip + 1
	T_VALI(tp) = COLON
	T_PREC(tp) = 0
	return (BINARY)

    case '(':
	ip = ip + 1
	T_VALI(tp) = LEFTPARENS
	T_PREC(tp) = 6
	return (UNARY)

    case ')':
	ip = ip + 1
	T_VALI(tp) = RIGHTPARENS
	# No precedence defined -- pops stack till meets left parens
	return (BINARY)

    case ',':
	ip = ip + 1
	T_VALI(tp) = COMMA
	# No precedence defined -- pops stack till meets left parens
	return (BINARY)

    default:
	# Must be either a function or database field identifier.
    }

    # Try the functions
    len = strlen ("log")
    if (strncmp("log", expr[ip], len) == 0) {
	ip = ip + len
	T_VALI(tp) = LOG
	T_PREC(tp) = 6
	return (UNARY)
    } 
    len = strlen ("ln")
    if (strncmp("ln", expr[ip], len) == 0) {
	ip = ip + len
    	T_VALI(tp) = LN
	T_PREC(tp) = 6
	return (UNARY)
    }
    len = strlen ("dexp")
    if (strncmp("dexp", expr[ip], len) == 0) {
	ip = ip + len
	T_VALI(tp) = DEXP
	T_PREC(tp) = 6
	return (UNARY)
    }
    len = strlen ("exp")
    if (strncmp("exp", expr[ip], len) == 0) {
	ip = ip + len
	T_VALI(tp) = EXP
	T_PREC(tp) = 6
	return (UNARY)
    }
    len = strlen ("sqrt")
    if (strncmp("sqrt", expr[ip], len) == 0) {
	ip = ip + len
	T_VALI(tp) = SQRT
	T_PREC(tp) = 6
	return (UNARY)
    }
    len = strlen ("abs")
    if (strncmp("abs", expr[ip], len) == 0) {
	ip = ip + len
	T_VALI(tp) = ABS
	T_PREC(tp) = 6
	return (UNARY)
    }
    len = strlen ("err")
    if (strncmp("err", expr[ip], len) == 0) {
	ip = ip + len
	T_VALI(tp) = SIGMA
	T_PREC(tp) = 6
	return (UNARY)
    }
    len = strlen ("indef")
    if (strncmp("indef", expr[ip], len) == 0) {
	ip = ip + len
	T_VALI(tp) = UNDEFINED
	T_PREC(tp) = 6
	return (UNARY)
    }
    len = strlen ("nint")
    if (strncmp("nint", expr[ip], len) == 0) {
	ip = ip + len
	T_VALI(tp) = NINT
	T_PREC(tp) = 6
	return (UNARY)
    }
    len = strlen ("min")
    if (strncmp("min", expr[ip], len) == 0) {
	ip = ip + len
	T_VALI(tp) = MIN
	T_PREC(tp) = 6
	return (UNARY)
    }
    len = strlen ("max")
    if (strncmp("max", expr[ip], len) == 0) {
	ip = ip + len
	T_VALI(tp) = MAX
	T_PREC(tp) = 6
	return (UNARY)
    }

    # Token must be a database field identifier.
    savelen = 0
    do i = 1, DB_NFIELDS(db) {
	# Match name at beginning of string, ignoring case.
	call sprintf (token, SZ_DBNAME+3, "^{%s}")
	    call pargstr (DB_NAME(db, i))
	len = strmatch (expr[ip], token)
	if (len > savelen) {
	    savelen = len
	    savei = i
	}
    }
    if (savelen > 0) {
	ip = ip + savelen - 1
	T_VALI(tp) = savei
	T_DTYPE(tp) = DB_TYPE(db, savei)
	if (DB_TYPE(db, savei) == TY_CHAR)
	    T_LEN(tp) = DB_SIZE(db, savei) - 1
	return (IDENTIFIER)
    }

    # Token not recognized.
    call error (0, "token not recognized")
end
