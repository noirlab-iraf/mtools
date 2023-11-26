include <error.h>
include <ctype.h>
include "postfix.h"
include "token.h"
include	"markers.h"
include "database.h"

# EVAL_EXPR -- Evaluate an expression for the entire database.  The ouput array
# should be allocated before calling this routine.  This procedure assumes
# that the expression has been previously checked for validity.  Thus any
# errors it finds are fatal.

procedure eval_expr (expr, db, index, output, eoutput, datatype, errors)
char	expr[ARB]	    # expression to be evaluated
pointer	db		    # DATABASE pointer
int	index[ARB]	    # good element index
pointer	output		    # pointer to array holding output
pointer	eoutput		    # pointer to array holding output errors
int	datatype	    # desired datatype of output
bool	errors		    # calculate and output the errors?

pointer	sp, pf
int	ipf, dtype, parse()

begin
    # allocate postfix and variable stack structures
    call smark (sp)
    call salloc (pf, SZ_POSTFIX*MAXDEPTH, TY_STRUCT)

    # Parse expression
    dtype = parse (expr, db, pf, ipf, errors, true)
    if (dtype == ERR)
	call fatal (0, "bad expression in eval_expr")

    # Check against obvious mismatches between desired and resultant datatypes
    if ((datatype == TY_CHAR || dtype == TY_CHAR) && datatype != dtype)
	call fatal (0, "datatype mismatch in eval_expr")
    if ((datatype == TY_BOOL || dtype == TY_BOOL) && datatype != dtype)
	call fatal (0, "datatype mismatch in eval_expr")
    
    # Evaluate the expression
    call evaluate (pf, ipf, db, index, output, eoutput, datatype, dtype)

    # Deallocate character strings on postfix stack
    call clean_pf (pf, ipf)
    call sfree (sp)
end

# TEST_EXPR -- Test an expression.  Returns the datatype of the result if
# its a legal expression, else returns ERR.  Possible datatypes are TY_INT,
# TY_DOUBLE, TY_CHAR, and TY_BOOL.

int procedure test_expr (expr, db, spit)
char	expr[ARB]	    # expression to be evaluated
pointer	db		    # DATABASE pointer
bool	spit		    # print error message for bad string?

pointer	sp, pf
int	ipf, datatype, parse()

begin
    call smark (sp)
    call salloc (pf, SZ_POSTFIX*MAXDEPTH, TY_STRUCT)
    datatype = parse (expr, db, pf, ipf, false, spit)
    if (datatype != ERR)
	call clean_pf (pf, ipf)
    call sfree (sp)
    return (datatype)
end

# NUM_EXPR -- Test whether expression is numeric or not (i.e. whether the
# resultant datatype is int or double).  Returns true if
# expression is numeric, else returns false.

bool procedure num_expr (expr, db, spit)
char	expr[ARB]	    # expression to be evaluated
pointer	db		    # DATABASE pointer
bool	spit		    # print error message for bad string?

int	test_expr()

begin
    switch (test_expr (expr, db, spit)) {
    case TY_INT, TY_DOUBLE:
	return (true)
    case TY_BOOL, TY_CHAR:
	call eprintf ("Warning: Expression is not numeric (%s)\n")
	    call pargstr (expr)
	return (false)
    case ERR:
	return (false)
    }
end

# SIZE_EXPR -- Test whether expression is a proper sizing function.  This is
# just testing whether or not its numeric, with the added proviso that it can
# start with a '~' to indicate that the size should be proportional to the
# expression rather than equal to it.  Returns true if expression is legal,
# else returns false.

bool procedure size_expr (expr, db, spit)
char	expr[ARB]	    # expression to be evaluated
pointer	db		    # DATABASE pointer
bool	spit		    # print error message for bad string?

int	ip
bool	num_expr(), streq()

begin
    ip = 1
    while (IS_WHITE(expr[ip]))
	ip = ip + 1
    if (streq (expr[ip], "error") || streq (expr[ip], "err") ||
        streq (expr[ip], "ERROR") || streq (expr[ip], "ERR"))
	return (true)
    if (streq (expr[ip], "same") || streq (expr[ip], "SAME"))
	return (true)
    if (expr[ip] == '~')
	ip = ip + 1
    return (num_expr (expr[ip], db, spit))
end

# BOOL_EXPR -- Test whether expression is boolean or not.  Returns true if
# expression is boolean, else returns false.

bool procedure bool_expr (expr, db, spit)
char	expr[ARB]	    # expression to be evaluated
pointer	db		    # DATABASE pointer
bool	spit		    # print error message for bad string?

int	test_expr()

begin
    switch (test_expr (expr, db, spit)) {
    case TY_BOOL:
	return (true)
    case TY_INT, TY_DOUBLE, TY_CHAR:
	call eprintf ("Warning: Expression is not boolean (%s)\n")
	    call pargstr (expr)
	return (false)
    case ERR:
	return (false)
    }
end

# TEST_MARKER -- Test whether a marker expression is legal.  Returns OK if
# legal, else returns ERR.
int procedure test_marker (expr, db, marked)
char	expr[ARB]	    # expression to be evaluated
pointer	db		    # DATABASE pointer
int	marked		    # Marker type for marked points

int	marktype, strdic()
bool	bool_expr()
pointer	sp, marker, subexpr

begin
    # Allocate strings
    call smark (sp)
    call salloc (marker, SZ_MARKERSTRING, TY_CHAR)
    call salloc (subexpr, SZ_LINE, TY_CHAR)

    # Fetch marker and expression
    call sscan (expr)
	call gargwrd (Memc[marker], SZ_MARKERSTRING)
	call gargstr (Memc[subexpr], SZ_LINE)

    # Test marker
    marktype = strdic (Memc[marker], Memc[marker], SZ_MARKERSTRING, MARKS)
    if (marktype == 0) {
	call eprintf ("Warning: unrecognized marker (%s)\n")
	    call pargstr (Memc[marker])
	call sfree (sp)
	return (ERR)
    }
    # Reserve marker for unmarked points
    if (marktype == marked) {
	call eprintf ("Warning: marker '%s' is reserved for 'marked' points\n")
	    call pargstr (Memc[marker])
	call sfree (sp)
	return (ERR)
    }

    # Test expression
    if (bool_expr (Memc[subexpr], db, true)) {
	call sfree (sp)
	return (OK)
    } else {
	call sfree (sp)
	return (ERR)
    }
end

# TEST_COLOR -- Test whether a color expression is legal.  Returns OK if
# legal, else returns ERR.
int procedure test_color (expr, db)
char	expr[ARB]	    # expression to be evaluated
pointer	db		    # DATABASE pointer

int	colortype, strdic()
bool	bool_expr()
pointer	sp, color, subexpr

begin
    # Allocate strings
    call smark (sp)
    call salloc (color, SZ_COLORSTRING, TY_CHAR)
    call salloc (subexpr, SZ_LINE, TY_CHAR)

    # Fetch marker and expression
    call sscan (expr)
	call gargwrd (Memc[color], SZ_COLORSTRING)
	call gargstr (Memc[subexpr], SZ_LINE)

    # Test marker
    colortype = strdic (Memc[color], Memc[color], SZ_COLORSTRING, COLORS)
    if (colortype == 0) {
	call eprintf ("Warning: unrecognized color (%s)\n")
	    call pargstr (Memc[color])
	call sfree (sp)
	return (ERR)
    }

    # Test expression
    if (bool_expr (Memc[subexpr], db, true)) {
	call sfree (sp)
	return (OK)
    } else {
	call sfree (sp)
	return (ERR)
    }
end

# PARSE -- Parse an expression, compiling it into a postfix command stack.
# The space for the postfix command stack must be allocated before calling
# this procedure.  Returns ERR if an illegal expression, otherwise it
# returns the data type of the result of the expression (TY_INT, TY_DOUBLE,
# TY_CHAR, or TY_BOOL).  Additional space will
# be allocated to hold string constants and variables on the postfix stack.
# This space must be cleared by the calling procedure, using the procedure
# "clean_pf".  If an error is returned, this space will already have been
# cleared, and an appropriate error message will be printed
# ("Warning: illegal expression (expr): description of error").
#
# The calling sequence is:
#	include "postfix.h"
#	...
#	call smark (sp)
# 	call salloc (pf, MAXDEPTH*SZ_POSTFIX, TY_STRUCT)
#	datatype = parse (expr, db, pf, ipf, errors)
#	if (datatype != ERR) {
#	    ...
#	    call clean_pf (pf, ipf)
#	}
#	call sfree (sp)

define	VS_DTYPE	Memi[($1)+($2)-1]	# Variable datatype stack

define	SZ_OSTACK	2	# Size of operator stack element
define	OS_ID		Memi[($1)+SZ_OSTACK*(($2)-1)]
define	OS_PREC		Memi[($1)+SZ_OSTACK*(($2)-1)+1]

int procedure parse (expr, db, pf, ipf, errors, spit)
char	expr[ARB]   	    # expression to be evaluated
pointer	db		    # DATABASE pointer
pointer	pf		    # pointer to the postfix command stack
int	ipf		    # on output, number of commands in postfix stack
bool	errors		    # calculate and output the errors?
bool	spit		    # print error message for bad string?

pointer	vs		    # variable stack pointer
pointer os		    # operator stack pointer
pointer	tp		    # token pointer
int	ivs, ios	    # variable and operator stack indexes
int	ip		    # expression array (expr) index
int 	datatype, last_token, get_token(), save, j
pointer	sp
errchk	pop_op, get_token
bool	calcErrors

begin
    call smark (sp)
    call salloc (vs, MAXDEPTH, TY_INT)
    call salloc (os, SZ_OSTACK*MAXDEPTH, TY_INT)
    call salloc (tp, SZ_TOKEN, TY_STRUCT)
    ipf = 0
    ivs = 0
    ios = 0
    ip = 1
    last_token = BINARY
    # Turn on error calculation if called for
    if (errors) {
	ipf = ipf + 1
    	PF_ACTION(pf, ipf) = ERRORS_ON
    }
    calcErrors = errors
    iferr {
      repeat {
	j = ip
	switch (get_token (expr, ip, last_token, db, tp)) {
	case IDENTIFIER:
	    if (last_token == IDENTIFIER || last_token == CONSTANT)
		call error (0, "bad mix of operators and operands")
	    ivs = ivs + 1
	    switch (T_DTYPE(tp)) {
	    case TY_SHORT, TY_INT, TY_LONG:
		VS_DTYPE(vs, ivs) = TY_INT
	    case TY_REAL, TY_DOUBLE:
		VS_DTYPE(vs, ivs) = TY_DOUBLE
	    default:	# CHAR or BOOL
		VS_DTYPE(vs, ivs) = T_DTYPE(tp)
	    }
	    ipf = ipf + 1
	    PF_ACTION(pf, ipf) = PUSH_IDENTIFIER
	    PF_DTYPE1(pf, ipf) = T_DTYPE(tp)
	    if (calcErrors && ! DB_ERROR(db,T_VALI(tp)))
		call error (0, "field has no error")
	    PF_ID(pf, ipf) = T_VALI(tp)
	    if (T_DTYPE(tp) == TY_CHAR)
		call malloc (PF_VALP(pf, ipf), T_LEN(tp), TY_CHAR)
	    last_token = IDENTIFIER

	case CONSTANT:
	    if (last_token == IDENTIFIER || last_token == CONSTANT)
		call error (0, "bad mix of operators and operands")
    	    ivs = ivs + 1
	    VS_DTYPE(vs, ivs) = T_DTYPE(tp)
	    ipf = ipf + 1
	    PF_ACTION(pf, ipf) = PUSH_CONSTANT
	    PF_DTYPE1(pf, ipf) = T_DTYPE(tp)
	    switch (T_DTYPE(tp)) {
	    case TY_INT:
	    	PF_VALI(pf, ipf) = T_VALI(tp)
	    case TY_DOUBLE:
	    	PF_VALD(pf, ipf) = T_VALD(tp)
	    case TY_CHAR:
		call malloc (PF_VALP(pf, ipf), T_LEN(tp), TY_CHAR)
		call strcpy (expr[T_VALI(tp)], Memc[PF_VALP(pf,ipf)],T_LEN(tp))
	    }
	    last_token = CONSTANT

	case UNARY:
	    if (last_token == IDENTIFIER || last_token == CONSTANT)
		call error (0, "bad mix of operators and operands")
	    # Unary operators always have the highest precedence, and so
	    # are simply put on the operator stack.  They can't pop the stack.
	    # Since they're right associative, they can't pop another unary
	    # operator either (binary operators are treated in this program
	    # as left associative, unarys as right associative).
	    ios = ios + 1
	    OS_ID(os, ios) = T_VALI(tp)
	    OS_PREC(os, ios) = T_PREC(tp)
	    last_token = UNARY
	    # If operand is ERR, then turn error computation on now
	    if (OS_ID(os, ios) == SIGMA) {
		if (calcErrors)
		    call error (0, "multiple layers of errors")
		ipf = ipf + 1
		calcErrors = true
	    	PF_ACTION(pf, ipf) = ERRORS_ON
	    }

	case BINARY:
	    if (last_token == UNARY || last_token == BINARY)
		call error (0, "bad mix of operators and operands")
	    if (T_VALI(tp) == COLON) {
		# Second part of choice operator.  Pop operator stack until a
		# '?' operator is encountered.
		while (ios > 0)
		    if (OS_ID(os, ios) != QUESTION) {
			if (OS_ID(os,ios) == SIGMA) calcErrors = false
			call pop_op(os, ios, vs, ivs, pf, ipf)
		    } else
			break
		if (ios == 0)
		    call error (0, "missing '?' for ':'")
	    	OS_ID(os, ios) = T_VALI(tp)
	    	OS_PREC(os, ios) = T_PREC(tp)
	    	last_token = BINARY
	    } else if (T_VALI(tp) == RIGHTPARENS || T_VALI(tp) == COMMA) {
		# Token is a right parenthesis or a comma.  Pop operator stack
		# until a left parenthesis is encountered.
		while (ios > 0)
		    if (OS_ID(os, ios) != LEFTPARENS) {
			if (OS_ID(os,ios) == SIGMA) calcErrors = false
		    	call pop_op(os, ios, vs, ivs, pf, ipf)
		    } else
			break
		if (ios == 0)
		    call error (0, "missing left parenthesis")
		if (T_VALI(tp) == RIGHTPARENS) {
		    ios = ios - 1  # Remove the left parens from the op stack
		    last_token = IDENTIFIER
		} else		# comma
		    last_token = BINARY
	    } else {
		# Left associative.  Pop operator stack if token is lower or
		# equal precedence, unless both operator stack and token
		# have precedence of 2 (i.e. <, >=, ==) in which case we must
		# check to see if its a range specification.
		while (ios > 0)
		    if (((T_PREC(tp) < OS_PREC(os, ios)) ||
			 (T_PREC(tp) == OS_PREC(os, ios) && T_PREC(tp) !=2)) &&
			OS_ID(os, ios) != LEFTPARENS) {
			if (OS_ID(os,ios) == SIGMA) calcErrors = false
		    	call pop_op(os, ios, vs, ivs, pf, ipf)
		    } else
			break
		# Check for a range specification (i.e. "19 < J < 21").
		if (ios > 0)
	            if (((T_VALI(tp) == LESSEQUAL || T_VALI(tp) == LESSTHAN) &&
		    (OS_ID(os,ios)==LESSEQUAL || OS_ID(os,ios) == LESSTHAN)) ||
	            ((T_VALI(tp) == MOREEQUAL || T_VALI(tp) == MORETHAN) &&
		    (OS_ID(os,ios)==MOREEQUAL || OS_ID(os,ios) == MORETHAN))) {
		    	ipf = ipf + 1
		    	PF_ACTION(pf,ipf)= STORE_TOP
	    	    	PF_DTYPE1(pf, ipf) = VS_DTYPE(vs, ivs)
		    	save = VS_DTYPE(vs, ivs)
		    	call pop_op (os, ios, vs, ivs, pf, ipf)
		    	ios = ios + 1
		    	OS_ID(os, ios) = BOOL_AND
		    	OS_PREC(os, ios) = 1
		    	ipf = ipf + 1
		    	PF_ACTION(pf,ipf) = RECALL_TOP
	    	    	PF_DTYPE1(pf, ipf) = save
		    	ivs = ivs + 1
		    	VS_DTYPE(vs, ivs) = save
	    	    }
		if (T_VALI(tp) == END_OF_EXPRESSION)
		    break
	    	ios = ios + 1
	    	OS_ID(os, ios) = T_VALI(tp)
	    	OS_PREC(os, ios) = T_PREC(tp)
	    	last_token = BINARY
	    }
	}
      }
    } then {
	# Error.  Clean up allocated character strings on postfix stack
	# and return as an error.
	if (spit) {
	    call eprintf ("Warning: illegal expression (%s): ")
	        call pargstr (expr)
	    call flush (STDERR)
	    call erract (EA_WARN)
	}
	call clean_pf (pf, ipf)
	call sfree (sp)
	return (ERR)
    }
    if (ios == 0 && ivs == 1) {
	datatype = VS_DTYPE(vs, ivs)
	ipf = ipf + 1
	PF_ACTION(pf, ipf) = END_OF_EXPRESSION
	PF_DTYPE1(pf, ipf) = datatype
    	call sfree (sp)
	return (datatype)
    } else {
	call eprintf ("Warning: illegal expression (%s): ")
	    call pargstr (expr)
	call eprintf ("bad mix of operators and operands\n")
	call clean_pf (pf, ipf)
    	call sfree (sp)
	return (ERR)
    }
end

# CLEAN_PF -- Clean up a postfix structure, meaning simply dealloc the
# character strings that were allocated in it.

procedure clean_pf (pf, ipf)
pointer	pf	# POSTFIX structure
int	ipf	# Number of elements on postix stack

int	i

begin
    # Deallocate character strings on postfix stack
    do i = 1, ipf
	if (((PF_ACTION(pf,i)==PUSH_CONSTANT ||
	      PF_ACTION(pf,i)==PUSH_IDENTIFIER) && PF_DTYPE1(pf,i)==TY_CHAR) ||
	      PF_ACTION(pf,i)==PRINTF)
	    call mfree (PF_VALP(pf,i), TY_CHAR)
end

# POP_OP -- Pop the top operator off the operator stack, making the
# appropriate additions to the postfix command stack.

procedure pop_op (os, ios, vs, ivs, pf, ipf)
pointer	os
int	ios
pointer	vs
int	ivs
pointer	pf
int	ipf

errchk	same_type

begin
    switch (OS_ID(os, ios)) {
    case LOG, LN, DEXP, EXP, SQRT, NINT, ABS, SIGMA: #Unary-arithmetic opertors
        if (ivs < 1)
	    call error (0, "too few operands")
	if (VS_DTYPE(vs, ivs) == TY_CHAR || VS_DTYPE(vs, ivs) == TY_BOOL)
	    call error (0, "mismatched data type")
	ipf = ipf + 1
	PF_ACTION(pf, ipf) = OS_ID(os, ios)
	PF_DTYPE1(pf, ipf) = VS_DTYPE(vs, ivs)
	# Datatype of result is double, unless input operand was NINT
	if (OS_ID(os, ios) == NINT)
	    VS_DTYPE(vs, ivs) = TY_INT
	else
	    VS_DTYPE(vs, ivs) = TY_DOUBLE
	# If operand is ERR, then turn error computation off
	if (OS_ID(os, ios) == SIGMA) {
	    ipf = ipf + 1
	    PF_ACTION(pf, ipf) = ERRORS_OFF
	}
	ios = ios - 1

    case UNDEFINED: # Unary-arithmetic with boolean result operators
        if (ivs < 1)
	    call error (0, "too few operands")
	if (VS_DTYPE(vs, ivs) == TY_CHAR || VS_DTYPE(vs, ivs) == TY_BOOL)
	    call error (0, "mismatched data type")
	ipf = ipf + 1
	PF_ACTION(pf, ipf) = OS_ID(os, ios)
	PF_DTYPE1(pf, ipf) = VS_DTYPE(vs, ivs)
	VS_DTYPE(vs, ivs) = TY_BOOL
	ios = ios - 1

    case UMINUS: # Unary-minus
        if (ivs < 1)
	    call error (0, "too few operands")
	if (VS_DTYPE(vs, ivs) == TY_CHAR || VS_DTYPE(vs, ivs) == TY_BOOL)
	    call error (0, "mismatched data type")
	ipf = ipf + 1
	PF_ACTION(pf, ipf) = OS_ID(os, ios)
	PF_DTYPE1(pf, ipf) = VS_DTYPE(vs, ivs)
	ios = ios - 1

    case NOT: # Unary-logical operators
        if (ivs < 1)
	    call error (0, "too few operands")
	if (VS_DTYPE(vs, ivs) != TY_BOOL)
	    call error (0, "mismatched data types")
	ipf = ipf + 1
	PF_ACTION(pf, ipf) = OS_ID(os, ios)
	PF_DTYPE1(pf, ipf) = VS_DTYPE(vs, ivs)
	ios = ios - 1

    case EXPONENTIATE, MULTIPLY, DIVIDE, ADD, SUBTRACT, MIN, MAX:
	if (ivs < 2)
	    call error (0, "too few operands")
	if (VS_DTYPE(vs, ivs) == TY_BOOL || VS_DTYPE(vs, ivs) == TY_CHAR)
	    call error (0, "mismatched data type")
	if (VS_DTYPE(vs, ivs-1) == TY_BOOL || VS_DTYPE(vs, ivs-1) == TY_CHAR)
	    call error (0, "mismatched data type")
	call same_type (vs, ivs, pf, ipf) 
	ipf = ipf + 1
	PF_ACTION(pf, ipf) = OS_ID(os, ios)
	PF_DTYPE1(pf, ipf) = VS_DTYPE(vs, ivs)
	PF_DTYPE2(pf, ipf) = VS_DTYPE(vs, ivs-1)
	ivs = ivs - 1
	ios = ios - 1

    case LESSEQUAL, MOREEQUAL, LESSTHAN, MORETHAN:
	if (ivs < 2)
	    call error (0, "too few operands")
	if (VS_DTYPE(vs, ivs) == TY_BOOL || VS_DTYPE(vs, ivs) == TY_CHAR)
	    call error (0, "mismatched data type")
	if (VS_DTYPE(vs, ivs-1) == TY_BOOL || VS_DTYPE(vs, ivs-1) == TY_CHAR)
	    call error (0, "mismatched data type")
	call same_type (vs, ivs, pf, ipf)
	ipf = ipf + 1
	PF_ACTION(pf, ipf) = OS_ID(os, ios)
	PF_DTYPE1(pf, ipf) = VS_DTYPE(vs, ivs)
	PF_DTYPE2(pf, ipf) = VS_DTYPE(vs, ivs-1)
	ivs = ivs - 1
	VS_DTYPE(vs, ivs) = TY_BOOL
	ios = ios - 1

    case EQUAL, NOTEQUAL:
	if (ivs < 2)
	    call error (0, "too few operands")
	if (VS_DTYPE(vs, ivs) == TY_BOOL || VS_DTYPE(vs, ivs-1) == TY_BOOL)
	    call error (0, "mismatched data type")
	if (VS_DTYPE(vs, ivs) == TY_CHAR && VS_DTYPE(vs, ivs-1) != TY_CHAR)
	    call error (0, "mismatched data type")
	call same_type (vs, ivs, pf, ipf)
	ipf = ipf + 1
	PF_ACTION(pf, ipf) = OS_ID(os, ios)
	PF_DTYPE1(pf, ipf) = VS_DTYPE(vs, ivs)
	PF_DTYPE2(pf, ipf) = VS_DTYPE(vs, ivs-1)
	ivs = ivs - 1
	VS_DTYPE(vs, ivs) = TY_BOOL
	ios = ios - 1

    case BOOL_AND, BOOL_OR: # Binary-logical
        if (ivs < 2)
	    call error (0, "too few operands")
	if (VS_DTYPE(vs, ivs) != TY_BOOL || VS_DTYPE(vs, ivs-1) != TY_BOOL)
	    call error (0, "mismatched data types")
	ipf = ipf + 1
	PF_ACTION(pf, ipf) = OS_ID(os, ios)
	PF_DTYPE1(pf, ipf) = VS_DTYPE(vs, ivs)
	PF_DTYPE2(pf, ipf) = VS_DTYPE(vs, ivs-1)
	ivs = ivs - 1
	ios = ios - 1

    case SUBSTRING, NOTSUBSTRING: #Binary-character
        if (ivs < 2)
	    call error (0, "too few operands")
	if (VS_DTYPE(vs, ivs) != TY_CHAR || VS_DTYPE(vs, ivs-1) != TY_CHAR)
	    call error (0, "mismatched data types")
	ipf = ipf + 1
	PF_ACTION(pf, ipf) = OS_ID(os, ios)
	PF_DTYPE1(pf, ipf) = VS_DTYPE(vs, ivs)
	PF_DTYPE2(pf, ipf) = VS_DTYPE(vs, ivs-1)
	ivs = ivs - 1
	VS_DTYPE(vs, ivs) = TY_BOOL
	ios = ios - 1

    case COLON: # Choice operator.
        if (ivs < 3)
	    call error (0, "too few operands")
	if (VS_DTYPE(vs, ivs-2) != TY_BOOL)
	    call error (0, "mismatched data types")
	call same_type (vs, ivs, pf, ipf)
	ipf = ipf + 1
	PF_ACTION(pf, ipf) = OS_ID(os, ios)
	PF_DTYPE1(pf, ipf) = VS_DTYPE(vs, ivs)
	PF_DTYPE2(pf, ipf) = VS_DTYPE(vs, ivs-1)
	ivs = ivs - 2
	VS_DTYPE(vs, ivs) = PF_DTYPE1(pf, ipf)
	ios = ios - 1

    default:
	call error (0, "bad mix of operators")
    }
end

# SAME_TYPE -- Change top two variables on stack to the same type.

procedure same_type (vs, ivs, pf, ipf)

pointer	vs	# Variable stack pointer
int	ivs	# Top of variable stack
pointer pf	# Post-fix stack pointer
int	ipf	# Length of post-fix stack

int	command

begin
	# Error if there aren't two variables on the stack
        if (ivs < 2)
	    call error (0, "too few operands")

	# Nothing done if they're already the same
	if (VS_DTYPE(vs, ivs) == VS_DTYPE(vs, ivs-1))
	    return

	# Since we know they don't match at this point, then an error if
	# one of the variables is boolean or character.
	if (VS_DTYPE(vs, ivs-1) == TY_CHAR || VS_DTYPE(vs, ivs-1) == TY_BOOL ||
	    VS_DTYPE(vs, ivs)   == TY_CHAR || VS_DTYPE(vs, ivs)   == TY_BOOL)
	    call error (0, "mismatched data types")

	# Determine which has least significance
	if (VS_DTYPE(vs, ivs) == TY_DOUBLE && VS_DTYPE(vs, ivs-1) == TY_INT)
	    command = CHTYPE2
	else if (VS_DTYPE(vs, ivs-1) == TY_DOUBLE && VS_DTYPE(vs, ivs)==TY_INT)
	    command = CHTYPE1
	else
	    call error (0, "Illegal data type")

	# Make the change
	ipf = ipf + 1
   	PF_ACTION(pf, ipf) = command
	if (command == CHTYPE1) {
	    PF_DTYPE1(pf, ipf) = VS_DTYPE(vs, ivs)
	    PF_DTYPE2(pf, ipf) = VS_DTYPE(vs, ivs-1)
	    VS_DTYPE(vs, ivs) = VS_DTYPE(vs, ivs-1)
	} else {
	    PF_DTYPE1(pf, ipf) = VS_DTYPE(vs, ivs-1)
	    PF_DTYPE2(pf, ipf) = VS_DTYPE(vs, ivs)
	    VS_DTYPE(vs, ivs-1) = VS_DTYPE(vs, ivs)
	}
end
