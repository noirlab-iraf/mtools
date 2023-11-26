include "pointer.h"
include "database.h"
include "token.h"
include "postfix.h"

define	MISSING_FIELD	false

# EVALUATE -- Do the actual evaluation.  The ouput array must be allocated
# before calling this procedure.

define	STEP	2	# Size of vs stack element
define	VSI	Memi[$1]	
define	VSD	Memd[P2D($1)]	
define	VSB	Memb[$1]
define	VSP	Memp[$1]	

define	ESTEP	1
define	VERR2	Memr[P2P($1)]

define	LN10	2.302585

procedure evaluate (pf, ipf, db, index, output, eoutput, datatype, dtype)
pointer	pf		# Postfix stack pointer
int	ipf		# Number of elements in postfix stack
pointer	db		# DATABASE pointer
int	index[ARB]	# Good element index
pointer	output		# Output results
pointer	eoutput		# Error output results
int	datatype	# Desired datatype for ouput
int	dtype		# Resulting datatype of postfix stack

real	esave, dbgerr2()
int	i, j, k, strmatch(), len, isave, nerrors
double	dsave
bool	streq(), strne(), errors
pointer	sp, vs, ivs, es, ies

short	dbgets()

int	dbgeti()

long	dbgetl()

real	dbgetr()

double	dbgetd()

bool	dbgetb()


begin
    # Allocate operand stack, and tempory storage
    call smark (sp)
    call salloc (vs, MAXDEPTH*STEP, TY_STRUCT)
    call salloc (es, MAXDEPTH, TY_REAL)

    # Evaluate the expression
    nerrors = 0
    errors = false
    for (k = 1; index[k] > 0; k = k + 1) {
	i = index[k]
	# Cheating here a bit, to speed the code a little.  Rather than
	# referring to variable stack elements as Mem$t[vs+ivs-STEP], we'll
	# initialize ivs = vs - STEP (rather than ivs = 0), and just refer
	# to elements as Mem$t[ivs].
	ivs = vs - STEP	    # Rather than ivs = 0
	ies = es - ESTEP    # Rather than ies = 0
	do j = 1, ipf {
	    switch (PF_ACTION(pf, j)) {
	    case END_OF_EXPRESSION:
		if (ivs == vs - STEP) # Stack empty --must have printed result
		    next
	    	switch (datatype) {

		case TY_REAL:
		    switch (dtype) {
		    case TY_INT:
			if (IS_INDEFI(VSI(ivs)))
			    Memr[output+k-1] = INDEFR
			else
			    Memr[output+k-1] = VSI(ivs)
		    case TY_DOUBLE:
			if (IS_INDEFD(VSD(ivs)))
			    Memr[output+k-1] = INDEFR
			else
			    Memr[output+k-1] = VSD(ivs)
		    }

		case TY_INT:
		    switch (dtype) {
		    case TY_INT:
			if (IS_INDEFI(VSI(ivs)))
			    Memi[output+k-1] = INDEFI
			else
			    Memi[output+k-1] = VSI(ivs)
		    case TY_DOUBLE:
			if (IS_INDEFD(VSD(ivs)))
			    Memi[output+k-1] = INDEFI
			else
			    Memi[output+k-1] = VSD(ivs)
		    }

		case TY_DOUBLE:
		    switch (dtype) {
		    case TY_INT:
			if (IS_INDEFI(VSI(ivs)))
			    Memd[output+k-1] = INDEFD
			else
			    Memd[output+k-1] = VSI(ivs)
		    case TY_DOUBLE:
			if (IS_INDEFD(VSD(ivs)))
			    Memd[output+k-1] = INDEFD
			else
			    Memd[output+k-1] = VSD(ivs)
		    }

		case TY_SHORT:
		    switch (dtype) {
		    case TY_INT:
			if (IS_INDEFI(VSI(ivs)))
			    Mems[output+k-1] = INDEFS
			else
			    Mems[output+k-1] = VSI(ivs)
		    case TY_DOUBLE:
			if (IS_INDEFD(VSD(ivs)))
			    Mems[output+k-1] = INDEFS
			else
			    Mems[output+k-1] = VSD(ivs)
		    }

		case TY_LONG:
		    switch (dtype) {
		    case TY_INT:
			if (IS_INDEFI(VSI(ivs)))
			    Meml[output+k-1] = INDEFL
			else
			    Meml[output+k-1] = VSI(ivs)
		    case TY_DOUBLE:
			if (IS_INDEFD(VSD(ivs)))
			    Meml[output+k-1] = INDEFL
			else
			    Meml[output+k-1] = VSD(ivs)
		    }

		case TY_BOOL:
	    	    Memb[output+k-1] = VSB(ivs)
		case TY_CHAR:
	    	    Memp[output+k-1] = VSP(ivs)
		}
		if (errors) {
		    if (IS_INDEFR(VERR2(ies)))
			Memr[eoutput+k-1] = INDEFR
		    else
		    	Memr[eoutput+k-1] = sqrt(VERR2(ies))
		}
	    case PRINTF:
		call fprintf (PF_FD(pf, j), Memc[PF_VALP(pf, j)])
		switch (PF_DTYPE1(pf, j)) {

		case TY_INT:
		    call pargi (VSI(ivs))

		case TY_DOUBLE:
		    call pargd (VSD(ivs))

		case TY_BOOL:
		    call pargb (VSB(ivs))

		case TY_CHAR:
		    call pargstr (Memc[VSP(ivs)])
		}
		if (errors) {
		    if (IS_INDEFR(VERR2(ies)))
		    	call pargr (INDEFR)
		    else
		    	call pargr (sqrt(VERR2(ies)))
		}
		ivs = ivs - STEP
		ies = ies - ESTEP
	    case ERRORS_ON:
		nerrors = nerrors + 1
		if (nerrors > 0)
		    errors = true
		else
		    errors = false
	    case ERRORS_OFF:
		nerrors = nerrors - 1
		if (nerrors > 0)
		    errors = true
		else
		    errors = false
	    case COLON:
		if (VSB(ivs-2*STEP)) {
		    switch (PF_DTYPE1(pf, j)) {

		    case TY_INT:
			VSI(ivs-2*STEP) = VSI(ivs-STEP)

		    case TY_DOUBLE:
			VSD(ivs-2*STEP) = VSD(ivs-STEP)

		    case TY_BOOL:
			VSB(ivs-2*STEP) = VSB(ivs-STEP)

		    case TY_CHAR:
			VSP(ivs-2*STEP) = VSP(ivs-STEP)
		    }
		    if (errors)
			VERR2(ies-2*ESTEP) = VERR2(ies-ESTEP)
		} else {
		    switch (PF_DTYPE1(pf, j)) {

		    case TY_INT:
			VSI(ivs-2*STEP) = VSI(ivs)

		    case TY_DOUBLE:
			VSD(ivs-2*STEP) = VSD(ivs)

		    case TY_BOOL:
			VSB(ivs-2*STEP) = VSB(ivs)

		    case TY_CHAR:
			VSP(ivs-2*STEP) = VSP(ivs)
		    }
		    if (errors)
			VERR2(ies-2*STEP) = VERR2(ies)
		}
		ivs = ivs - 2*STEP
		ies = ies - 2*ESTEP
	    case SEQUENCE: # Print the sequence number
		ivs = ivs + STEP
		ies = ies + ESTEP   
		VSI(ivs) = k
		if (errors)
		    VERR2(ies) = 0.
	    case EXPONENTIATE:
	    	switch (PF_DTYPE1(pf, j)) {

		case TY_INT:
		    if (errors) {
		    	if (IS_INDEFR(VERR2(ies-ESTEP))||IS_INDEFR(VERR2(ies)))
			    VERR2(ies-ESTEP) = INDEFR
		    	else {
			    VERR2(ies-ESTEP) = (VSI(ivs) * VSI(ivs-STEP) **
			      (VSI(ivs) - 1)) ** 2. * VERR2(ies-ESTEP)
			    if (VERR2(ies) != 0.)
			        VERR2(ies-ESTEP) = VERR2(ies-ESTEP) +
				        (VSI(ivs-STEP) ** VSI(ivs) *
			        log(double(VSI(ivs-STEP)))) ** 2. * VERR2(ies)
			}
		    }
		    if (IS_INDEFI(VSI(ivs-STEP)) || IS_INDEFI(VSI(ivs)))
			VSI(ivs-STEP) = INDEFI
		    else
		        VSI(ivs-STEP) = VSI(ivs-STEP) ** VSI(ivs)

		case TY_DOUBLE:
		    if (errors) {
		    	if (IS_INDEFR(VERR2(ies-ESTEP))||IS_INDEFR(VERR2(ies)))
			    VERR2(ies-ESTEP) = INDEFR
		    	else {
			    VERR2(ies-ESTEP) = (VSD(ivs) * VSD(ivs-STEP) **
			      (VSD(ivs) - 1)) ** 2. * VERR2(ies-ESTEP)
			    if (VERR2(ies) != 0.)
			        VERR2(ies-ESTEP) = VERR2(ies-ESTEP) +
				        (VSD(ivs-STEP) ** VSD(ivs) *
			        log(double(VSD(ivs-STEP)))) ** 2. * VERR2(ies)
			}
		    }
		    if (IS_INDEFD(VSD(ivs-STEP)) || IS_INDEFD(VSD(ivs)))
			VSD(ivs-STEP) = INDEFD
		    else
		        VSD(ivs-STEP) = VSD(ivs-STEP) ** VSD(ivs)

		}
		ivs = ivs - STEP
		ies = ies - ESTEP
	    case MULTIPLY:
	    	switch (PF_DTYPE1(pf, j)) {

		case TY_INT:
		    if (errors) {
		    	if (IS_INDEFR(VERR2(ies-ESTEP))||IS_INDEFR(VERR2(ies)))
			    VERR2(ies-ESTEP) = INDEFR
		    	else
			    VERR2(ies-ESTEP) = VERR2(ies-ESTEP) /
			      (VSI(ivs-STEP) * VSI(ivs-STEP)) +
			      VERR2(ies) / (VSI(ivs) * VSI(ivs))
		    }
		    if (IS_INDEFI(VSI(ivs-STEP)) || IS_INDEFI(VSI(ivs)))
			VSI(ivs-STEP) = INDEFI
		    else
		    	VSI(ivs-STEP) = VSI(ivs-STEP) * VSI(ivs)
		    if (errors)
			if (! IS_INDEFR(VERR2(ies-ESTEP)))
			    VERR2(ies-ESTEP) = VERR2(ies-ESTEP) *
				VSI(ivs-STEP) * VSI(ivs-STEP)

		case TY_DOUBLE:
		    if (errors) {
		    	if (IS_INDEFR(VERR2(ies-ESTEP))||IS_INDEFR(VERR2(ies)))
			    VERR2(ies-ESTEP) = INDEFR
		    	else
			    VERR2(ies-ESTEP) = VERR2(ies-ESTEP) /
			      (VSD(ivs-STEP) * VSD(ivs-STEP)) +
			      VERR2(ies) / (VSD(ivs) * VSD(ivs))
		    }
		    if (IS_INDEFD(VSD(ivs-STEP)) || IS_INDEFD(VSD(ivs)))
			VSD(ivs-STEP) = INDEFD
		    else
		    	VSD(ivs-STEP) = VSD(ivs-STEP) * VSD(ivs)
		    if (errors)
			if (! IS_INDEFR(VERR2(ies-ESTEP)))
			    VERR2(ies-ESTEP) = VERR2(ies-ESTEP) *
				VSD(ivs-STEP) * VSD(ivs-STEP)

		}
		ivs = ivs - STEP
		ies = ies - ESTEP
	    case DIVIDE:
	    	switch (PF_DTYPE1(pf, j)) {

		case TY_INT:
		    if (errors) {
		    	if (IS_INDEFR(VERR2(ies-ESTEP))||IS_INDEFR(VERR2(ies)))
			    VERR2(ies-ESTEP) = INDEFR
		    	else
			    VERR2(ies-ESTEP) = VERR2(ies-ESTEP) /
			      (VSI(ivs-STEP) * VSI(ivs-STEP)) +
			      VERR2(ies) / (VSI(ivs) * VSI(ivs))
		    }
		    if (IS_INDEFI(VSI(ivs-STEP)) || IS_INDEFI(VSI(ivs)))
			VSI(ivs-STEP) = INDEFI
		    else
		    	VSI(ivs-STEP) = VSI(ivs-STEP) / VSI(ivs)
		    if (errors)
			if (! IS_INDEFR(VERR2(ies-ESTEP)))
			    VERR2(ies-ESTEP) = VERR2(ies-ESTEP) *
				VSI(ivs-STEP) * VSI(ivs-STEP)

		case TY_DOUBLE:
		    if (errors) {
		    	if (IS_INDEFR(VERR2(ies-ESTEP))||IS_INDEFR(VERR2(ies)))
			    VERR2(ies-ESTEP) = INDEFR
		    	else
			    VERR2(ies-ESTEP) = VERR2(ies-ESTEP) /
			      (VSD(ivs-STEP) * VSD(ivs-STEP)) +
			      VERR2(ies) / (VSD(ivs) * VSD(ivs))
		    }
		    if (IS_INDEFD(VSD(ivs-STEP)) || IS_INDEFD(VSD(ivs)))
			VSD(ivs-STEP) = INDEFD
		    else
		    	VSD(ivs-STEP) = VSD(ivs-STEP) / VSD(ivs)
		    if (errors)
			if (! IS_INDEFR(VERR2(ies-ESTEP)))
			    VERR2(ies-ESTEP) = VERR2(ies-ESTEP) *
				VSD(ivs-STEP) * VSD(ivs-STEP)

		}
		ivs = ivs - STEP
		ies = ies - ESTEP
	    case ADD:
	    	switch (PF_DTYPE1(pf, j)) {

		case TY_INT:
		    if (errors) {
			if (IS_INDEFR(VERR2(ies-ESTEP))||IS_INDEFR(VERR2(ies)))
			    VERR2(ies-ESTEP) = INDEFR
			else
			    VERR2(ies-ESTEP) = VERR2(ies-ESTEP) + VERR2(ies)
		    }
		    if (IS_INDEFI(VSI(ivs-STEP)) || IS_INDEFI(VSI(ivs)))
			VSI(ivs-STEP) = INDEFI
		    else
		    	VSI(ivs-STEP) = VSI(ivs-STEP) + VSI(ivs)

		case TY_DOUBLE:
		    if (errors) {
			if (IS_INDEFR(VERR2(ies-ESTEP))||IS_INDEFR(VERR2(ies)))
			    VERR2(ies-ESTEP) = INDEFR
			else
			    VERR2(ies-ESTEP) = VERR2(ies-ESTEP) + VERR2(ies)
		    }
		    if (IS_INDEFD(VSD(ivs-STEP)) || IS_INDEFD(VSD(ivs)))
			VSD(ivs-STEP) = INDEFD
		    else
		    	VSD(ivs-STEP) = VSD(ivs-STEP) + VSD(ivs)

		}
		ivs = ivs - STEP
		ies = ies - ESTEP
	    case SUBTRACT:
	    	switch (PF_DTYPE1(pf, j)) {

		case TY_INT:
		    if (errors) {
			if (IS_INDEFR(VERR2(ies-ESTEP))||IS_INDEFR(VERR2(ies)))
			    VERR2(ies-ESTEP) = INDEFR
			else
			    VERR2(ies-ESTEP) = VERR2(ies-ESTEP) + VERR2(ies)
		    }
		    if (IS_INDEFI(VSI(ivs-STEP)) || IS_INDEFI(VSI(ivs)))
			VSI(ivs-STEP) = INDEFI
		    else
		    	VSI(ivs-STEP) = VSI(ivs-STEP) - VSI(ivs)

		case TY_DOUBLE:
		    if (errors) {
			if (IS_INDEFR(VERR2(ies-ESTEP))||IS_INDEFR(VERR2(ies)))
			    VERR2(ies-ESTEP) = INDEFR
			else
			    VERR2(ies-ESTEP) = VERR2(ies-ESTEP) + VERR2(ies)
		    }
		    if (IS_INDEFD(VSD(ivs-STEP)) || IS_INDEFD(VSD(ivs)))
			VSD(ivs-STEP) = INDEFD
		    else
		    	VSD(ivs-STEP) = VSD(ivs-STEP) - VSD(ivs)

		}
		ivs = ivs - STEP
		ies = ies - ESTEP
	    case LESSEQUAL:
	    	switch (PF_DTYPE1(pf, j)) {

		case TY_INT:
		    if (IS_INDEFI(VSI(ivs-STEP)) || IS_INDEFI(VSI(ivs)))
			VSB(ivs-STEP) = MISSING_FIELD
		    else
		    	VSB(ivs-STEP) = VSI(ivs-STEP) <= VSI(ivs)

		case TY_DOUBLE:
		    if (IS_INDEFD(VSD(ivs-STEP)) || IS_INDEFD(VSD(ivs)))
			VSB(ivs-STEP) = MISSING_FIELD
		    else
		    	VSB(ivs-STEP) = VSD(ivs-STEP) <= VSD(ivs)

		}
		ivs = ivs - STEP
		ies = ies - ESTEP
	    case MOREEQUAL:
	    	switch (PF_DTYPE1(pf, j)) {

		case TY_INT:
		    if (IS_INDEFI(VSI(ivs-STEP)) || IS_INDEFI(VSI(ivs)))
			VSB(ivs-STEP) = MISSING_FIELD
		    else
		    	VSB(ivs-STEP) = VSI(ivs-STEP) >= VSI(ivs)

		case TY_DOUBLE:
		    if (IS_INDEFD(VSD(ivs-STEP)) || IS_INDEFD(VSD(ivs)))
			VSB(ivs-STEP) = MISSING_FIELD
		    else
		    	VSB(ivs-STEP) = VSD(ivs-STEP) >= VSD(ivs)

		}
		ivs = ivs - STEP
		ies = ies - ESTEP
	    case LESSTHAN:
	    	switch (PF_DTYPE1(pf, j)) {

		case TY_INT:
		    if (IS_INDEFI(VSI(ivs-STEP)) || IS_INDEFI(VSI(ivs)))
			VSB(ivs-STEP) = MISSING_FIELD
		    else
		    	VSB(ivs-STEP) = VSI(ivs-STEP) < VSI(ivs)

		case TY_DOUBLE:
		    if (IS_INDEFD(VSD(ivs-STEP)) || IS_INDEFD(VSD(ivs)))
			VSB(ivs-STEP) = MISSING_FIELD
		    else
		    	VSB(ivs-STEP) = VSD(ivs-STEP) < VSD(ivs)

		}
		ivs = ivs - STEP
		ies = ies - ESTEP
	    case MORETHAN:
	    	switch (PF_DTYPE1(pf, j)) {

		case TY_INT:
		    if (IS_INDEFI(VSI(ivs-STEP)) || IS_INDEFI(VSI(ivs)))
			VSB(ivs-STEP) = MISSING_FIELD
		    else
		    	VSB(ivs-STEP) = VSI(ivs-STEP) > VSI(ivs)

		case TY_DOUBLE:
		    if (IS_INDEFD(VSD(ivs-STEP)) || IS_INDEFD(VSD(ivs)))
			VSB(ivs-STEP) = MISSING_FIELD
		    else
		    	VSB(ivs-STEP) = VSD(ivs-STEP) > VSD(ivs)

		}
		ivs = ivs - STEP
		ies = ies - ESTEP
	    case EQUAL:
	    	switch (PF_DTYPE1(pf, j)) {

		case TY_INT:
		    if (IS_INDEFI(VSI(ivs-STEP)) || IS_INDEFI(VSI(ivs)))
			VSB(ivs-STEP) = MISSING_FIELD
		    else
		    	VSB(ivs-STEP) = VSI(ivs-STEP) == VSI(ivs)

		case TY_DOUBLE:
		    if (IS_INDEFD(VSD(ivs-STEP)) || IS_INDEFD(VSD(ivs)))
			VSB(ivs-STEP) = MISSING_FIELD
		    else
		    	VSB(ivs-STEP) = VSD(ivs-STEP) == VSD(ivs)

		case TY_CHAR:
		    VSB(ivs-STEP) = streq (Memc[VSP(ivs-STEP)], Memc[VSP(ivs)])
		}
		ivs = ivs - STEP
		ies = ies - ESTEP
	    case NOTEQUAL:
	    	switch (PF_DTYPE1(pf, j)) {

		case TY_INT:
		    if (IS_INDEFI(VSI(ivs-STEP)) || IS_INDEFI(VSI(ivs)))
			VSB(ivs-STEP) = MISSING_FIELD
		    else
		    	VSB(ivs-STEP) = VSI(ivs-STEP) != VSI(ivs)

		case TY_DOUBLE:
		    if (IS_INDEFD(VSD(ivs-STEP)) || IS_INDEFD(VSD(ivs)))
			VSB(ivs-STEP) = MISSING_FIELD
		    else
		    	VSB(ivs-STEP) = VSD(ivs-STEP) != VSD(ivs)

		case TY_CHAR:
		    VSB(ivs-STEP) = strne (Memc[VSP(ivs-STEP)], Memc[VSP(ivs)])
		}
		ivs = ivs - STEP
		ies = ies - ESTEP
	    case SUBSTRING:
		VSB(ivs-STEP) = strmatch (Memc[VSP(ivs-STEP)], Memc[VSP(ivs)]) > 0
		ivs = ivs - STEP
		ies = ies - ESTEP
	    case NOTSUBSTRING:
		VSB(ivs-STEP) = strmatch (Memc[VSP(ivs-STEP)], Memc[VSP(ivs)]) ==0
		ivs = ivs - STEP
		ies = ies - ESTEP
	    case BOOL_OR:
		VSB(ivs-STEP) = VSB(ivs-STEP) || VSB(ivs)
		ivs = ivs - STEP
		ies = ies - ESTEP
	    case BOOL_AND:
		VSB(ivs-STEP) = VSB(ivs-STEP) && VSB(ivs)
		ivs = ivs - STEP
		ies = ies - ESTEP
	    case NOT:
		VSB(ivs) = ! VSB(ivs)
	    case UMINUS:
		switch (PF_DTYPE1(pf, j)) {

		case TY_INT:
		    if (IS_INDEFI(VSI(ivs)))
			VSI(ivs) = INDEFI
		    else
		    	VSI(ivs) = - VSI(ivs)

		case TY_DOUBLE:
		    if (IS_INDEFD(VSD(ivs)))
			VSD(ivs) = INDEFD
		    else
		    	VSD(ivs) = - VSD(ivs)

		}
	    case LOG:
		switch (PF_DTYPE1(pf, j)) {
		case TY_DOUBLE:
		    if (errors)
			if (! IS_INDEFR(VERR2(ies)))
			    VERR2(ies) = VERR2(ies) / (VSD(ivs) * VSD(ivs) *
					 LN10 * LN10)
		    if (IS_INDEFD(VSD(ivs)))
			VSD(ivs) = INDEFD
		    else
		    	VSD(ivs) = log10 (VSD(ivs))
		case TY_INT:
		    if (errors)
			if (! IS_INDEFR(VERR2(ies)))
			    VERR2(ies) = VERR2(ies) / (VSI(ivs) * VSI(ivs) *
					 LN10 * LN10)
		    if (IS_INDEFI(VSI(ivs)))
			VSD(ivs) = INDEFR
		    else
		        VSD(ivs) = log10 (double(VSI(ivs)))
		}
	    case LN:
		switch (PF_DTYPE1(pf, j)) {
		case TY_DOUBLE:
		    if (errors)
			if (! IS_INDEFR(VERR2(ies)))
			    VERR2(ies) = VERR2(ies) / (VSD(ivs) * VSD(ivs))
		    if (IS_INDEFD(VSD(ivs)))
			VSD(ivs) = INDEFD
		    else
		    	VSD(ivs) = log (VSD(ivs))
		case TY_INT:
		    if (errors)
			if (! IS_INDEFR(VERR2(ies)))
			    VERR2(ies) = VERR2(ies) / (VSI(ivs) * VSI(ivs))
		    if (IS_INDEFI(VSI(ivs)))
			VSD(ivs) = INDEFR
		    else
		        VSD(ivs) = log (double(VSI(ivs)))
		}
	    case DEXP:
		switch (PF_DTYPE1(pf, j)) {
		case TY_DOUBLE:
		    if (IS_INDEFD(VSD(ivs)))
			VSD(ivs) = INDEFD
		    else
		    	VSD(ivs) = 10. ** VSD(ivs)
		    if (errors)
			if (! IS_INDEFR(VERR2(ies)))
			    VERR2(ies) = VERR2(ies)*LN10*LN10*VSD(ivs)*VSD(ivs)
		case TY_INT:
		    if (IS_INDEFI(VSI(ivs)))
			VSD(ivs) = INDEFR
		    else
		        VSD(ivs) = 10. ** VSI(ivs)
		    if (errors)
			if (! IS_INDEFR(VERR2(ies)))
			    VERR2(ies) = VERR2(ies)*LN10*LN10*VSI(ivs)*VSI(ivs)
		}
	    case EXP:
		switch (PF_DTYPE1(pf, j)) {
		case TY_DOUBLE:
		    if (IS_INDEFD(VSD(ivs)))
			VSD(ivs) = INDEFD
		    else
		    	VSD(ivs) = exp (VSD(ivs))
		    if (errors)
			if (! IS_INDEFR(VERR2(ies)))
			    VERR2(ies) = VERR2(ies)*VSD(ivs)*VSD(ivs)
		case TY_INT:
		    if (IS_INDEFI(VSI(ivs)))
			VSD(ivs) = INDEFR
		    else
		        VSD(ivs) = exp (double(VSI(ivs)))
		    if (errors)
			if (! IS_INDEFR(VERR2(ies)))
			    VERR2(ies) = VERR2(ies)*VSI(ivs)*VSI(ivs)
		}
	    case SQRT:
		switch (PF_DTYPE1(pf, j)) {
		case TY_DOUBLE:
		    if (errors)
			if (! IS_INDEFR(VERR2(ies)))
			    VERR2(ies) = VERR2(ies) / (4 * VSD(ivs))
		    if (IS_INDEFD(VSD(ivs)))
			VSD(ivs) = INDEFD
		    else
		    	VSD(ivs) = sqrt (VSD(ivs))
		case TY_INT:
		    if (errors)
			if (! IS_INDEFR(VERR2(ies)))
			    VERR2(ies) = VERR2(ies) / (4 * VSI(ivs))
		    if (IS_INDEFI(VSI(ivs)))
			VSD(ivs) = INDEFR
		    else
		        VSD(ivs) = sqrt (double(VSI(ivs)))
		}
	    case ABS:
		switch (PF_DTYPE1(pf, j)) {
		case TY_DOUBLE:
		    if (IS_INDEFD(VSD(ivs)))
			VSD(ivs) = INDEFD
		    else
		    	VSD(ivs) = abs (VSD(ivs))
		case TY_INT:
		    if (IS_INDEFI(VSI(ivs)))
			VSD(ivs) = INDEFR
		    else
		        VSD(ivs) = abs (double(VSI(ivs)))
		}
	    case SIGMA:
		if (IS_INDEFR(VERR2(ies)))
		    VSD(ivs) = INDEFD
		else
		    VSD(ivs) = sqrt(VERR2(ies))
		VERR2(ies) = INDEFR
	    case NINT:
		switch (PF_DTYPE1(pf, j)) {
		case TY_DOUBLE:
		    if (IS_INDEFD(VSD(ivs)))
			VSI(ivs) = INDEFI
		    else
		    	VSI(ivs) = nint (VSD(ivs))
		case TY_INT:
		}
	    case MIN:
	    	switch (PF_DTYPE1(pf, j)) {

		case TY_INT:
		    if (errors) {
			if (IS_INDEFR(VERR2(ies-ESTEP))||IS_INDEFR(VERR2(ies)))
			    VERR2(ies-ESTEP) = INDEFR
			else {
			    if (VSI(ivs) < VSI(ivs-STEP))
				VERR2(ies-ESTEP) = VERR2(ies)
			    else
				VERR2(ies-ESTEP) = VERR2(ies-ESTEP)
			}
		    }
		    if (IS_INDEFI(VSI(ivs-STEP)) || IS_INDEFI(VSI(ivs)))
			VSI(ivs-STEP) = INDEFI
		    else
		    	VSI(ivs-STEP) = min (VSI(ivs-STEP), VSI(ivs))

		case TY_DOUBLE:
		    if (errors) {
			if (IS_INDEFR(VERR2(ies-ESTEP))||IS_INDEFR(VERR2(ies)))
			    VERR2(ies-ESTEP) = INDEFR
			else {
			    if (VSD(ivs) < VSD(ivs-STEP))
				VERR2(ies-ESTEP) = VERR2(ies)
			    else
				VERR2(ies-ESTEP) = VERR2(ies-ESTEP)
			}
		    }
		    if (IS_INDEFD(VSD(ivs-STEP)) || IS_INDEFD(VSD(ivs)))
			VSD(ivs-STEP) = INDEFD
		    else
		    	VSD(ivs-STEP) = min (VSD(ivs-STEP), VSD(ivs))

		}
		ivs = ivs - STEP
		ies = ies - ESTEP
	    case MAX:
	    	switch (PF_DTYPE1(pf, j)) {

		case TY_INT:
		    if (errors) {
			if (IS_INDEFR(VERR2(ies-ESTEP))||IS_INDEFR(VERR2(ies)))
			    VERR2(ies-ESTEP) = INDEFR
			else {
			    if (VSI(ivs) > VSI(ivs-STEP))
				VERR2(ies-ESTEP) = VERR2(ies)
			    else
				VERR2(ies-ESTEP) = VERR2(ies-ESTEP)
			}
		    }
		    if (IS_INDEFI(VSI(ivs-STEP)) || IS_INDEFI(VSI(ivs)))
			VSI(ivs-STEP) = INDEFI
		    else
		    	VSI(ivs-STEP) = max (VSI(ivs-STEP), VSI(ivs))

		case TY_DOUBLE:
		    if (errors) {
			if (IS_INDEFR(VERR2(ies-ESTEP))||IS_INDEFR(VERR2(ies)))
			    VERR2(ies-ESTEP) = INDEFR
			else {
			    if (VSD(ivs) > VSD(ivs-STEP))
				VERR2(ies-ESTEP) = VERR2(ies)
			    else
				VERR2(ies-ESTEP) = VERR2(ies-ESTEP)
			}
		    }
		    if (IS_INDEFD(VSD(ivs-STEP)) || IS_INDEFD(VSD(ivs)))
			VSD(ivs-STEP) = INDEFD
		    else
		    	VSD(ivs-STEP) = max (VSD(ivs-STEP), VSD(ivs))

		}
		ivs = ivs - STEP
		ies = ies - ESTEP
	    case UNDEFINED:
		switch (PF_DTYPE1(pf, j)) {

		case TY_INT:
		    VSB(ivs) = IS_INDEFI(VSI(ivs))

		case TY_DOUBLE:
		    VSB(ivs) = IS_INDEFD(VSD(ivs))

		}
	    case PUSH_IDENTIFIER:
		ivs = ivs + STEP
		ies = ies + ESTEP
		switch (PF_DTYPE1(pf, j)) {

		case TY_SHORT:
		    if (IS_INDEFS(dbgets (db, i, PF_ID(pf, j))))
			VSI(ivs) = INDEFI
		    else
		    	VSI(ivs) = dbgets (db, i, PF_ID(pf, j))
		    if (errors)
			VERR2(ies) = dbgerr2 (db, i, PF_ID(pf, j))

		case TY_INT:
		    if (IS_INDEFI(dbgeti (db, i, PF_ID(pf, j))))
			VSI(ivs) = INDEFI
		    else
		    	VSI(ivs) = dbgeti (db, i, PF_ID(pf, j))
		    if (errors)
			VERR2(ies) = dbgerr2 (db, i, PF_ID(pf, j))

		case TY_LONG:
		    if (IS_INDEFL(dbgetl (db, i, PF_ID(pf, j))))
			VSI(ivs) = INDEFI
		    else
		    	VSI(ivs) = dbgetl (db, i, PF_ID(pf, j))
		    if (errors)
			VERR2(ies) = dbgerr2 (db, i, PF_ID(pf, j))


		case TY_REAL:
		    if (IS_INDEFR(dbgetr (db, i, PF_ID(pf, j))))
			VSD(ivs) = INDEFD
		    else
		    	VSD(ivs) = dbgetr (db, i, PF_ID(pf, j))
		    if (errors)
			VERR2(ies) = dbgerr2 (db, i, PF_ID(pf, j))

		case TY_DOUBLE:
		    if (IS_INDEFD(dbgetd (db, i, PF_ID(pf, j))))
			VSD(ivs) = INDEFD
		    else
		    	VSD(ivs) = dbgetd (db, i, PF_ID(pf, j))
		    if (errors)
			VERR2(ies) = dbgerr2 (db, i, PF_ID(pf, j))

		case TY_BOOL:
		    VSB(ivs) = dbgetb (db, i, PF_ID(pf, j))
		case TY_CHAR:
		    len = DB_SIZE(db, PF_ID(pf, j))-1
		    call dbgstr (db, i, PF_ID(pf, j), Memc[PF_VALP(pf, j)],len)
		    VSP(ivs) = PF_VALP(pf, j)
		}
	    case PUSH_CONSTANT:
		ivs = ivs + STEP
		ies = ies + ESTEP
		switch (PF_DTYPE1(pf, j)) {

		case TY_INT:
		    VSI(ivs) = PF_VALI(pf, j)
		    if (errors)
		    	VERR2(ies) = 0.

		case TY_DOUBLE:
		    VSD(ivs) = PF_VALD(pf, j)
		    if (errors)
		    	VERR2(ies) = 0.

		case TY_CHAR:
		    VSP(ivs) = PF_VALP(pf, j)
		}
	    case STORE_TOP:
		switch (PF_DTYPE1(pf, j)) {
		case TY_INT:
		    isave = VSI(ivs)
		case TY_DOUBLE:
		    dsave = VSD(ivs)
		}
		if (errors)
		    esave = VERR2(ies)
	    case RECALL_TOP:
		ivs = ivs + STEP
		ies = ies + ESTEP
		switch (PF_DTYPE1(pf, j)) {
		case TY_INT:
		    VSI(ivs) = isave
		case TY_DOUBLE:
		    VSD(ivs) = dsave
		}
		if (errors)
		    VERR2(ies) = esave
	    case CHTYPE1:
		switch (PF_DTYPE1(pf, j)) {

		case TY_INT:
		    switch (PF_DTYPE2(pf, j)) {
		    case TY_INT:
			if (IS_INDEFI(VSI(ivs)))
			    VSI(ivs) = INDEFI
			else
			    VSI(ivs) = VSI(ivs)
		    case TY_DOUBLE:
			if (IS_INDEFI(VSI(ivs)))
			    VSD(ivs) = INDEFD
			else
			    VSD(ivs) = VSI(ivs)
		    }

		case TY_DOUBLE:
		    switch (PF_DTYPE2(pf, j)) {
		    case TY_INT:
			if (IS_INDEFD(VSD(ivs)))
			    VSI(ivs) = INDEFI
			else
			    VSI(ivs) = VSD(ivs)
		    case TY_DOUBLE:
			if (IS_INDEFD(VSD(ivs)))
			    VSD(ivs) = INDEFD
			else
			    VSD(ivs) = VSD(ivs)
		    }

		}
	    case CHTYPE2:
		switch (PF_DTYPE1(pf, j)) {

		case TY_INT:
		    switch (PF_DTYPE2(pf, j)) {
		    case TY_INT:
			if (IS_INDEFI(VSI(ivs-STEP)))
			    VSI(ivs-STEP) = INDEFI
			else
			    VSI(ivs-STEP) = VSI(ivs-STEP)
		    case TY_DOUBLE:
			if (IS_INDEFI(VSI(ivs-STEP)))
			    VSD(ivs-STEP) = INDEFD
			else
			    VSD(ivs-STEP) = VSI(ivs-STEP)
		    }

		case TY_DOUBLE:
		    switch (PF_DTYPE2(pf, j)) {
		    case TY_INT:
			if (IS_INDEFD(VSD(ivs-STEP)))
			    VSI(ivs-STEP) = INDEFI
			else
			    VSI(ivs-STEP) = VSD(ivs-STEP)
		    case TY_DOUBLE:
			if (IS_INDEFD(VSD(ivs-STEP)))
			    VSD(ivs-STEP) = INDEFD
			else
			    VSD(ivs-STEP) = VSD(ivs-STEP)
		    }

		}
	    }
	}
    }
    call sfree (sp)
end
