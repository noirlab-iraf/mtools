include <gset.h>
include <ctype.h>
include <error.h>
include <mach.h>
include "pointer.h"
include "database.h"

# REPLACE -- Replace database entries from a file.  "Inline" is a wordlist,
# whose first word is the replacement file, and subsequent words are the
# variables to replace, in the order they appear in the file (don't specify
# the serial number name).  Each line in the replacement file should contain
# the serial number of the object to replace, followed by the replacement
# values, all separated by white space.  Returns the number of database entries
# that had values replaced (just to know whether or not to redraw the graph).

define	MAX_VARIABLES	20	# The maximum number of replacement variables

int procedure replace (gp, db, index, inline)
pointer	gp		# GIO pointer
pointer	db		# DATABASE pointer
int	index[ARB]	# Selected elements index
char	inline[ARB]	# Input string

pointer	sp, filename, field, buffer, format
pointer	rp[MAX_VARIABLES], erp[MAX_VARIABLES]
int	rplace[MAX_VARIABLES], nscan(), idx_field(), ip, missing
int	ikey, nvar, k, garg_key(), fd, getline(), open()
int	replaced, dbkey(), nerr

begin
    call smark (sp)
    call salloc (filename, SZ_FNAME, TY_CHAR)
    call salloc (field, SZ_DBNAME, TY_CHAR)
    call salloc (buffer, SZ_LINE, TY_CHAR)
    call salloc (format, SZ_LINE, TY_CHAR)

    # Read input file name and variable names to replace
    nvar = 0
    call sscan (inline)
    call gargwrd (Memc[filename], SZ_FNAME)
    call gargwrd (Memc[field], SZ_DBNAME)
    while (nscan() == nvar+2) {
	nvar = nvar+1
	if (nvar > MAX_VARIABLES) {
	    call eprintf ("Warning: Can't replace more than %d fields at once -- nothing done\n")
		call pargi (MAX_VARIABLES)
	    call sfree (sp)
	    return (0)
	}
	rplace[nvar] = idx_field (db, Memc[field])
	if (rplace[nvar] == 0) {
	    call eprintf ("Warning: Database field not recognized (%s) -- nothing done\n")
		call pargstr (Memc[field])
	    call sfree (sp)
	    return (0)
	}
	call gargwrd (Memc[field], SZ_DBNAME)
    }
    if (nscan() < 2) {
	call eprintf ("Warning: No fields to replace -- nothing done\n")
	call sfree (sp)
	return (0)
    }

    nerr = 0
    for (k = 1; k <= nvar; k = k+1) {
	call salloc (rp[k], DB_SIZE(db,rplace[k]), DB_TYPE(db,rplace[k]))
	call salloc (erp[k], DB_SIZE(db,rplace[k]), TY_REAL)
	if (DB_ERROR(db,rplace[k]))
	    nerr = nerr + 1
    }

    # Read file line by line and replace values
    missing = NO
    iferr {
    	fd = open (Memc[filename], READ_ONLY, TEXT_FILE)
    } then {
	call erract (EA_WARN)
	call sfree (sp)
	return (0)
    }
    replaced = 0
    repeat {
        if (fd == STDIN) {
	    call printf ("Replace: ")
	    call flush (STDOUT)
	}
	if (getline (fd, Memc[buffer]) == EOF)
	    break
        # Skip comment lines and blank lines.
        if (Memc[buffer] == '#')
	    next
        for (ip=1;  IS_WHITE(Memc[buffer+ip-1]);  ip=ip+1)
	     ;
        if (Memc[buffer+ip-1] == '\n' || Memc[buffer+ip-1] == EOS)
	     next

	call sscan (Memc[buffer])

	# Read and match serial number
	ikey = garg_key (db, index)
	if (ikey == 0) {
	    if (missing == NO && fd != STDIN) {
		call gdeactivate (gp, AW_CLEAR)
		missing = YES
	    }
	    call reset_scan()
	    call gargwrd (Memc[field], SZ_DBNAME)
	    if (fd == STDIN)
	        call printf ("Search key not in defined sample (%s) --- ")
	    else
	        call eprintf ("Search key not in defined sample (%s)\n")
	    call pargstr (Memc[field])
	    next
	}	    

	# Read replacement values
	for (k = 1; k <= nvar; k = k+1) {
	    switch (DB_TYPE(db, rplace[k])) {
	    case TY_SHORT:
		call gargs (Mems[rp[k]])
	    case TY_INT:
		call gargi (Memi[rp[k]])
	    case TY_LONG:
		call gargl (Meml[rp[k]])
	    case TY_REAL:
		call gargr (Memr[rp[k]])
	    case TY_DOUBLE:
		call gargd (Memd[rp[k]])
	    case TY_BOOL:
		call gargb (Memb[rp[k]])
	    case TY_CHAR:
		call gargwrd (Memc[rp[k]], DB_SIZE(db, rplace[k])-1)
	    }
	    if (DB_ERROR(db, rplace[k]))
		call gargr (Memr[erp[k]])
	}

	# If not enough values read, skip this line
	if (nscan() < nvar+nerr+1) {
	    call eprintf ("Warning: Insufficient arguments for ")
	    call dbfprintf (STDERR, db, index[ikey], dbkey(db), NO, NO)
	    call eprintf (" -- not replaced")
	    if (fd == STDIN)
		call eprintf (" --- ")
	    else
		call eprintf ("\n")
	    next
	}

	# Replace values
	for (k = 1; k <= nvar; k = k+1) {
	    switch (DB_TYPE(db, rplace[k])) {
	    case TY_SHORT:
		call dbputs (db, index[ikey], rplace[k], Mems[rp[k]])
	    case TY_INT:
		call dbputi (db, index[ikey], rplace[k], Memi[rp[k]])
	    case TY_LONG:
		call dbputl (db, index[ikey], rplace[k], Meml[rp[k]])
	    case TY_REAL:
		call dbputr (db, index[ikey], rplace[k], Memr[rp[k]])
	    case TY_DOUBLE:
		call dbputd (db, index[ikey], rplace[k], Memd[rp[k]])
	    case TY_BOOL:
		call dbputb (db, index[ikey], rplace[k], Memb[rp[k]])
	    case TY_CHAR:
		call dbpstr (db, index[ikey], rplace[k], Memc[rp[k]])
	    }
	    if (DB_ERROR(db, rplace[k]))
		call dbperr (db, index[ikey], rplace[k], Memr[erp[k]])
	}
	replaced = replaced + 1
    }
    if (missing == YES)
	call greactivate (gp, AW_PAUSE)
    call close (fd)
    call sfree (sp)
    return (replaced)
end
