# This file contains miscellaneous database procedures.

include "database.h"

# DBKEY -- Return the database key field index.

int procedure dbkey (db)
pointer	db	# DATABASE pointer

begin
    return (DB_KEY(db))
end

# DBNFIELDS -- Return the number of fields in the database.

int procedure dbnfields (db)
pointer	db	# DATABASE pointer

begin
    return (DB_NFIELDS(db))
end

# PARG_DBNAME -- Put out the name of the specified field.

procedure parg_dbname (db, field)
pointer	db		# DATABASE pointer
int	field		# Database field

begin
    call pargstr (DB_NAME(db, field))
end

# PARG_DBFORMAT (db, field)

procedure parg_dbformat (db, field)
pointer	db		# DATABASE pointer
int	field		# Database field

begin
    call pargstr (DB_FORMAT(db, field))
end

# DBERROR -- Return true if this field has an associated error field,
# otherwise return false.

bool procedure dberror (db, field)
pointer	db		# DATABASE pointer
int	field		# Database field

begin
    return (DB_ERROR(db, field))
end

# DBFPRINTF -- Print a database entry with its format string.

procedure dbfprintf (fd, db, record, field, error, sign)
int	fd	# File descriptor
pointer	db	# DATABASE pointer
int	record	# Database record
int	field	# Database field
int	error	# Print the error field if present?
int	sign	# If printing the error field, print a "+-" sign?

pointer	sp, buffer
short	dbgets()
int	dbgeti()
long	dbgetl()
real	dbgetr(), dbgerr()
double	dbgetd()
bool	dbgetb()
char    fmt[10]

begin
    if (DB_TYPE(db, field) == TY_CHAR) {
	call sprintf (fmt, 10, "'%%-%ds")
	call pargi (DB_SIZE(db, field)+1)
        call fprintf (fd, fmt)
    } else
        call fprintf (fd, DB_FORMAT(db, field))
    switch (DB_TYPE(db, field)) {
    case TY_SHORT:
	call pargs (dbgets (db, record, field))
    case TY_INT:
	call pargi (dbgeti (db, record, field))
    case TY_LONG:
	call pargl (dbgetl (db, record, field))
    case TY_REAL:
	call pargr (dbgetr (db, record, field))
    case TY_DOUBLE:
	call pargd (dbgetd (db, record, field))
    case TY_BOOL:
	call pargb (dbgetb (db, record, field))
    case TY_CHAR:
	call smark (sp)
	call salloc (buffer, DB_SIZE(db, field)+1, TY_CHAR)
	call dbgstr (db, record, field, Memc[buffer], DB_SIZE(db, field)+1)
	call strcat ("'", Memc[buffer], DB_SIZE(db, field)+1)
	call pargstr (Memc[buffer])
	call sfree (sp)
    default:
    }
    if (error == YES && DB_ERROR(db, field)) {
	if (sign == YES)
	    call fprintf (fd, " +- ")
	else
	    call fprintf (fd, "  ")
	call fprintf (fd, DB_EFORMAT(db, field))
	call pargr (dbgerr (db, record, field))
    }
end

# IDX_FIELD -- Return the field structure index for a named field.
# Returns 0 if the named field doesn't match any of the fields in the
# database.

int procedure idx_field (db, fname)
pointer	db
char	fname[ARB]

int	i, j, first, last, gstrmatch(), len, strlen()
pointer	sp, field

begin
    call smark (sp)
    call salloc (field, SZ_DBNAME+2, TY_CHAR)
    for (i = 1; i <= DB_NFIELDS(db); i = i+1) {
	call sprintf (Memc[field], SZ_DBNAME+2, "{%s}")
	    call pargstr (DB_NAME(db, i))
	len = strlen(fname)
	j = gstrmatch (fname, Memc[field], first, last)
	if (j == len+1 && first == 1)
	    break
    }
    if (i > DB_NFIELDS(db))
	i = 0
    call sfree (sp)
    return (i)
end

# GARG_KEY -- Get a KEY request from the input stream.  Returns the
# selected element index position for that KEY, or 0 if it doesn't
# find it.

define	RKEY_TOLERANCE	0.0005	# Required accurancy to match datatype real
				# or double precision keys

int procedure garg_key (db, index)
pointer	db		# DATABASE pointer
int	index[ARB]	# Selected elements index

int	field, j
pointer	sp, key, buffer
bool	streq()
short	dbgets()
int	dbgeti()
long	dbgetl()
real	dbgetr()
double	dbgetd()

begin
    field = DB_KEY(db)
    # Read the KEY in appropriate format
    call smark (sp)
    call salloc (key, DB_SIZE(db, field), DB_TYPE(db, field))
    switch (DB_TYPE(db, field)) {
    case TY_SHORT:
	call gargs (Mems[key])
    case TY_INT:
	call gargi (Memi[key])
    case TY_LONG:
	call gargl (Meml[key])
    case TY_REAL:
	call gargr (Memr[key])
    case TY_DOUBLE:
	call gargd (Memd[key])
    case TY_CHAR:
	call gargwrd (Memc[key], DB_SIZE(db, field)-1)
    }

    # Find matching database KEY
    switch (DB_TYPE(db, field)) {
    case TY_SHORT:
	for (j = 1; index[j] > 0; j = j + 1)
	    if (dbgets (db, index[j], field) == Mems[key])
		break
    case TY_INT:
	for (j = 1; index[j] > 0; j = j + 1)
	    if (dbgeti (db, index[j], field) == Memi[key])
		break
    case TY_LONG:
	for (j = 1; index[j] > 0; j = j + 1)
	    if (dbgetl (db, index[j], field) == Meml[key])
		break
    case TY_REAL:
	for (j = 1; index[j] > 0; j = j + 1)
	    if (abs(dbgetr (db, index[j], field) - Memr[key]) <=
		RKEY_TOLERANCE)
		break
    case TY_DOUBLE:
	for (j = 1; index[j] > 0; j = j + 1)
	    if (abs(dbgetd (db, index[j], field) - Memd[key]) <=
		RKEY_TOLERANCE)
		break
    case TY_CHAR:
        call salloc (buffer, DB_SIZE(db, field)-1, DB_TYPE(db, field))
	for (j = 1; index[j] > 0; j = j + 1) {
	    call dbgstr(db, index[j], field, Memc[buffer], DB_SIZE(db,field)-1)
	    if (streq (Memc[buffer], Memc[key]))
		break
	}
    }
    if (index[j] == 0)
	j = 0
    call sfree (sp)
    return (j)
end
