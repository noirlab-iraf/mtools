include <tbset.h>
include "pointer.h"
include "database.h"

# OPEN_TABLE -- Read an STSDAS TABLES format.  Returns number of database
# fields.

int procedure open_table (db, database)

pointer	db		# DATABASE pointer
char	database[ARB]	# STSDAS TABLES file

int	nfields, i, j, key, tbpsta(), colnum, lendata, lenfmt
pointer	lbuf, sp, tp, tbtopn(), tbcnum(), colptr
bool	streq()

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	# Open the table
	tp = tbtopn (database, READ_ONLY, 0)

	# Get number of fields
	nfields = tbpsta (tp, TBL_NCOLS)

	# Read each field
	call malloc (db,SZ_DBOFFSET+SZ_DBSTRUCT*(nfields),TY_STRUCT)
	j = 0
	do i = 1, nfields {
	    colptr = tbcnum (tp, i)
	    call tbcinf (colptr, colnum, DB_NAME(db,j+1), Memc[lbuf],
		DB_FORMAT(db,j+1), DB_TYPE(db,j+1), lendata, lenfmt)
	    if (j > 0) { # test to see whether this is an error column
	    	call strcpy ("e", Memc[lbuf], SZ_LINE)
	    	call strcat (DB_NAME(db,j), Memc[lbuf], SZ_LINE)
	    	if (streq (Memc[lbuf], DB_NAME(db,j+1))) {
		    DB_ERROR(db,j) = true
		    if (DB_TYPE(db,j+1) != TY_REAL)
			call fatal (0, "Error field must be type real")
		    call strcpy (DB_FORMAT(db,j+1), DB_EFORMAT(db,j),
				 SZ_DBFORMAT)
		    next
		}
	    }
	    # New field
	    j = j + 1	
	    if (DB_TYPE(db,j) < 0) {
		DB_SIZE(db,j) = - DB_TYPE(db,j) + 1 # room for EOS
		DB_TYPE(j) = TY_CHAR
	    } else
		DB_SIZE(db,j) = 1	# or = lendata = 1
	    DB_ERROR(db,j) = false
	}

	# Reallocate space
	if (j < nfields) {
	    nfields = j
	    call realloc (db,SZ_DBOFFSET+SZ_DBSTRUCT*(nfields),TY_STRUCT)
	}

	# Check for a key header parameter
	iferr {
	    call tbhgtt (tp, "KEY", Memc[lbuf], SZ_LINE)
	    key = 1
	    do i = 1, nfields
		if (streq (Memc[lbuf], DB_NAME(db,i))) {
		    key = i
		    break
		}
	    if (i > nfields)
		call fatal (0, "KEY header parameter doesn't match any column name")
    	} then
	    key = 1
	    
	# Close the table
	call tbtclo (tp)

	DB_NFIELDS(db) = nfields
	DB_KEY(db) = key

	call sfree (sp)
	return (nfields)
end

# READ_TABLE -- Read an STSDAS TABLES database into memory. Returns number of
# objects in the database.

int procedure read_table (db, database)

pointer	db			# DATABASE pointer
char	database[ARB]		# Database file

int	nfields, i, j, k, nrows, tbpsta()
pointer	tp, tbtopn(), tbcnum(), sp, nullflag

begin
	# Open the table
	tp = tbtopn (database, READ_ONLY, 0)

	# Get number of rows
	nrows = tbpsta (tp, TBL_NROWS)

	# Allocate junk space for nullflags
	call smark (sp)
	call salloc (nullflag, nrows, TY_BOOL)

	# Initial memory allocation
	nfields = DB_NFIELDS(db)
	for (j = 1; j <= nfields; j = j+1) {
	    call malloc (DB_POINTER(db,j), nrows*DB_SIZE(db,j), DB_TYPE(db,j))
	    if (DB_ERROR(db,j))
	    	call malloc (DB_EPOINTER(db,j), nrows*DB_SIZE(db,j), TY_REAL)
	}

	# Read in the columns.
	i = 0
	for (j = 1; j <= nfields; j = j+1) {
	    i = i + 1
	    switch (DB_TYPE(db, j)) {
	    case TY_INT:
		call tbcgti (tp, tbcnum(tp,i), Memi[DB_POINTER(db,j)],
			     Memb[nullflag], 1, nrows)
	    case TY_REAL:
		call tbcgtr (tp, tbcnum(tp,i), Memr[DB_POINTER(db,j)],
			     Memb[nullflag], 1, nrows)
	    case TY_DOUBLE:
		call tbcgtd (tp, tbcnum(tp,i), Memd[DB_POINTER(db,j)],
			     Memb[nullflag], 1, nrows)
	    case TY_BOOL:
		call tbcgtb (tp, tbcnum(tp,i), Memb[DB_POINTER(db,j)],
			     Memb[nullflag], 1, nrows)
	    case TY_CHAR:
		call tbcgtt (tp, tbcnum(tp,i), Memc[DB_POINTER(db,j)],
			     Memb[nullflag], DB_SIZE(db,j), 1, nrows)
	    }
	    # Read in the error if present
	    if (DB_ERROR(db,j)) {
		i = i + 1
		call tbcgtr (tp, tbcnum(tp,i), Memr[DB_EPOINTER(db,j)],
			     Memb[nullflag], 1, nrows)
		do k = 1, nrows
		    if (! IS_INDEFR(DB_ERR2(db,k,j)))
		    	DB_ERR2(db,k,j) = DB_ERR2(db,k,j) * DB_ERR2(db,k,j)
	    }
	}

	call tbtclo (tp)
	call sfree (sp)
	DB_NRECORDS(db) = nrows
	return (nrows)
end
