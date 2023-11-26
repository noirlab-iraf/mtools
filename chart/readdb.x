include <ctype.h>
include "database.h"

define	SZ_BUF		1000
define	SZ_BIGLINE	(10*SZ_LINE)

# READ_DB -- Read a database into memory. Returns number of objects in the
# database.

int procedure read_db (db, database)

pointer	db			# DATABASE pointer
char	database[ARB]		# Database file

int	fd, nfields, i, j, ip, lineno, open(), getlline()
int	buflen, itemp, len, ctoi(), ctol(), ctor(), ctod(), ctowrd()
pointer	sp, lbuf

begin
	call smark (sp)
	call salloc (lbuf, SZ_BIGLINE, TY_CHAR)

	i = 1
	lineno = 0
	fd = open (database, READ_ONLY, TEXT_FILE)

	# Initial memory allocation

	nfields = DB_NFIELDS(db)
	buflen = SZ_BUF
	for (j = 1; j <= nfields; j = j+1) {
	    call malloc (DB_POINTER(db,j), buflen*DB_SIZE(db,j), DB_TYPE(db,j))
	    if (DB_ERROR(db,j))
	    	call malloc (DB_EPOINTER(db,j), buflen*DB_SIZE(db,j), TY_REAL)
	}

	while (getlline (fd, Memc[lbuf], SZ_BIGLINE) != EOF) {
	    # Skip comment lines and blank lines.
	    lineno = lineno + 1
	    if (Memc[lbuf] == '#')
		next
	    for (ip=1;  IS_WHITE(Memc[lbuf+ip-1]);  ip=ip+1)
		;
	    if (Memc[lbuf+ip-1] == '\n' || Memc[lbuf+ip-1] == EOS)
		next

	    # Allocate additional memory if needed.
	    if (i > buflen) {
		buflen = buflen + SZ_BUF
		for (j = 1; j <= nfields; j = j+1) {
		    call realloc (DB_POINTER(db,j), buflen*DB_SIZE(db,j),
				  DB_TYPE(db,j))
		    if (DB_ERROR(db,j))
		    	call realloc (DB_EPOINTER(db,j), buflen*DB_SIZE(db,j),
				  TY_REAL)
		}
	    }

	    # Read in the variables.
	    for (j = 1; j <= nfields; j = j+1) {
		switch (DB_TYPE(db, j)) {
		case TY_SHORT:
		    len = ctoi (Memc[lbuf], ip, itemp)
		    DB_VALS(db,i,j) = itemp
		case TY_INT:
		    len = ctoi (Memc[lbuf], ip, DB_VALI(db,i,j))
		case TY_LONG:
		    len = ctol (Memc[lbuf], ip, DB_VALL(db,i,j))
		case TY_REAL:
		    len = ctor (Memc[lbuf], ip, DB_VALR(db,i,j))
		case TY_DOUBLE:
		    len = ctod (Memc[lbuf], ip, DB_VALD(db,i,j))
		case TY_BOOL:
		    while (IS_WHITE(Memc[lbuf+ip-1]))
			ip = ip + 1
		    itemp = ip
		    switch (Memc[lbuf+ip-1]) {
		    case 'Y','y','T','t':
			DB_VALB(db,i,j) = true
			ip = ip + 1
			while (IS_ALPHA(Memc[lbuf+ip-1]))
			    ip = ip + 1
			len = ip - itemp
		    case 'N','n','F','f':
			DB_VALB(db,i,j) = false
			ip = ip + 1
			while (IS_ALPHA(Memc[lbuf+ip-1]))
			    ip = ip + 1
			len = ip - itemp
		    default:
			len = 0
		    }
		case TY_CHAR:
		    len = ctowrd (Memc[lbuf], ip, DB_VALC(db,i,j),
				  DB_SIZE(db,j)-1)
		}
		# Test that the variable was read
		if (len == 0)
		    break
		# Read in the error if present
		if (DB_ERROR(db,j)) {
		    len = ctor (Memc[lbuf], ip, DB_ERR2(db,i,j))
		    if (! IS_INDEFR(DB_ERR2(db,i,j)))
		    	DB_ERR2(db,i,j) = DB_ERR2(db,i,j) * DB_ERR2(db,i,j)
		    # Test that the error was read
		    if (len == 0)
		    	break
		}
	    }

	    if (len == 0) {
		call eprintf ("Warning: Insufficient args; %s, line %d: %s\n")
		    call pargstr (database)
		    call pargi (lineno)
		    call pargstr (Memc[lbuf])
		next
	    }
	    i = i+1
	}
	i = i-1

	# Final memory reallocation
	for (j = 1; j <= nfields; j = j+1) {
	    call realloc (DB_POINTER(db,j), i*DB_SIZE(db,j), DB_TYPE(db,j))
	    if (DB_ERROR(db,j))
		call realloc (DB_EPOINTER(db,j), i*DB_SIZE(db,j), TY_REAL)
	}

	call close (fd)
	call sfree (sp)
	DB_NRECORDS(db) = i
	return (i)
end
