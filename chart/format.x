include <ctype.h>
include "pointer.h"
include "database.h"

# OPEN_FORMAT -- Read a database format file.  Returns number of database
# fields.

define	SZ_TYPEVAR	10	# Max size of a variable type string(i.e."INT")

int procedure open_format (db, dbformat)

pointer	db		# DATABASE pointer
char	dbformat[ARB]	# Database format file

int	ip, nfields, fd, getline(), nscan(), open(), j, key
pointer	lbuf, sp, typevar
bool	streq()

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)
	call salloc (typevar, SZ_TYPEVAR, TY_CHAR)
	call malloc (db, SZ_DBOFFSET+SZ_DBSTRUCT, TY_STRUCT)

	# Get database format file

	fd = open (dbformat, READ_ONLY, TEXT_FILE)

	# Read each field

	nfields = 0
	key = 1
	while (getline (fd, Memc[lbuf]) != EOF) {
	    # Skip comment lines and blank lines.
	    if (Memc[lbuf] == '#')
		next
	    for (ip=lbuf;  IS_WHITE(Memc[ip]);  ip=ip+1)
		;
	    if (Memc[ip] == '\n' || Memc[ip] == EOS)
		next
	
	    # Read field description.
	    nfields = nfields + 1

    	    call realloc (db,SZ_DBOFFSET+SZ_DBSTRUCT*(nfields),TY_STRUCT)

	    call sscan(Memc[ip])
		call gargwrd (DB_NAME(db, nfields), SZ_DBNAME)
		call gargwrd (Memc[typevar], SZ_TYPEVAR)
		call gargwrd (DB_FORMAT(db, nfields), SZ_DBFORMAT)

	    if (nscan() != 3) {
		call eprintf("Warning: Underspecified database field (%s)\n")
		    call pargstr (Memc[lbuf])
		call fatal (0, "Bad database format file")
	    }
	    call strupr (Memc[typevar])
	    if (streq(Memc[typevar], "DOUBLE")) {
		DB_TYPE(db, nfields) = TY_DOUBLE
		DB_SIZE(db, nfields) = 1
	    } else if (streq(Memc[typevar], "REAL")) {
		DB_TYPE(db, nfields) = TY_REAL
		DB_SIZE(db, nfields) = 1
	    } else if (streq(Memc[typevar], "SHORT")) {
		DB_TYPE(db, nfields) = TY_SHORT
		DB_SIZE(db, nfields) = 1
	    } else if (streq(Memc[typevar], "INT")) {
		DB_TYPE(db, nfields) = TY_INT
		DB_SIZE(db, nfields) = 1
	    } else if (streq(Memc[typevar], "LONG")) {
		DB_TYPE(db, nfields) = TY_LONG
		DB_SIZE(db, nfields) = 1
	    } else if (streq(Memc[typevar], "BOOLEAN")) {
		DB_TYPE(db, nfields) = TY_BOOL
		DB_SIZE(db, nfields) = 1
	    } else if (streq(Memc[typevar], "BOOL")) {
		DB_TYPE(db, nfields) = TY_BOOL
		DB_SIZE(db, nfields) = 1
	    } else if (streq(Memc[typevar], "CHAR")) {
		DB_TYPE(db, nfields) = TY_CHAR
		call gargi (DB_SIZE(db, nfields))
		DB_SIZE(db, nfields) = DB_SIZE(db, nfields) + 1 # room for EOS
		if (nscan() != 4) {
		    call eprintf ("String length not specified: %s")
		    	call pargstr (Memc[lbuf])
		    call fatal (0, "Bad database format file")
		}
	    } else {
		call eprintf("Warning: Illegal database field datatype (%s)\n")
		    call pargstr (Memc[lbuf])
		call fatal (0, "Bad database format file")
	    }
	    if (DB_FORMAT(db, nfields) != '%') {
		call eprintf("Warning: Bad format string (%s)\n")
	    	    call pargstr (Memc[lbuf])
		call fatal (0, "Bad database format file")
	    }
	    # Test whether this is the search KEY field, or if it has an error
	    DB_ERROR(db, nfields) = false
	    j = nscan()
	    call gargwrd (Memc[typevar], SZ_TYPEVAR)
	    if (nscan() == j+1) {
		if (streq("key", Memc[typevar]) || streq("KEY",Memc[typevar]))
		    key = nfields
		else if (streq("error", Memc[typevar]) ||
			 streq("ERROR", Memc[typevar])) {
		    DB_ERROR(db, nfields) = true
		    call gargwrd (DB_EFORMAT(db, nfields), SZ_DBFORMAT)
		    if (nscan() != j+2) {
			call eprintf("Warning: Missing format string for an error field (%s)\n")
		    	    call pargstr (Memc[lbuf])
			call fatal (0, "Bad database format file")
		    }
		    if (DB_EFORMAT(db, nfields) != '%') {
			call eprintf("Warning: Bad format string for an error field (%s)\n")
		    	    call pargstr (Memc[lbuf])
			call fatal (0, "Bad database format file")
		    }
		}
	    }
	    j = nscan()
	    call gargwrd (Memc[typevar], SZ_TYPEVAR)
	    if (nscan() == j+1) {
		if (streq("key", Memc[typevar]) || streq("KEY",Memc[typevar]))
		    key = nfields
		else if (streq("error", Memc[typevar]) ||
			 streq("ERROR",Memc[typevar])) {
		    DB_ERROR(db, nfields) = true
		    call gargwrd (DB_EFORMAT(db, nfields), SZ_DBFORMAT)
		    if (nscan() != j+2) {
			call eprintf("Warning: Missing format string for an error field (%s)\n")
		    	    call pargstr (Memc[lbuf])
			call fatal (0, "Bad database format file")
		    }
		    if (DB_EFORMAT(db, nfields) != '%') {
			call eprintf("Warning: Bad format string for an error field (%s)\n")
		    	    call pargstr (Memc[lbuf])
			call fatal (0, "Bad database format file")
		    }
		}
	    }
	}
	DB_NFIELDS(db) = nfields
	DB_KEY(db) = key

	call close (fd)
	call sfree (sp)
	return (nfields)
end

# CLOSE_FORMAT -- Close a database format structure.

procedure close_format (db)
pointer	db

int i
begin
    for (i = 1; i <= DB_NFIELDS(db); i = i+1) {
	call mfree (DB_POINTER(db, i), DB_TYPE(db, i))
	if (DB_ERROR(db, i))
	    call mfree (DB_EPOINTER(db, i), TY_REAL)
    }
    call mfree (db, TY_STRUCT)
end
