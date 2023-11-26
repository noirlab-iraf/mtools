include <ctype.h>
include "pointer.h"
include "database.h"

# OPEN_FORMAT -- Read a database format file.  Returns number of database
# fields.

define	SZ_KEY	4
define	SZ_TYPEVAR	10	# Max size of a variable type string(i.e."INT")

int procedure open_format (db, dbformat)

pointer	db		# DATABASE pointer
char	dbformat[ARB]	# Database format file

int	ip, nfields, fd, getline(), nscan(), open(), j, key, offset, d_offset
int	size
pointer	lbuf, sp, typevar
char	typevar[SZ_TYPEVAR]
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
	offset = 0
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
		call gargwrd (Memc(typevar), SZ_TYPEVAR)
		call gargwrd (DB_FORMAT(db, nfields), SZ_DBFORMAT)

	    if (nscan() != 3) {
		call eprintf("Warning: Underspecified database field (%s)\n")
		    call pargstr (Memc[lbuf])
		call fatal (0, "Bad database format file")
	    }
	    call strupr (Memc(typevar))
	    if (streq(Memc(typevar), "DOUBLE")) {
		DB_TYPE(db, nfields) = TY_DOUBLE
		DB_SIZE(db, nfields) = 1
		size = SZ_DOUBLE
		d_offset = SZ_DOUBLE
	    } else if (streq(Memc(typevar), "REAL")) {
		DB_TYPE(db, nfields) = TY_REAL
		DB_SIZE(db, nfields) = 1
		size = SZ_REAL
		d_offset = SZ_REAL
	    } else if (streq(Memc(typevar), "SHORT")) {
		DB_TYPE(db, nfields) = TY_SHORT
		DB_SIZE(db, nfields) = 1
		size = SZ_SHORT
		d_offset = SZ_SHORT
	    } else if (streq(Memc(typevar), "INT")) {
		DB_TYPE(db, nfields) = TY_INT
		DB_SIZE(db, nfields) = 1
		size = SZ_INT
		d_offset = SZ_INT
	    } else if (streq(Memc(typevar), "LONG")) {
		DB_TYPE(db, nfields) = TY_LONG
		DB_SIZE(db, nfields) = 1
		size = SZ_LONG
		d_offset = SZ_LONG
	    } else if (streq(Memc(typevar), "BOOLEAN")) {
		DB_TYPE(db, nfields) = TY_BOOL
		DB_SIZE(db, nfields) = 1
		size = SZ_BOOL
		d_offset = SZ_BOOL
	    } else if (streq(Memc(typevar), "BOOL")) {
		DB_TYPE(db, nfields) = TY_BOOL
		DB_SIZE(db, nfields) = 1
		size = SZ_BOOL
		d_offset = SZ_BOOL
	    } else if (streq(Memc(typevar), "CHAR")) {
		DB_TYPE(db, nfields) = TY_CHAR
		call gargi (DB_SIZE(db, nfields))
		if (nscan() != 4) {
		    call eprintf ("String length not specified: %s")
		    	call pargstr (Memc[lbuf])
		    call fatal (0, "Bad database format file")
		}
		size = SZ_CHAR
		d_offset = SZ_CHAR * (DB_SIZE(db, nfields) + 1) # +1 for EOS
	    } else {
		call eprintf("Warning: Illegal database field datatype (%s)\n")
		    call pargstr (Memc[lbuf])
		call fatal (0, "Bad database format file")
	    }
	    # Guarantee proper alignment for datatype
	    if (size > SZ_STRUCT)
		offset = offset + mod (offset, size/SZ_STRUCT)
	    DB_OFFSET(db, nfields) = offset
	    # Determine next offset
	    offset = offset + d_offset / SZ_STRUCT
	    if (mod (d_offset, SZ_STRUCT) > 0)
		offset = offset + 1
	    # Test whether this is the search KEY field
	    j = nscan()
	    call gargwrd (Memc[typevar], SZ_TYPEVAR)
	    if (nscan() == j+1)
		if (streq("key", Memc[typevar]) || streq("KEY", Memc[typevar]))
		    key = nfields
	}
	DB_SZRECORD(db) = offset
	DB_NFIELDS(db) = nfields
	DB_KEY(db) = key

	call close (fd)
	call sfree (sp)
	return (nfields)
end

# CLOSE_FORMAT -- Close a database format structure.

procedure close_format (db)
pointer	db

int i, j
begin
    call mfree (DB_PTR(db), TY_STRUCT)
    call mfree (db, TY_STRUCT)
end
