include "pointer.h"

# The DATABASE data structure
# Note that were allocation a structure element (size 2 chars) for each
# character in the nam and format strings.  This wastes some space, but it
# insures proper word alignment.  This is the recommended procedure by the
# IRAF people for small structures like this.  It also guarantees adequate
# space for the EOSs.
define	SZ_DBNAME	20
define	SZ_DBFORMAT	10
define	SZ_DBSTRUCT	(5+SZ_DBNAME+2*SZ_DBFORMAT)
define	SZ_DBOFFSET	3

define	DB_NFIELDS	Memi[$1]    	# Number of fields in the database
define	DB_NRECORDS	Memi[$1+1]  	# Number of records in the database
define	DB_KEY	    	Memi[$1+2]  	#Field that uniquely identifies entries
define	DB_POINTER	Memp[$1+SZ_DBOFFSET+SZ_DBSTRUCT*($2-1)]	    # Storage
define	DB_TYPE	    	Memi[$1+SZ_DBOFFSET+SZ_DBSTRUCT*($2-1)+1]   # Datatype
define	DB_SIZE	    	Memi[$1+SZ_DBOFFSET+SZ_DBSTRUCT*($2-1)+2]   # Size
define	DB_ERROR    	Memb[$1+SZ_DBOFFSET+SZ_DBSTRUCT*($2-1)+3]   # Errors?
define	DB_EPOINTER	Memp[$1+SZ_DBOFFSET+SZ_DBSTRUCT*($2-1)+4]   # Err store
define	DB_NAME	    	Memc[P2C($1+SZ_DBOFFSET+SZ_DBSTRUCT*($2-1)+5)]   # Name
define	DB_FORMAT      Memc[P2C($1+SZ_DBOFFSET+SZ_DBSTRUCT*($2-1)+5+SZ_DBNAME)]
define	DB_EFORMAT     Memc[P2C($1+SZ_DBOFFSET+SZ_DBSTRUCT*($2-1)+5+SZ_DBNAME+SZ_DBFORMAT)]

# Convenient abbrev.	DB_VALx(db, record, field)
define DB_VALS  Mems[DB_POINTER($1,$3)+($2)-1]
define DB_VALI  Memi[DB_POINTER($1,$3)+($2)-1]
define DB_VALL  Meml[DB_POINTER($1,$3)+($2)-1]
define DB_VALR  Memr[DB_POINTER($1,$3)+($2)-1]
define DB_VALD  Memd[DB_POINTER($1,$3)+($2)-1]
define DB_VALB  Memb[DB_POINTER($1,$3)+($2)-1]
define DB_VALC  Memc[DB_POINTER($1,$3)+DB_SIZE($1,$3)*(($2)-1)]

define DB_ERR2	Memr[DB_EPOINTER($1,$3)+($2)-1]
