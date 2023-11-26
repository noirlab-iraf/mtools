include "pointer.h"

# The DATABASE data structure
define	SZ_DBNAME	10
define	SZ_DBFORMAT	10
define	SZ_DBSTRUCT	(3+2+SZ_DBNAME+SZ_DBFORMAT) 	# +2 for two EOSs
define	SZ_DBOFFSET	5

define	DB_NFIELDS	Memi[$1]    	# Number of fields in the database
define	DB_NENTRIES	Memi[$1+1]  	# Number of entries in the database
define	DB_SZRECORD 	Memi[$1+2]  	# Number of structure elements per rec
define	DB_KEY	    	Memi[$1+3]  	#Field that uniquely identifies entries
define	DB_PTR(db)  	Memp[$1+4]  	# Pointer to data
define	DB_TYPE	    	Memi[$1+SZ_DBOFFSET+SZ_DBSTRUCT*($2-1)]     # Datatype
define	DB_SIZE	    	Memi[$1+SZ_DBOFFSET+SZ_DBSTRUCT*($2-1)+1]   # Size
define	DB_OFFSET   	Memi[$1+SZ_DBOFFSET+SZ_DBSTRUCT*($2-1)+2]   # Offset
define	DB_NAME	    	Memc[P2C($1+SZ_DBOFFSET+SZ_DBSTRUCT*($2-1)+3]   # Name
define	DB_FORMAT	Memc[P2C($1+SZ_DBOFFSET+SZ_DBSTRUCT*($2-1)+3+SZ_DBNAME+1]  # Format

# Convenient abbrev.	DB_VALx(db, record, field)
define DB_VALS  Mems[P2S(DB_PTR($1)+DB_SZRECORD(db)*(($2)-1)+DB_OFFSET(db,$3))]
define DB_VALI  Mems[DB_PTR($1)+DB_SZRECORD(db)*(($2)-1)+DB_OFFSET(db,$3)]
define DB_VALL  Mems[P2L(DB_PTR($1)+DB_SZRECORD(db)*(($2)-1)+DB_OFFSET(db,$3))]
define DB_VALR  Mems[DB_PTR($1)+DB_SZRECORD(db)*(($2)-1)+DB_OFFSET(db,$3)]
define DB_VALD  Mems[P2D(DB_PTR($1)+DB_SZRECORD(db)*(($2)-1)+DB_OFFSET(db,$3))]
define DB_VALB  Mems[DB_PTR($1)+DB_SZRECORD(db)*(($2)-1)+DB_OFFSET(db,$3)]
define DB_VALC  Memc[P2C(DB_PTR($1)+DB_SZRECORD(db)*(($2)-1)+DB_OFFSET(db,$3))]
