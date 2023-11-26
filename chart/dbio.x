# This contains the basic DBIO routines.

include "database.h"

# DBGET$T -- Get the value for the specified field in the specified record
# in a database.


short procedure dbgets (db, record, field)
pointer	db		# DATABASE pointer
int	record		# Record to fetch
int	field		# Field to fetch from the record

begin
    return (DB_VALS(db, record, field))
end


int procedure dbgeti (db, record, field)
pointer	db		# DATABASE pointer
int	record		# Record to fetch
int	field		# Field to fetch from the record

begin
    return (DB_VALI(db, record, field))
end


long procedure dbgetl (db, record, field)
pointer	db		# DATABASE pointer
int	record		# Record to fetch
int	field		# Field to fetch from the record

begin
    return (DB_VALL(db, record, field))
end


real procedure dbgetr (db, record, field)
pointer	db		# DATABASE pointer
int	record		# Record to fetch
int	field		# Field to fetch from the record

begin
    return (DB_VALR(db, record, field))
end


double procedure dbgetd (db, record, field)
pointer	db		# DATABASE pointer
int	record		# Record to fetch
int	field		# Field to fetch from the record

begin
    return (DB_VALD(db, record, field))
end


bool procedure dbgetb (db, record, field)
pointer	db		# DATABASE pointer
int	record		# Record to fetch
int	field		# Field to fetch from the record

begin
    return (DB_VALB(db, record, field))
end


# DBGSTR -- Get the string from the specified character field in the specified
# record in a database.

procedure dbgstr (db, record, field, outstr, maxch)
pointer	db		# DATABASE pointer
int	record		# Record to fetch
int	field		# Field to fetch from the record
char	outstr[ARB]	# Output string
int	maxch		# Maximum number of characters in outstr

begin
    call strcpy (DB_VALC(db, record, field), outstr, maxch)
end

# DBPUT$T -- Put the given value into the specified field in the specified
# record in a database.


procedure dbputs (db, record, field, value)
pointer	db		# DATABASE pointer
int	record		# Record to fetch
int	field		# Field to fetch from the record
short	value		# Value to put into the database

begin
    DB_VALS(db, record, field) = value
end


procedure dbputi (db, record, field, value)
pointer	db		# DATABASE pointer
int	record		# Record to fetch
int	field		# Field to fetch from the record
int	value		# Value to put into the database

begin
    DB_VALI(db, record, field) = value
end


procedure dbputl (db, record, field, value)
pointer	db		# DATABASE pointer
int	record		# Record to fetch
int	field		# Field to fetch from the record
long	value		# Value to put into the database

begin
    DB_VALL(db, record, field) = value
end


procedure dbputr (db, record, field, value)
pointer	db		# DATABASE pointer
int	record		# Record to fetch
int	field		# Field to fetch from the record
real	value		# Value to put into the database

begin
    DB_VALR(db, record, field) = value
end


procedure dbputd (db, record, field, value)
pointer	db		# DATABASE pointer
int	record		# Record to fetch
int	field		# Field to fetch from the record
double	value		# Value to put into the database

begin
    DB_VALD(db, record, field) = value
end


procedure dbputb (db, record, field, value)
pointer	db		# DATABASE pointer
int	record		# Record to fetch
int	field		# Field to fetch from the record
bool	value		# Value to put into the database

begin
    DB_VALB(db, record, field) = value
end


# DBPSTR -- Put the input string into the specified character field in the
# specified record in a database.

procedure dbpstr (db, record, field, str)
pointer	db		# DATABASE pointer
int	record		# Record to fetch
int	field		# Field to fetch from the record
char	str[ARB]	# Input string to enter into the database

begin
    call strcpy (str, DB_VALC(db, record, field), DB_SIZE(db, field)-1)
end

# DBGERR -- Get the error for the specified field in the specified
# record in a database.

real procedure dbgerr (db, record, field)
pointer	db		# DATABASE pointer
int	record		# Record to fetch
int	field		# Field to fetch from the record

begin
    if (IS_INDEFR(DB_ERR2(db, record, field)))
	return (INDEFR)
    else
    	return (sqrt(DB_ERR2(db, record, field)))
end

# DBGERR2 -- Get the square error for the specified field in the specified
# record in a database.

real procedure dbgerr2 (db, record, field)
pointer	db		# DATABASE pointer
int	record		# Record to fetch
int	field		# Field to fetch from the record

begin
    return (DB_ERR2(db, record, field))
end

# DBPERR -- Sqaure and put the given value into the square error for the
# specified field in the specified record in a database.  The input value
# must be the square error.

procedure dbperr (db, record, field, value)
pointer	db		# DATABASE pointer
int	record		# Record to fetch
int	field		# Field to fetch from the record
real	value		# The square error to put into the database

begin
    if (IS_INDEFR(value))
    	DB_ERR2(db, record, field) = INDEFR
    else
    	DB_ERR2(db, record, field) = value * value
end


