include <error.h>
include "postfix.h"
include "token.h"

define MAX_EXPR_SIZE	200	    # Maximum size of an expression

# PR_FLIST -- Print the values of the functions listed in a word list for
# a specified database entry, according to a given format string.  Returns
# OK if expression and format string are valid, else returns ERR.

int procedure pr_flist (fd, expr, db, index, format, printit)
int	fd		# Output file descriptor
char	expr[ARB]	# Word list of functions to print
pointer	db		# DATABASE pointer
int	index[ARB]	# Selected element index
char	format[ARB]	# Format string
int	printit		# Print the expression (YES) or just test it (NO)

pointer	sp, word, pf, format2, pf2, ipf2, junk1, junk2, buf
int	j, style, i, datatype, parse(), ipf, ip, len, len2, ipsave
int	ctowrd(), strlen(), errcode(), ctofmt()
bool	streq(), errors

begin
    # Allocate postfix stack, and tempory strings
    call smark (sp)
    call salloc (pf, SZ_POSTFIX*MAXDEPTH, TY_STRUCT)
    call salloc (word, MAX_EXPR_SIZE, TY_CHAR)
    call salloc (buf, MAX_EXPR_SIZE, TY_CHAR)
    call salloc (format2, SZ_LINE, TY_CHAR)

    len = strlen (format)
    ip = 1
    j = 1
    i = 0
    ipf = 0
    iferr {
      while (ctowrd (expr, ip, Memc[word], MAX_EXPR_SIZE) > 0) {
	errors = false
	if (streq(Memc[word], "SEQUENCE") || streq(Memc[word], "sequence")) {
	    ipf = ipf + 1
	    PF_ACTION(pf, ipf) = SEQUENCE
	    PF_DTYPE1(pf, ipf) = TY_INT
	    datatype = TY_INT
	} else {
	    ipsave = ip
	    if (ctowrd (expr, ipsave, Memc[buf], MAX_EXPR_SIZE) > 0)
	    	if (streq(Memc[buf], "error") || streq(Memc[buf], "ERROR") ||
	            streq(Memc[buf], "err")   || streq(Memc[buf], "ERR")) {
		    errors = true
		    ip = ipsave
		}
	    pf2 = pf + SZ_POSTFIX*ipf
	    datatype = parse (Memc[word], db, pf2, ipf2, errors, true)
	    if (datatype == ERR)
		call error (1, "")
	    if (errors && (datatype != TY_DOUBLE && datatype != TY_INT))
		call error (1, "can't print error for boolean or string expr")
	    ipf2 = ipf2 - 1	# Remove END_OF_EXPRESSION
	    ipf = ipf + ipf2
	}
	if (len > 0) {
		style = ctofmt (format, j, Memc[format2], SZ_LINE)
	    	switch (style) {
	    	case TY_DOUBLE:
		    switch (datatype) {
		    case TY_INT:
		    	ipf = ipf + 1
		    	PF_ACTION(pf, ipf) = CHTYPE1
		    	PF_DTYPE1(pf, ipf) = datatype
		    	PF_DTYPE2(pf, ipf) = style
		    	datatype = style
		    case TY_DOUBLE:
		    case TY_CHAR, TY_BOOL:
		    	call error (2, "mismatched datatype and format string")
		    }
	    	case TY_INT:
		    switch (datatype) {
		    case TY_INT:
		    case TY_DOUBLE:
		    	ipf = ipf + 1
		    	PF_ACTION(pf, ipf) = CHTYPE1
		    	PF_DTYPE1(pf, ipf) = datatype
		    	PF_DTYPE2(pf, ipf) = style
		    	datatype = style
		    case TY_CHAR, TY_BOOL:
		    	call error (2, "mismatched datatype and format string")
		    }
	    	case TY_BOOL, TY_CHAR:
		    if (style != datatype)
		    	call error (2, "mismatched datatype and format string")
	    	}
	    	if (errors) {
		    len2 = strlen(Memc[format2])
		    style = ctofmt (format, j, Memc[format2+len2], SZ_LINE)
		    if (style != TY_DOUBLE)
		    	call error (2, "format string for error must be of type real")
		}
        } else {
	    Memc[format2] = ' '
	    Memc[format2+1] = ' '
	    Memc[format2+2] = '%'
	    switch (datatype) {
	    case TY_INT:
		Memc[format2+3] = 'd'
	    case TY_DOUBLE:
		Memc[format2+3] = 'g'
	    case TY_BOOL:
		Memc[format2+3] = 'b'
	    case TY_CHAR:
		Memc[format2+3] = 's'
	    }
	    if (errors) {
		Memc[format2+4] = ' '
		Memc[format2+5] = ' '
		Memc[format2+6] = '%'
		Memc[format2+7] = 'g'
	    	Memc[format2+8] = EOS
	    } else
	    	Memc[format2+4] = EOS
	}
	ipf = ipf + 1
	PF_ACTION(pf, ipf) = PRINTF
	PF_DTYPE1(pf, ipf) = datatype
	PF_FD(pf, ipf) = fd
	len2 = strlen (Memc[format2])
	call malloc (PF_VALP(pf, ipf), len2, TY_CHAR)
	call strcpy (Memc[format2], Memc[PF_VALP(pf, ipf)], len2)
	if (errors) {
	    ipf = ipf + 1
	    PF_ACTION(pf, ipf) = ERRORS_OFF
	}
      }
      # Add rest of input format string to last output format string
      if (len > 0) {
	len2 = strlen (Memc[format2])
	i = len2 + 1
    	while (format[j] != EOS) {
	    # Test for too many format strings
	    if (format[j] == '%') {
		if (format[j+1] != '%')
		    call error (2, "too few functions")
	    	Memc[format2+i-1] = format[j]
		i = i + 1
		j = j + 1
	    }
	    Memc[format2+i-1] = format[j]
	    i = i + 1
	    j = j + 1
    	}
    	Memc[format2+i-1] = EOS
      }
      # Add a carriage return if missing
      len2 = strlen (Memc[format2])
      if (Memc[format2+len2-1] != '\n') {
	Memc[format2+len2] = '\n'
    	Memc[format2+len2+1] = EOS
      }
      len2 = strlen (Memc[format2])
      if (errors) {
      	call realloc (PF_VALP(pf, ipf-1), len2, TY_CHAR)
      	call strcpy (Memc[format2], Memc[PF_VALP(pf, ipf-1)], len2)
      } else {
      	call realloc (PF_VALP(pf, ipf), len2, TY_CHAR)
      	call strcpy (Memc[format2], Memc[PF_VALP(pf, ipf)], len2)
      }
    } then {
	call clean_pf (pf, ipf)
	# If error occurred when parsing, then an error message has already
	# been printed.
	if (errcode() == 2) {
	    call eprintf ("Warning: Illegal print list (\"%s\": %s): ")
	    	call pargstr (format)
	    	call pargstr (expr)
	    call flush (STDERR)
	    call erract (EA_WARN)
	}
	call sfree (sp)
	return (ERR)
    }
    if (printit == YES)
    	call evaluate (pf, ipf, db, index, junk1, junk2, TY_CHAR, TY_CHAR)

    call clean_pf (pf, ipf)
    call sfree (sp)
    return (OK)
end

# CTOFMT -- Similar to CTOWRD, except that rather than fetching the next word,
# it fetches the next valid format substring.  Returns the datatype of the
# format substring.  Calls ERROR on an error.

int procedure ctofmt (str, ip, outstr, maxch)
char	str[ARB]			# input string
int     ip                              # pointer into input string
char    outstr[ARB]                     # receives extracted word
int     maxch

int	new, fini, j, style, ndot

begin
    new = NO
    fini = NO

    # Determine data type from format string
    j = ip
    repeat {
    	switch (str[j]) {
    	case EOS:
	    call error (2, "format string too short")
    	case '%':
	    if (new == YES) {
		if (str[j-1] == '%')
	    	    new = NO
		else
		    call error (2, "illegal format string")
	    } else {
	    	new = YES
		ndot = 0
	    }
	case '-':
	    if (new == YES)
		if (str[j-1] != '%')
		    call error (2, "illegal format string")
	case '.':
	    if (new == YES) {
		if (ndot != 0)
		    call error (2, "illegal format string")
		ndot = 1
	    }
    	case '0','1','2','3','4','5','6','7','8','9':
    	case 'e','f','g','h','m':
	    if (new == YES) {
	    	style = TY_DOUBLE
	    	fini = YES
	    }
    	case 'd','o','u','x':
	    if (new == YES) {
	    	style = TY_INT
	    	fini = YES
	    }
    	case 's':
	    if (new == YES) {
	    	style = TY_CHAR
	    	fini = YES
	    }
    	case 'b':
	    if (new == YES) {
	    	style = TY_BOOL
	    	fini = YES
	    }
    	case 'z':
	    if (new == YES) {
	    	call error (2, "complex datatype not supported")
	    }
    	case 't','w':
	    if (new == YES)
		call error (2, "illegal format string")
    	default:
	    if (new == YES)
		call error (2, "illegal format string")
    	}
	if (j - ip + 1 > max_ch)
	    call error (2, "format string too long")
    	outstr[j-ip+1] = str[j]	
	if (fini == YES)
	    break
	j = j + 1
    }
    j = j + 1
    outstr[j-ip+1] = EOS
    ip = j
    return (style)
end


