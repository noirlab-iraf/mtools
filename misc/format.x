task	format

# FORMAT -- Format STDIN and pass it on to STDOUT.

procedure format ()

char	fstring[SZ_LINE], cval, sval[SZ_LINE]
int	i, len, strlen(), ival, scan()
double	dval
bool	bval

begin
	# Get the format string
	call clgstr ("fstring", fstring, SZ_LINE)

	# Print the format string
	call printf (fstring)

	# Now read each token from STDIN and pass it to STDOUT
	len = strlen (fstring)
	i = scan()
	for (i = 1; i <= len; i = i + 1) {
	    if (fstring[i] == '%') {
		repeat {
		    i = i + 1
	    	    switch (fstring[i]) {
	    	    case 'b':
		    	call gargb (bval)
			call pargb (bval)
			break
		    case 'c':
			call gargc (cval)
			call pargc (cval)
			break
		    case 'd', 'o', 'r', 'u', 'x':
			call gargi (ival)
			call pargi (ival)
			break
		    case 'e', 'f', 'g', 'h', 'm':
			call gargd (dval)
			call pargd (dval)
			break
		    case 's':
			call gargwrd (sval, SZ_LINE)
			call pargstr (sval)
			break
		    case 'z':
			call error (0, "Complex format not supported")
		    default:
		    }
	    	}
	    }
	}
end
