include <ctype.h>

# GET_STRDIC -- Get the string for the indexed string in a string dictionary.
# Returns the first character of the string (EOS if fewer strings than
# number requested).

int procedure get_strdic (string_dic, idx, token, length)
char	string_dic[ARB]		# String dictionary
int	idx			# The word to fetch
char	token[length]		# Output string with requested word
int	length			# Maximum length of the output string

char	delimiter
int	ndelimiter, i, start

begin
    delimiter = string_dic[1]
    ndelimiter = 1
    i = 2    
    while (ndelimiter != idx) {
	if (string_dic[i] == EOS)
	    return (EOS)
	if (string_dic[i] == delimiter)
	    ndelimiter = ndelimiter + 1
	i = i+1
    }
    start = i
    while (string_dic[i] != delimiter  && string_dic[i] != EOS &&
	   i-start < length) {
	token[i-start+1] = string_dic[i]
	i = i+1
    }
    token[i-start+1] = EOS
    return (token[1])
end

# PARG_QSTR -- Put a possibly quoted string onto the output.  If the string
# contains white space, then quote the string, escaping any embedded quotes.

procedure parg_qstr (expr)
char	expr[ARB]	# String to output

int	ip, strlen(), i
pointer	sp, buffer

begin
    ip = 1
    while (expr[ip] != EOS && ! IS_WHITE(expr[ip]))
	ip = ip + 1
    if (IS_WHITE(expr[ip])) {
	call smark (sp)
	call salloc (buffer, 2*strlen(expr), TY_CHAR)
	ip = 1
	Memc[buffer] = '"'
	i = 2
	while (expr[ip] != EOS) {
	    if (expr[ip] == '"') {
		Memc[buffer+i-1] = '\\'
		i = i+1
	    }
	    Memc[buffer+i-1] = expr[ip]
	    ip = ip + 1
	    i = i + 1
	}
	Memc[buffer+i-1] = '"'
	Memc[buffer+i] = EOS
	call pargstr (Memc[buffer])
	call sfree (sp)
    } else
	call pargstr (expr)
end
	

