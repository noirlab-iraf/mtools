# LINELIST -- Read coordinate line list.

include <ctype.h>

procedure linelist (coordlist, nfeatures, fname, frest, ftype, max_features)

char	coordlist[SZ_FNAME]	# coordinate list file
int	nfeatures		# number of features (both on input and output)
char	fname[SZ_FNAME, ARB]	# name of feature
real	frest[ARB]		# rest wavelength of feature
int	ftype[ARB]		# type of feature (emission/absorption)
int	max_features		# maximum number of features allowed


int	open(), fscan(), nscan(), fd, len, i, j, strlen()
bool	streq()
char	buffer[SZ_LINE]
errchk	open

begin
	# Read in coordinate list
	if (streq (coordlist, ""))
	    return
	iferr (fd = open(coordlist, READ_ONLY, TEXT_FILE)) {
	    call eprintf ("coordinate list file (%s) not found\n")
	    call pargstr (coordlist)
	    return
	}
	while (fscan(fd) != EOF) {
	    nfeatures = nfeatures + 1
	    call gargr (frest[nfeatures])
	    call gargi (ftype[nfeatures])
	    call gargstr (buffer, SZ_FNAME)
	    len = strlen (buffer)
	    for (i = 1; buffer[i] != EOS; i = i+1)
		if (! IS_WHITE(buffer[i]))
		    break
	    do j = i, len+1
		fname[j-i+1,nfeatures] = buffer[j]
	    if (nscan() != 3)
		nfeatures = nfeatures - 1
	    if (nfeatures == max_features) {
		call eprintf ("maximum number of features read from %s\n")
		    call pargstr (coordlist)
		break
	    }
	}
	call close (fd)
	return
end
