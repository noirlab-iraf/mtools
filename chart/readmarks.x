include <gset.h>
include <ctype.h>
include <error.h>

# READ_MARKS -- Read a file of serial numbers to be marked.  The first word in
# each line of the file is read, and if it is a serial number, that object is
# marked.  If no file is specified, then the user is prompted for serial
# numbers until she/he responds with a <cr>.

procedure read_marks (gp, db, index, marker, fname, marked)
pointer	gp		# GIO pointer
pointer	db		# DATABASE pointer
int	index[ARB]	# Selected elements index
int	marker[ARB]	# Markers
char	fname[SZ_FNAME]	# Input file
int	marked		# Marker type for marked objects

pointer	buffer, sp
int	ikey, garg_key(), fd, getline(), open(), ip, interactive
int	missing
bool	streq()

begin
    if (streq(fname, "")) {
	call strcpy ("STDIN", fname, SZ_FNAME)
	interactive = YES
    } else
	interactive = NO

    iferr {
	fd = open (fname, READ_ONLY, TEXT_FILE)
    } then {
	call erract (EA_WARN)
	return
    }

    if (interactive == YES) {
	call printf ("Search key: ")
	call flush (STDOUT)
    }

    missing = NO
    call smark (sp)
    call salloc (buffer, SZ_LINE, TY_CHAR)
    while (getline (fd, Memc[buffer]) != EOF) {
        # Skip comment lines and blank lines.
	if ((Memc[buffer] == '\n' || Memc[buffer] == EOS) && interactive==YES)
	    break
        if (Memc[buffer] == '#')
	    next
        for (ip=1;  IS_WHITE(Memc[buffer+ip-1]);  ip=ip+1)
	     ;
        if (Memc[buffer+ip-1] == '\n' || Memc[buffer+ip-1] == EOS)
	     next

	call sscan (Memc[buffer])
	ikey = garg_key (db, index)
	if (ikey == 0) {
	    if (interactive == NO && missing == NO) {
		call gdeactivate (gp, AW_CLEAR)
		missing = YES
	    }
	    call reset_scan()
	    call gargwrd (Memc[buffer], SZ_LINE)
	    if (interactive == YES)
	        call printf ("Search key not found (%s) --- ")
	    else
	    	call eprintf ("Search key not found (%s)\n")
	    call pargstr (Memc[buffer])
	} else
	    marker[ikey] = marked

	if (interactive == YES) {
	    call printf ("Search key: ")
	    call flush (STDOUT)
	}
    }
    if (interactive == NO && missing == YES)
	call greactivate (gp, AW_PAUSE)
    call close (fd)
    call sfree (sp)
end
