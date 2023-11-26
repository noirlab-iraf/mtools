# Formerly gio$stdgraph/t_gkideco.x

task	gki2mng

# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<gki.h>

# GKI2IGI -- Convert the contents of one or more metacode files to MONGO input.

procedure gki2mng()

int	fd, list
pointer	gki, sp, fname
int	dd[LEN_GKIDD]

int	clpopni(), clgfil(), clplen(), open()
int	gki_fetch_next_instruction()

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	# Open list of metafiles to be decoded.
	list = clpopni ("input")

	# Set up the decoding graphics kernel.
	call gkg_install (dd, STDOUT)

	# Process a list of metacode files, writing the decoded metacode
	# instructions on the standard output.

	while (clgfil (list, Memc[fname], SZ_FNAME) != EOF) {
	    # Print header if new file.
#	    if (clplen (list) > 1) {
#		call printf ("\n# METAFILE '%s':\n")
#		    call pargstr (Memc[fname])
#	    }

	    # Open input file.
	    iferr (fd = open (Memc[fname], READ_ONLY, BINARY_FILE)) {
		call erract (EA_WARN)
		next
	    } else
		call gkg_grstream (fd)

	    # Process the metacode.
	    while (gki_fetch_next_instruction (fd, gki) != EOF)
		call gki_execute (Mems[gki], dd)

	    call close (fd)
	}

	call clpcls (list)
	call sfree (sp)
end
