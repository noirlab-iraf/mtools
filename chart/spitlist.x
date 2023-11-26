# XXXXX CHange all AW_LEAVE back to AW_CLEAR and delete these first two lines
# Also change APPEND in file open back to NEW_FILE
define	AW_LEAVE	0

include	<ctype.h>
include <chars.h>
include <gset.h>
include <pkg/gtools.h>
include <error.h>
include "database.h"
include "chart.h"

define	SZ_CONTROL	10	# Max size of control word (i.e. "HEADER")
define	SZ_BIGLINE	10*SZ_LINE	# Max size of function list

# Output file formatting definitions
define	FORMAT_WORDS	"|FORMAT|LOOP|HEADER|COUNT|"
define	FORMAT		1
define	LOOP		2
define	HEADER		3
define	COUNT		4

define	SAMPLES		"|all|marked|unmarked|"
define	ALL		1
define	MARKED		2
define	UNMARKED	3

procedure spitlist (ch, gp, gt, cmd, db, index, marker, nselected, sample)
pointer ch				# CHART pointer
pointer	gp				# GIO pointer
pointer	gt				# GTOOLS pointer
char	cmd[ARB]			# Command string
pointer	db				# DATABASE pointer
int	index[ARB]			# Selected elements index
int	marker[ARB]			# Data arrays for error listing
int	nselected			# Number of selected points
char	sample[ARB]			# all, marked, or unmarked

pointer	sp, indx, array, filename, format_file, buffer, format, control, junk
pointer	funclist, sindex, tindex
int	strlen(), getlline(), ctowrd(), open(), fd, xfd, i, j, ngood, strdic()
int	ip, len, fp, pr_flist(), ncmd, dbnfields ()
bool	strne(), streq(), dberror()

begin
    # Allocate strings
    call smark (sp)
    call salloc (filename, SZ_FNAME, TY_CHAR)
    call salloc (format_file, SZ_FNAME, TY_CHAR)
    call salloc (buffer, SZ_BIGLINE, TY_CHAR)
    call salloc (format, SZ_BIGLINE, TY_CHAR)
    call salloc (control, SZ_CONTROL, TY_CHAR)
    call salloc (funclist, SZ_BIGLINE, TY_CHAR)

    # Determine sample type
    ncmd = strdic (sample, Memc[format], SZ_BIGLINE, SAMPLES)

    # Fetch output file and format file
    ip = 1
    while (IS_WHITE(cmd[ip]))
	ip = ip + 1
    if (ctowrd (cmd, ip, Memc[filename], SZ_FNAME) == 0)
	call strcpy ("STDOUT", Memc[filename], SZ_FNAME)
    if (ctowrd (cmd, ip, Memc[format_file], SZ_FNAME) == 0)
	if (streq (Memc[CH_OUTFORMAT(ch)], ""))
	    call strcpy ("DATABASE", Memc[format_file], SZ_FNAME)
	else
	    call strcpy (Memc[CH_OUTFORMAT(ch)], Memc[format_file], SZ_FNAME)

    call salloc (indx, nselected+1, TY_INT)

    # Test the format_file
    if (strne (Memc[format_file], "DATABASE") &&
	strne (Memc[format_file], "database")) {
	iferr {
    	    xfd = open (Memc[format_file], READ_ONLY, TEXT_FILE)
	} then {
	    call erract (EA_WARN)
	    call sfree (sp)
	    return
	}
    	while (getlline (xfd, Memc[buffer], SZ_BIGLINE) != EOF) {
    	    # Skip comment lines (##).
    	    if (Memc[buffer] == '#' && Memc[buffer+1] == '#')
	    	next
	    call sscan (Memc[buffer])
	    call gargwrd (Memc[control], SZ_CONTROL)
	    i = strdic (Memc[control],Memc[control],SZ_CONTROL,FORMAT_WORDS)
	    switch (i) {
	    case FORMAT:
	    	call gargstr (Memc[format], SZ_BIGLINE)
	    	# Strip leading and trailing quotes
	    	len = strlen (Memc[format])
	    	for (fp = 1; IS_WHITE(Memc[format+fp-1]); fp = fp + 1)
		    ;
	    	if ((Memc[format+fp-1] ==DQUOTE || Memc[format+fp-1] ==SQUOTE)
		     && Memc[format+len-1] == Memc[format+fp-1]) {
		    fp = fp + 1
		    Memc[format+len-1] = EOS
	    	}
	    case COUNT:
		if (pr_flist (fd, "SEQUENCE", db, Memi[indx],
		    Memc[format+fp-1], NO) == ERR) {
		    call close (xfd)
		    call sfree (sp)
		    return
		}
	    case HEADER:
	    case LOOP:
	    	call gargstr (Memc[funclist], SZ_BIGLINE)
		if (pr_flist (fd, Memc[funclist], db, Memi[indx],
			       Memc[format+fp-1], NO) == ERR) {
		    call close (xfd)
		    call sfree (sp)
		    return
		}
	    }
	}
	call close (xfd)
    }

    # Determine relevant data
    ngood = 0
    do i = 1, nselected
	if ((marker[i] == CH_MMARK(ch) && ncmd == MARKED) ||
	    (marker[i] != CH_MMARK(ch) && ncmd == UNMARKED) ||
	    ncmd == ALL) {
	    ngood = ngood + 1
	    Memi[indx+ngood-1] = index[i]
	}
    Memi[indx+ngood] = 0

    # Sort the relevant points
    if (strne (Memc[CH_SORTER(ch)], "")) {
        call salloc (array, ngood, TY_DOUBLE)
	call eval_expr (Memc[CH_SORTER(ch)], db, Memi[indx], array, junk,
	    TY_DOUBLE, false)
	call salloc (sindex, ngood, TY_INT)
	call salloc (tindex, ngood, TY_INT)
	for (i = 1; i <= ngood; i = i + 1)
	    Memi[sindex+i-1] = i
        call sort_index (Memd[array], Memi[sindex], ngood)
	call amovi (Memi[indx], Memi[tindex], ngood)
	for (i = 1; i <= ngood; i = i + 1)
	    Memi[indx+i-1] = Memi[tindex+Memi[sindex+i-1]-1]
    }

    # Open the output file
    iferr {
    	fd = open (Memc[filename], APPEND, TEXT_FILE)
    } then {
	call erract (EA_WARN)
	call sfree (sp)
	return
    }

    # Print the list
    if (streq (Memc[format_file], "DATABASE") ||
	streq (Memc[format_file], "database")) {
	# Print out the database
	if (streq ("STDOUT", Memc[filename]))
	    call gdeactivate (gp, AW_LEAVE)
    	call header (ch, gt, fd)
	switch (ncmd) {
	case ALL:
	    call fprintf (fd, "#  %d objects in sample\n")
		call pargi (ngood)
	case MARKED:
	    call fprintf (fd, "#  %d marked objects\n")
		call pargi (ngood)
	case UNMARKED:
	    call fprintf (fd, "#  %d unmarked objects\n")
		call pargi (ngood)
	}
    	call fprintf (fd, "#\n#")
    	for (i = 1; i <= dbnfields (db); i=i+1) {
	    call fprintf (fd, "  %s")
		call parg_dbname (db, i)
	    if (dberror (db, i))
		call fprintf (fd, "  err")
    	}
    	call fprintf (fd, "\n")
    	for (i = 1; i <= ngood; i = i+1) {
  	    for (j = 1; j <= dbnfields (db); j = j+1) {
		if (j > 1)
	    	    call fprintf (fd, "  ")
		call dbfprintf (fd, db, Memi[indx+i-1], j, YES, NO)
   	    }
   	    call fprintf (fd, "\n")
   	}
    } else {
	# Print list using specified format file
	iferr {
    	    xfd = open (Memc[format_file], READ_ONLY, TEXT_FILE)
	} then {
	    call erract (EA_WARN)
	    call close (fd)
	    call sfree (sp)
	    return
	}
	if (streq ("STDOUT", Memc[filename]))
	    call gdeactivate (gp, AW_LEAVE)
    	while (getlline (xfd, Memc[buffer], SZ_BIGLINE) != EOF) {
    	    # Skip comment lines (##).
    	    if (Memc[buffer] == '#' && Memc[buffer+1] == '#')
	    	next
	    call sscan (Memc[buffer])
	    call gargwrd (Memc[control], SZ_CONTROL)
	    i = strdic (Memc[control],Memc[control],SZ_CONTROL,FORMAT_WORDS)
	    switch (i) {
	    case 0:
	    	call fprintf (fd, Memc[buffer])
	    case FORMAT:
	    	call gargstr (Memc[format], SZ_BIGLINE)
	    	# Strip leading and trailing quotes
	    	len = strlen (Memc[format])
	    	for (fp = 1; IS_WHITE(Memc[format+fp-1]); fp = fp + 1)
		    ;
	    	if ((Memc[format+fp-1] ==DQUOTE || Memc[format+fp-1] ==SQUOTE)
		     && Memc[format+len-1] == Memc[format+fp-1]) {
		    fp = fp + 1
		    Memc[format+len-1] = EOS
	    	}
	    case COUNT:
	    	call fprintf (fd, Memc[format+fp-1])
		    call pargi (ngood)
		call fprintf (fd, "\n")
	    case HEADER:
	    	call header (ch, gt, fd)
	    	switch (ncmd) {
	    	case ALL:
	    	    call fprintf (fd, "#  %d objects in sample\n")
			call pargi (ngood)
		case MARKED:
	    	    call fprintf (fd, "#  %d marked objects\n")
			call pargi (ngood)
		case UNMARKED:
	    	    call fprintf (fd, "#  %d unmarked objects\n")
			call pargi (ngood)
		}
	    case LOOP:
	    	call gargstr (Memc[funclist], SZ_BIGLINE)
		i = pr_flist (fd, Memc[funclist], db, Memi[indx],
			       Memc[format+fp-1], YES)
	    }
	}
	call close (xfd)
    }
			      
    call close (fd)
    if (streq(Memc[filename], "STDOUT"))
	call greactivate (gp, AW_PAUSE)
    call sfree (sp)
end

procedure gspitlist(ch, gp, gt, cmd, db, index, marker, nselected, sample)
pointer ch				# CHART pointer
pointer	gp				# GIO pointer
pointer	gt				# GTOOLS pointer
char	cmd[ARB]			# Command string
pointer	db				# DATABASE pointer
int	index[ARB]			# Selected elements index
int	marker[ARB]			# Data arrays for error listing
int	nselected			# Number of selected points
char	sample[ARB]			# all, marked, or unmarked

pointer	sp, indx, array, filename, format, sindex, tindex, junk
int	open(), fd, i, ngood, ctowrd(), ip, pr_flist(), ncmd, strdic()
bool	strne(), streq()

begin
    # Allocate strings
    call smark (sp)
    call salloc (filename, SZ_FNAME, TY_CHAR)
    call salloc (format, SZ_BIGLINE, TY_CHAR)

    # Determine sample type
    ncmd = strdic (sample, Memc[format], SZ_BIGLINE, SAMPLES)

    # Fetch format string
    ip = 1
    while (IS_WHITE(cmd[ip]))
	ip = ip + 1
    if (cmd[ip] == DQUOTE || cmd[ip] == SQUOTE) {
	if (ctowrd (cmd, ip, Memc[format], SZ_BIGLINE) == 0) {
            call eprintf ("Warning: Bad format string (%s)\n")
		call pargstr (cmd)
            call sfree (sp)
            return
        }
    } else
        call strcpy ("", Memc[format], SZ_BIGLINE)

    # Fetch output file and function list
    if (ctowrd (cmd, ip, Memc[filename], SZ_FNAME) == 0) {
        call eprintf ("Warning: Output file not specified (%s)\n")
	    call pargstr (cmd)
        call sfree (sp)
        return
    }
    
    call salloc (indx, nselected+1, TY_INT)

    # Test expression and format string
    if (pr_flist (fd, cmd[ip], db, Memi[indx], Memc[format], NO) == ERR) {
	call sfree (sp)
	return
    }

    # Determine relevant data
    ngood = 0
    do i = 1, nselected
	if ((marker[i] == CH_MMARK(ch) && ncmd == MARKED) ||
	    (marker[i] != CH_MMARK(ch) && ncmd == UNMARKED) ||
	    ncmd == ALL) {
	    ngood = ngood + 1
	    Memi[indx+ngood-1] = index[i]
	}
    Memi[indx+ngood] = 0

    # Sort the relevant points
    if (strne (Memc[CH_SORTER(ch)], "")) {
        call salloc (array, ngood, TY_DOUBLE)
	call eval_expr (Memc[CH_SORTER(ch)], db, Memi[indx], array, junk,
	    TY_DOUBLE, false)
	call salloc (sindex, ngood, TY_INT)
	call salloc (tindex, ngood, TY_INT)
	for (i = 1; i <= ngood; i = i + 1)
	    Memi[sindex+i-1] = i
        call sort_index (Memd[array], Memi[sindex], ngood)
	call amovi (Memi[indx], Memi[tindex], ngood)
	for (i = 1; i <= ngood; i = i + 1)
	    Memi[indx+i-1] = Memi[tindex+Memi[sindex+i-1]-1]
    }

    # Open the output file
    iferr {
    	fd = open (Memc[filename], NEW_FILE, TEXT_FILE)
    } then {
	call erract (EA_WARN)
	call sfree (sp)
	return
    }

    # Print the list
    if (streq ("STDOUT", Memc[filename]))
	call gdeactivate (gp, AW_LEAVE)
    call header (ch, gt, fd)
    switch (ncmd) {
    case ALL:
	call fprintf (fd, "#  %d objects in sample\n")
	    call pargi (ngood)
    case MARKED:
	call fprintf (fd, "#  %d marked objects\n")
	    call pargi (ngood)
    case UNMARKED:
	call fprintf (fd, "#  %d unmarked objects\n")
	    call pargi (ngood)
    }
    call fprintf (fd, "#\n# %s\n")
	call pargstr (cmd[ip])
    i = pr_flist (fd, cmd[ip], db, Memi[indx], Memc[format], YES)
			      
    call close (fd)
    if (streq(Memc[filename], "STDOUT"))
	call greactivate (gp, AW_PAUSE)
    call sfree (sp)
end

# SORT_INDEX -- Sort an array of indexes, based on the values in the array
# which the indexes point to.  Taken from Numerical Recipes.

procedure sort_index (array, indx, npts)
double	array[ARB]		# Array of values to sort by.
int	indx[ARB]		# Array of indexes for 'array'.  This is the
				# array that's actually sorted.
int	npts			# Number of points to be sorted.

int	l, j, ir, indxt, i
double	q

begin
    if (npts < 2)
	return
    l = npts / 2 + 1
    ir = npts
    repeat {
	if (l > 1) {
	    l = l-1
	    indxt = indx[l]
	    q = array[indxt]
	} else {
	    indxt = indx[ir]
	    q = array[indxt]
	    indx[ir] = indx[1]
	    ir = ir - 1
	    if (ir == 1) {
		indx[1] = indxt
		return
	    }
	}
	i = l
	j = l + l
	while (j <= ir) {
	    if (j < ir && array[indx[j]] < array[indx[j+1]])
		j = j+1
	    if (q < array[indx[j]]) {
		indx[i] = indx[j]
		i = j
		j = j + j
	    } else
		j = ir+1
	}
	indx[i] = indxt
    }
end

# HEADER -- Print a header to a file, which is a number of comment lines
# specifying the database and sample definitions.

define	SZ_TITLE	(5*SZ_LINE)

procedure header (ch, gt, fd)
pointer	ch  	# CHART pointer
pointer	gt	# GTOOLS pointer
int	fd	# Output file descriptor


pointer	sp, comment
int	len, strlen(), i
bool	strne()

begin
    call smark (sp)
    call salloc (comment, SZ_TITLE, TY_CHAR)

    call sysid (Memc[comment], SZ_TITLE)
    call fprintf (fd, "#  %s\n")
        call pargstr (Memc[comment])
    call gt_gets (gt, GTPARAMS, Memc[comment], SZ_TITLE)
    call fprintf (fd, "#  database: %s\n")
        call pargstr (Memc[comment])
    call gt_gets (gt, GTTITLE, Memc[comment], SZ_TITLE)
    len = strlen (Memc[comment])
    call fprintf (fd, "#  ")
    do i = 1, len {
	call putc (fd, Memc[comment+i-1])
	if (Memc[comment+i-1] == '\n')
    	    call fprintf (fd, "#  ")
    }
    call fprintf (fd, "\n")
    if (strne (Memc[CH_SORTER(ch)], "")) {
	call fprintf (fd, "#  sorter: %s\n")
	call pargstr (Memc[CH_SORTER(ch)])
    }
    call sfree (sp)
end
