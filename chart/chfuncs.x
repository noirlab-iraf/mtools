#include <pkg/gtools.h>
include "gtools.h"
include <error.h>
include <ctype.h>
include <chars.h>
include "chart.h"
include "cutoff.h"
include	"markers.h"
	
# CH_OPEN -- Open CHART parameter structure.

procedure ch_open (ch, db, gt)

pointer	ch		# CHART pointer
pointer	db		# DATABASE pointer
pointer	gt[CH_NGKEYS+1]	# GTOOLS pointers

int	i, clgwrd(), clgeti(), test_marker(), open_format(), test_color()
#int	open_table()
double	clgetd()
real	clgetr()
bool	clgetb(), strne(), bool_expr(), size_expr(), num_expr()
#bool	streq()
pointer	sp, buffer
int	get_keys()

begin
	call smark (sp)
	call salloc (buffer, SZ_LINE, TY_CHAR)

	# Allocate memory for the package parameter structure.
	call malloc (ch, CH_LENSTRUCT, TY_STRUCT)
	for (i = 1; i <= CH_NCUTOFFS; i = i+1)
	    call malloc (CH_CUTOFF(ch, i), SZ_LINE, TY_CHAR)
	for (i = 1; i <= CH_NMARKERS; i = i+1)
	    call malloc (CH_MARKER(ch, i), SZ_LINE, TY_CHAR)
	for (i = 1; i <= CH_NCOLORS; i = i+1)
	    call malloc (CH_COLOR(ch, i), SZ_LINE, TY_CHAR)
	call malloc (CH_XSIZE(ch), CH_SZFUNCTION, TY_CHAR)
	call malloc (CH_YSIZE(ch), CH_SZFUNCTION, TY_CHAR)
	call malloc (CH_SORTER(ch), CH_SZFUNCTION, TY_CHAR)
	call malloc (CH_DATABASE(ch),   SZ_FNAME, TY_CHAR)
	call malloc (CH_DBFORMAT(ch),   SZ_FNAME, TY_CHAR)
	call malloc (CH_OUTFORMAT(ch), SZ_FNAME, TY_CHAR)
	call malloc (CH_KEYS(ch),      SZ_FNAME, TY_CHAR)
	for (i = 1; i <= CH_NGKEYS; i = i+1) {
	    call calloc (CH_AXIS(ch, i, 1), CH_SZFUNCTION, TY_CHAR)
	    call calloc (CH_AXIS(ch, i, 2), CH_SZFUNCTION, TY_CHAR)
	    call calloc (CH_UNIT(ch, i, 1), CH_SZFUNCTION, TY_CHAR)
	    call calloc (CH_UNIT(ch, i, 2), CH_SZFUNCTION, TY_CHAR)
	    call calloc (CH_AXSIZE(ch, i, 1), CH_SZFUNCTION, TY_CHAR)
	    call calloc (CH_AXSIZE(ch, i, 2), CH_SZFUNCTION, TY_CHAR)
	    CH_DEFINED(ch, i) = false
	}

	# Read database format file
	call clgstr ("dbformat", Memc[CH_DBFORMAT(ch)], SZ_FNAME)
#	if (streq (Memc[CH_DBFORMAT(ch)], "STSDAS")) {
#	    if (open_table (db, Memc[CH_DATABASE(ch)]) == 0)
#	    	call fatal (0, "bad STSDAS table: no defined fields")
#	} else
	    if (open_format (db, Memc[CH_DBFORMAT(ch)]) == 0)
	    	call fatal (0, "bad parameter: dbformat: no defined variables")

	# Read graph keys description file
	call clgstr ("keys", Memc[CH_KEYS(ch)], SZ_FNAME)
	if (get_keys (ch, db, gt, Memc[CH_KEYS(ch)]) == 0)
	    call fatal (0, "bad parameter: keys: no defined graph keys")

	# Read remaining parameters
	do i = 1, CH_NCUTOFFS {
	    call sprintf (Memc[buffer], SZ_LINE, "cutoff%d")
		call pargi (i)
	    call clgstr (Memc[buffer], Memc[CH_CUTOFF(ch,i)], SZ_LINE)
	    if (strne ("", Memc[CH_CUTOFF(ch,i)]))
	    	if (! bool_expr (Memc[CH_CUTOFF(ch,i)], db, true)) {
		    call sprintf (Memc[buffer], SZ_LINE,
				  "bad parameter: cutoff%d")
			call pargi (i)
		    call fatal (0, Memc[buffer])
		}
	}

	CH_LOGIC(ch) = clgwrd ("logic", Memc[buffer], SZ_LINE, LOGIC_LIST)
	if (CH_LOGIC(ch) == 0)
	    call fatal (0, "bad parameter: logic")

	CH_MMARK(ch) = clgwrd ("mmarker", Memc[buffer],SZ_LINE,MARKS)
	if (CH_MMARK(ch) == 0)
	    call fatal (0, "bad parameter: mmarker")
	do i = 1, CH_NMARKERS {
	    call sprintf (Memc[buffer], SZ_LINE, "marker%d")
		call pargi (i)
	    call clgstr (Memc[buffer], Memc[CH_MARKER(ch,i)], SZ_LINE)
	    if (strne ("", Memc[CH_MARKER(ch,i)]))
	    	if (test_marker(Memc[CH_MARKER(ch,i)],db,CH_MMARK(ch))==ERR) {
		    call sprintf (Memc[buffer], SZ_LINE,
				  "bad parameter: marker%d")
			call pargi (i)
		    call fatal (0, Memc[buffer])
		}
	}
	do i = 1, CH_NCOLORS {
	    call sprintf (Memc[buffer], SZ_LINE, "color%d")
		call pargi (i)
	    call clgstr (Memc[buffer], Memc[CH_COLOR(ch,i)], SZ_LINE)
	    if (strne ("", Memc[CH_COLOR(ch,i)]))
	    	if (test_color(Memc[CH_COLOR(ch,i)],db)==ERR) {
		    call sprintf (Memc[buffer], SZ_LINE,
				  "bad parameter: color%d")
			call pargi (i)
		    call fatal (0, Memc[buffer])
		}
	}

	CH_DEFMARKER(ch) = clgwrd ("def_marker", Memc[buffer],SZ_LINE,MARKS)
	if (CH_DEFMARKER(ch) == 0)
	    call fatal (0, "bad parameter: def_marker")
	if (CH_DEFMARKER(ch) == CH_MMARK(ch))
	    call fatal (0, "bad parameter: def_mark: can't be the same as mmarker")
	call clgstr ("xsize", Memc[CH_XSIZE(ch)], CH_SZFUNCTION)
	if (strne (Memc[CH_XSIZE(ch)], ""))
	    if (! size_expr (Memc[CH_XSIZE(ch)], db, true))
	    	call fatal (0, "bad parameter: xsize")
	call clgstr ("ysize", Memc[CH_YSIZE(ch)], CH_SZFUNCTION)
	if (strne (Memc[CH_YSIZE(ch)], ""))
	    if (! size_expr (Memc[CH_YSIZE(ch)], db, true))
	    	call fatal (0, "bad parameter: ysize")
	call clgstr ("sorter", Memc[CH_SORTER(ch)], CH_SZFUNCTION)
	if (strne (Memc[CH_SORTER(ch)], ""))
	    if (! num_expr (Memc[CH_SORTER(ch)], db, true))
		call fatal (0, "bad parameter: sorter")
	CH_FIELD(ch) = clgwrd ("field", Memc[buffer], SZ_LINE, FIELD_LIST)
	if (CH_FIELD(ch) == 0)
	    call fatal (0, "bad parameter: field")
	CH_XCENTER(ch) = clgetd ("xcenter")
	CH_YCENTER(ch) = clgetd ("ycenter")
	CH_RADIUS(ch)  = clgetd ("radius")
	CH_MAXSIZE(ch) = clgetr ("max_size")
	CH_MINSIZE(ch) = clgetr ("min_size")
	CH_PLOTARROWS(ch) = clgetb ("outliers")
	CH_NBINS(ch)   = clgeti ("nbins")
	CH_Z1(ch)      = clgetd ("z1")
	CH_Z2(ch)      = clgetd ("z2")
	CH_AUTOSCALE(ch)=clgetb ("autoscale")
	CH_TOPCLOSED(ch)=clgetb ("top_closed")
	CH_PLOTTYPE(ch) = clgwrd ("plot_type", Memc[buffer],SZ_LINE,HGM_TYPES)
	if (CH_PLOTTYPE(ch) == 0)
	    call fatal (0, "bad parameter: plot_type")
	CH_LOG(ch)     = clgetb ("log")
	call clgstr ("outformat", Memc[CH_OUTFORMAT(ch)], SZ_FNAME)
	call sfree (sp)
end

# GET_KEYS -- Read graph keys description file.  Returns the numer of keys
# defined (5 max).

define SZ_BIGLINE	(10*SZ_LINE)

int procedure get_keys (ch, db, gt, filename)

pointer	ch
pointer	db
pointer	gt[CH_NGKEYS+1]
char	filename[SZ_FNAME]

int	i, fd, ip, open(), getlline(), j, parse_key()
pointer	sp, buffer

begin
	call smark (sp)
	call salloc (buffer, SZ_BIGLINE, TY_CHAR)
	iferr {
	    fd = open (filename, READ_ONLY, TEXT_FILE)
	} then {
	    call erract (EA_WARN)
	    return (0)
	}
	i = 0
	repeat {
	    # Prompt if reading STDIN
	    if (fd == STDIN) {
	    	call printf ("Graph key description: ")
	    	call flush (STDOUT)
	    }
	    # Get line
	    if (getlline (fd, Memc[buffer], SZ_BIGLINE) == EOF)
		break

	    # Skip comment lines and blank lines.
	    if (Memc[buffer] == '#')
		next
	    for (ip=1;  IS_WHITE(Memc[buffer+ip-1]);  ip=ip+1)
		;
	    if (Memc[buffer+ip-1] == '\n' || Memc[buffer+ip-1] == EOS)
		next

	    # Read key
	    i = i + 1
	    if (i > CH_NGKEYS) {
	    	call eprintf ("Warning: Too many graph keys defined (%s)\n")
		    call pargstr (filename)
		i = CH_NGKEYS
		break
	    }
	    if (parse_key (Memc[buffer+ip-1], db,
	         Memc[CH_AXIS(ch,i,1)], Memc[CH_AXIS(ch,i,2)],
		 Memc[CH_UNIT(ch,i,1)], Memc[CH_UNIT(ch,i, 2)],
		 Memc[CH_AXSIZE(ch,i,1)],Memc[CH_AXSIZE(ch,i,2)],
		 CH_SQUARE(ch,i),
		 CH_FLIP(ch,i,1), CH_FLIP(ch,i,2),
		 GT_XMIN(gt[i]), GT_XMAX(gt[i]),
		 GT_YMIN(gt[i]), GT_YMAX(gt[i]),
		 CH_ERROK(ch,i,1), CH_ERROK(ch,i,2)) == ERR) {
		i = i - 1
		next
	    }
	    CH_DEFINED(ch, i) = true
	    call gt_sets (gt[i], GTXLABEL, Memc[CH_AXIS(ch, i, 1)])
	    call gt_sets (gt[i], GTYLABEL, Memc[CH_AXIS(ch, i, 2)])
	    call gt_sets (gt[i], GTXUNITS, Memc[CH_UNIT(ch, i, 1)])
	    call gt_sets (gt[i], GTYUNITS, Memc[CH_UNIT(ch, i, 2)])
	}
	for (j = i+1; j <= CH_NGKEYS; j = j + 1)
	    CH_DEFINED(ch, j) = false
	call close (fd)
	call sfree (sp)
	return (i)
end

# PARSE_KEY -- Parse a graph key description string.  Returns ERR if an error
# occurred.

define	SZ_KEYFLAG	10

define	KEY_FLAGS   "|SQUARE|XFLIP|YFLIP|LIMITS|XUNITS|YUNITS|XSIZE|YSIZE|"

define	KEY_SQUARE	1
define	KEY_XFLIP	2
define	KEY_YFLIP	3
define	KEY_LIMITS	4
define	KEY_XUNITS	5
define	KEY_YUNITS	6
define	KEY_XSIZE	7
define	KEY_YSIZE	8

int procedure parse_key (buffer, db, xfunc, yfunc, xunits, yunits, xsize,
			 ysize, square, xflip, yflip, xmin, xmax, ymin, ymax,
			 xerrOk, yerrOk)
char	buffer[ARB]
pointer	db
char	xfunc[CH_SZFUNCTION]
char	yfunc[CH_SZFUNCTION]
char    xunits[CH_SZFUNCTION]
char    yunits[CH_SZFUNCTION]
char    xsize[CH_SZFUNCTION]
char    ysize[CH_SZFUNCTION]
char	tmp[CH_SZFUNCTION]
bool    square
bool    xflip
bool    yflip
real    xmin
real    xmax
real    ymin
real    ymax
bool	xerrOk
bool	yerrOk

int	i, nscan(), key, strdic(), strlen()
char	keyflag[SZ_KEYFLAG]
bool	num_expr(), size_expr(), errsize()

begin
    # Get axis functions
    call sscan (buffer)
    call gargwrd (xfunc, CH_SZFUNCTION)
    call gargwrd (yfunc, CH_SZFUNCTION)
    if (nscan() != 2) {
	call eprintf ("Warning: Underspecified graph key -- %s")
	    call pargstr (buffer)
	return (ERR)
    }
    if (! num_expr (xfunc, db, true))
	return (ERR)
    if (! num_expr (yfunc, db, true))
	return (ERR)

    # Set defaults
    square = false
    xflip = false
    yflip = false
    xmin = INDEF
    xmax = INDEF
    ymin = INDEF
    ymax = INDEF
    call strcpy ("", xunits, CH_SZFUNCTION)
    call strcpy ("", yunits, CH_SZFUNCTION)
    call strcpy ("", xsize, CH_SZFUNCTION)
    call strcpy ("", ysize, CH_SZFUNCTION)


    # Parse additional flags
    i = nscan()
    call gargwrd (keyflag, SZ_KEYFLAG)
    while (nscan() == i + 1) {
	call strupr (keyflag)
	key = strdic (keyflag, keyflag, SZ_KEYFLAG, KEY_FLAGS)
	switch (key) {
	case KEY_SQUARE:
	    square = true
	case KEY_XFLIP:
	    xflip = true
	case KEY_YFLIP:
	    yflip = true
	case KEY_LIMITS:
	    call gargr (xmin)
	    call gargr (xmax)
	    call gargr (ymin)
	    call gargr (ymax)
	    if (nscan() != i+5) {
		call eprintf ("Warning: Bad limits for graph key -- %s")
		    call pargstr (buffer)
		return (ERR)
	    }
	case KEY_XUNITS:
	    call gargwrd (xunits, CH_SZFUNCTION)
	    if (nscan() != i+2) {
		call eprintf ("Warning: Bad x-axis unit for graph key -- %s")
		    call pargstr (buffer)
		return (ERR)
	    }
	case KEY_YUNITS:
	    call gargwrd (yunits, CH_SZFUNCTION)
	    if (nscan() != i+2) {
		call eprintf ("Warning: Bad y-axis unit for graph key -- %s")
		    call pargstr (buffer)
		return (ERR)
	    }
	case KEY_XSIZE:
	    call gargwrd (xsize, CH_SZFUNCTION)
	    if (nscan() != i+2) {
		call eprintf (
			"Warning: Bad x-axis graph key size function -- %s")
		    call pargstr (buffer)
		return (ERR)
	    }
	    if (errsize(xsize)) {
		call sprintf(tmp, CH_SZFUNCTION, "err(%s)")
		    call pargstr (xfunc)
		if (! size_expr (tmp, db, true))
		    return ERR
	    } else if (! size_expr (xsize, db, true))
		return (ERR)
	case KEY_YSIZE:
	    call gargwrd (ysize, CH_SZFUNCTION)
	    if (nscan() != i+2) {
		call eprintf (
			"Warning: Bad y-axis graph key size function -- %s")
		    call pargstr (buffer)
		return (ERR)
	    }
	    if (errsize(ysize)) {
		call sprintf(tmp, CH_SZFUNCTION, "err(%s)")
		    call pargstr (yfunc)
		if (! size_expr (tmp, db, true))
		    return ERR
	    } else if (! size_expr (ysize, db, true))
		return (ERR)
	default:
	    call eprintf ("Warning: Unrecognized flag for graph key -- %s")
		call pargstr (buffer)
	    return (ERR)
	}
	i = nscan()
	call gargwrd (keyflag, SZ_KEYFLAG)
    }

    # If no X or Y axis size function was specified, check whether an ERR
    # function would be legal
    if (strlen(xsize) == 0) {
	call sprintf(tmp, CH_SZFUNCTION, "err(%s)")
	    call pargstr (xfunc)
	if (size_expr (tmp, db, false))
	    xerrOk = true
	else
	    xerrOk = false
    } else {
	xerrOk = true
    }
    if (strlen(ysize) == 0) {
	call sprintf(tmp, CH_SZFUNCTION, "err(%s)")
	    call pargstr (yfunc)
	if (size_expr (tmp, db, false))
	    yerrOk = true
	else
	    yerrOk = false
    } else {
	yerrOk = true
    }
    return (OK)
end

# PUT_KEYS -- Write current graph keys definitions to graph keys description
# file.

procedure put_keys (ch, gt, filename)

pointer	ch
pointer	gt[CH_NGKEYS+1]
char	filename[SZ_FNAME]

int	i, fd, open(), strlen()

begin
	iferr {
	    fd = open (filename, NEW_FILE, TEXT_FILE)
	} then {
	    call erract (EA_WARN)
	    return
	}
	call fprintf (fd, "# CHART graph key definitions for %s.\n")
	    call pargstr (Memc[CH_DATABASE(ch)])
	for (i = 1; i <= CH_NGKEYS; i =i+1) {
	    if (! CH_DEFINED(ch, i)) {
		if (fd == STDOUT)
		    call fprintf (fd, "# Undefined\n")
		next
	    }
	    call fprintf (fd, "%s  %s")
		call parg_qstr (Memc[CH_AXIS(ch, i, 1)])
		call parg_qstr (Memc[CH_AXIS(ch, i, 2)])
	    if (strlen (Memc[CH_UNIT(ch, i, 1)]) > 0) {
		call fprintf (fd, "  xunits  %s")
		call parg_qstr (Memc[CH_UNIT(ch, i, 1)])
	    }
	    if (strlen (Memc[CH_UNIT(ch, i, 2)]) > 0) {
		call fprintf (fd, "  yunits  %s")
		call parg_qstr (Memc[CH_UNIT(ch, i, 2)])
	    }
	    if (strlen (Memc[CH_AXSIZE(ch, i, 1)]) > 0) {
		call fprintf (fd, "  xsize  %s")
		call parg_qstr (Memc[CH_AXSIZE(ch, i, 1)])
	    }
	    if (strlen (Memc[CH_AXSIZE(ch, i, 2)]) > 0) {
		call fprintf (fd, "  ysize  %s")
		call parg_qstr (Memc[CH_AXSIZE(ch, i, 2)])
	    }
	    if (! IS_INDEF(GT_XMIN(gt[i])) || ! IS_INDEF(GT_XMAX(gt[i])) ||
		! IS_INDEF(GT_YMIN(gt[i])) || ! IS_INDEF(GT_YMAX(gt[i]))) {
		call fprintf (fd, "  limits  %g  %g  %g  %g")
		call pargr (GT_XMIN(gt[i]))
		call pargr (GT_XMAX(gt[i]))
		call pargr (GT_YMIN(gt[i]))
		call pargr (GT_YMAX(gt[i]))
	    }
	    if (CH_FLIP(ch, i, 1))
		call fprintf (fd, "  xflip")
	    if (CH_FLIP(ch, i, 2))
		call fprintf (fd, "  yflip")
	    if (CH_SQUARE(ch, i))
		call fprintf (fd, "  square")
	    call fprintf (fd, "\n")
	}
	call close (fd)
end


# CH_CLOSE -- Close CHART parameter structure.

procedure ch_close (ch)
pointer	ch		# CHART pointer

int	i

begin
    if (ch != NULL) {
	# Free memory for the package parameter structure.
	for (i = 1; i <= CH_NCUTOFFS; i = i+1)
	    call mfree (CH_CUTOFF(ch, i), TY_CHAR)
	for (i = 1; i <= CH_NMARKERS; i = i+1)
	    call mfree (CH_MARKER(ch, i), TY_CHAR)
	for (i = 1; i <= CH_NCOLORS; i = i+1)
	    call mfree (CH_COLOR(ch, i), TY_CHAR)
	for (i = 1; i <= CH_NGKEYS; i = i+1) {
	    call mfree (CH_AXIS(ch, i, 1), TY_CHAR)
	    call mfree (CH_AXIS(ch, i, 2), TY_CHAR)
	    call mfree (CH_UNIT(ch, i, 1), TY_CHAR)
	    call mfree (CH_UNIT(ch, i, 2), TY_CHAR)
	    call mfree (CH_AXSIZE(ch, i, 1), TY_CHAR)
	    call mfree (CH_AXSIZE(ch, i, 2), TY_CHAR)
	}
	call mfree (CH_XSIZE(ch), TY_CHAR)
	call mfree (CH_YSIZE(ch), TY_CHAR)
	call mfree (CH_SORTER(ch), TY_CHAR)
	call mfree (CH_KEYS(ch), TY_CHAR)
	call mfree (CH_DATABASE(ch), TY_CHAR)
	call mfree (CH_DBFORMAT(ch), TY_CHAR)
	call mfree (CH_OUTFORMAT(ch), TY_CHAR)
	call mfree (ch, TY_STRUCT)
	}
end

# CH_SHOW -- Show the values of the selection parameters.

procedure ch_show (ch, file)
pointer	ch		# CHART pointer
char	file[ARB]	# output file

int	i, fd, open(), get_strdic()
pointer	sp, temp
begin
    iferr {
    	fd = open (file, NEW_FILE, TEXT_FILE)
    } then {
	call erract (EA_WARN)
	return
    }

    call smark (sp)
    call salloc (temp, SZ_LINE, TY_CHAR)

    do i = 1, CH_NCUTOFFS {
	call fprintf (fd, "     cutoff%d = %s\n")
	    call pargi (i)
	    call pargstr (Memc[CH_CUTOFF(ch, i)])
    }
    i = get_strdic (LOGIC_LIST, CH_LOGIC(ch), Memc[temp], SZ_LINE)
    call fprintf (fd, "       logic = %s\n")
	call pargstr (Memc[temp])
    do i = 1, CH_NMARKERS {
	call fprintf (fd, "     marker%d = %s\n")
	    call pargi (i)
	    call pargstr (Memc[CH_MARKER(ch, i)])
    }
    do i = 1, CH_NCOLORS {
	call fprintf (fd, "     color%d = %s\n")
	    call pargi (i)
	    call pargstr (Memc[CH_COLOR(ch, i)])
    }
    i = get_strdic (MARKS, CH_DEFMARKER(ch), Memc[temp], SZ_LINE)
    call fprintf (fd, "  def_marker = %s\n")
	call pargstr (Memc[temp])
    i = get_strdic (MARKS, CH_MMARK(ch), Memc[temp], SZ_LINE)
    call fprintf (fd, "     mmarker = %s\n")
	call pargstr (Memc[temp])
    call fprintf (fd, "       xsize = %s\n")
	call pargstr (Memc[CH_XSIZE(ch)])
    call fprintf (fd, "       ysize = %s\n")
	call pargstr (Memc[CH_YSIZE(ch)])
    call fprintf (fd, "      sorter = %s\n")
	call pargstr (Memc[CH_SORTER(ch)])
    i = get_strdic (FIELD_LIST, CH_FIELD(ch), Memc[temp], SZ_LINE)
    call fprintf (fd, "       field = %s\n")
	call pargstr (Memc[temp])
    call fprintf (fd, "     xcenter = %g\n")
	call pargd (CH_XCENTER(ch))
    call fprintf (fd, "     ycenter = %g\n")
	call pargd (CH_YCENTER(ch))
    call fprintf (fd, "      radius = %g\n")
	call pargd (CH_RADIUS(ch))
    call fprintf (fd, "    max_size = %g\n")
	call pargr (CH_MAXSIZE(ch))
    call fprintf (fd, "    min_size = %g\n")
	call pargr (CH_MINSIZE(ch))
    call fprintf (fd, "    dbformat = %s\n")
	call pargstr (Memc[CH_DBFORMAT(ch)])
    call fprintf (fd, "   outformat = %s\n")
	call pargstr (Memc[CH_OUTFORMAT(ch)])
    call fprintf (fd, "    outliers = %b\n")
	call pargb (CH_PLOTARROWS(ch))
    call fprintf (fd, "\n")
    call fprintf (fd, "       nbins = %d\n")
	call pargi (CH_NBINS(ch))
    call fprintf (fd, "          z1 = %g\n")
	call pargd (CH_Z1(ch))
    call fprintf (fd, "          z2 = %g\n")
	call pargd (CH_Z2(ch))
    call fprintf (fd, "   autoscale = %b\n")
	call pargb (CH_AUTOSCALE(ch))
    call fprintf (fd, "  top_closed = %b\n")
	call pargb (CH_TOPCLOSED(ch))
    if (CH_PLOTTYPE(ch) == HGM_LINE)
        call fprintf (fd, "   plot_type = line\n")
    else
        call fprintf (fd, "   plot_type = box\n")
    call close (fd)
    call sfree (sp)
end
