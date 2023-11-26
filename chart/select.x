include	<pkg/gtools.h>
include <mach.h>
include <gio.h>
include <ctype.h>
include "chart.h"
include	"cutoff.h"
include "markers.h"

define	LEN_TITLE	72
define	SZ_TITLE	(5*SZ_LINE)

# SELECT -- Select objects of interest from the database and mark them
# appropriately.  Returns the number of selected points.

int procedure select (gt, ch, db, npts, index, marker, color)
pointer	gt[CH_NGKEYS+1]	# GTOOLS pointer
pointer	ch		# CHART pointer
pointer	db		# DATABASE pointer
int	npts		# Number of entries in database
int	index[ARB]	# Good element index
int	marker[ARB]	# Marker array
int	color[ARB]	# Color array

int	i, cut, strlen(), marktype, colortype, strdic(), j, nselected, len, k
int	get_strdic()
pointer	sp, result, title, smarker, scolor, equation, result2, junk
bool	strne()

begin
    call smark (sp)
    call salloc (result, npts, TY_BOOL)
    call salloc (result2, npts, TY_BOOL)
    call salloc (title, SZ_TITLE, TY_CHAR)
    call salloc (smarker, SZ_MARKERSTRING, TY_CHAR)
    call salloc (scolor, SZ_COLORSTRING, TY_CHAR)
    call salloc (equation, SZ_LINE, TY_CHAR)

    # Intialize index
    do i = 1, npts
	index[i] = i
    index[npts+1] = 0

    # Initialize result2 for OR
    if (CH_LOGIC(ch) == OR)
	do i = 1, npts
	    Memb[result2+i-1] = false

    # Apply "cutoffX" parameters
    call strcpy ("", Memc[title], SZ_TITLE)
    len = 0
    cut = NO
    for (i = 1; i <= CH_NCUTOFFS; i = i+1) {
	if (strne (Memc[CH_CUTOFF(ch, i)], "")) {
	    cut = YES
	    call eval_expr (Memc[CH_CUTOFF(ch, i)], db, index, result, junk,
			    TY_BOOL, false)
	    switch (CH_LOGIC(ch)) {
	    case AND:
	    	j = 0
	    	for (k = 1; index[k] > 0; k = k + 1)
		    if (Memb[result+k-1]) {
		    	j = j + 1
		    	index[j] = index[k]
	    	    }
	    	index[j+1] = 0
	    case OR:
		do j = 1, npts
		    Memb[result2+j-1] = Memb[result2+j-1] || Memb[result+j-1]
	    }
	    # Write title
	    if (len > 0 && len+strlen(Memc[CH_CUTOFF(ch,i)])+5 > LEN_TITLE) {
		call strcat ("\n", Memc[title], SZ_TITLE)
		len = 0
	    }
	    if (strlen(Memc[title]) > 0) {
		if (len > 0) {
		    call strcat (" ", Memc[title], SZ_TITLE)
		    len = len + 1
		}
		switch (CH_LOGIC(ch)) {
		case AND:
		    call strcat ("AND ", Memc[title], SZ_TITLE)
		    len = len + 4
		case OR:
		    call strcat ("OR ", Memc[title], SZ_TITLE)
		    len = len + 3
		}
	    }
	    call strcat (Memc[CH_CUTOFF(ch, i)], Memc[title], SZ_TITLE)
	    len = len + strlen(Memc[CH_CUTOFF(ch, i)])
	}
    }
    if (CH_LOGIC(ch) == OR && cut == YES) {
	j = 0
	do i = 1, npts
	    if (Memb[result2+i-1]) {
		j = j + 1
		index[j] = i
	    }
	index[j+1] = 0
    }

    # Apply field limits
    # Determine number of objects in sample before field cutoff
    for (i = 1; index[i] > 0; i = i + 1)
	;
    nselected = i - 1
    call cut_field (ch, db, index, nselected, Memc[title], SZ_TITLE)
    do i = 1, CH_NGKEYS+1
    	call gt_sets (gt[i], GTTITLE, Memc[title])

    # Determine final number of objects in sample
    for (i = 1; index[i] > 0; i = i + 1)
	;
    nselected = i - 1

    # Set selected objects to default marker type, color white
    call amovki (CH_DEFMARKER(ch), marker, nselected)
    call amovki (1, color, nselected)

    # Apply "markerX" parameters to set appropriate marker types
    len = 0
    call strcpy ("", Memc[title], SZ_TITLE)
    for (i = 1; i <= CH_NMARKERS; i = i+1) {
	if (strne (Memc[CH_MARKER(ch, i)], "")) {
	    call sscan (Memc[CH_MARKER(ch, i)])
	    call gargwrd (Memc[smarker], SZ_MARKERSTRING)
	    marktype = strdic (Memc[smarker], Memc[smarker], SZ_MARKERSTRING,
			       MARKS)
	    call gargstr (Memc[equation], SZ_LINE)
	    call eval_expr (Memc[equation], db, index, result, junk, TY_BOOL,
			    false)
	    for (j = 1; j <= nselected; j = j+1)
		if (Memb[result+j-1])
		    marker[j] = marktype
	    # Write title
	    if (len > 0 && len+strlen(Memc[smarker])+strlen(Memc[equation])+6 > LEN_TITLE) {
		call strcat ("\n", Memc[title], SZ_TITLE)
		len = 0
	    }
	    if (len > 0) {
	    	call strcat ("    ", Memc[title], SZ_TITLE)
		len = len + 4
	    }
	    call strcat (Memc[smarker], Memc[title], SZ_TITLE)
	    call strcat (": ", Memc[title], SZ_TITLE)
	    call strcat (Memc[equation], Memc[title], SZ_TITLE)
	    len = len + strlen(Memc[smarker])+strlen(Memc[equation])+3
	}
    }

    # Add default marker to title string
    if (strlen (Memc[title]) > 0) {
	i = get_strdic (MARKS, CH_DEFMARKER(ch), Memc[smarker],
			SZ_MARKERSTRING)
	if (len > 0 && len+strlen(Memc[smarker])+20 > LEN_TITLE) {
	    call strcat ("\n", Memc[title], SZ_TITLE)
	    len = 0
	}
	if (len > 0) {
	    call strcat ("    ", Memc[title], SZ_TITLE)
	    len = len + 4
	}
	call strcat (Memc[smarker], Memc[title], SZ_TITLE)
	call strcat (": default", Memc[title], SZ_TITLE)
	len = len + strlen(Memc[smarker])+10
    }

    # Apply "colorX" parameters to set appropriate marker colors
    for (i = 1; i <= CH_NCOLORS; i = i+1) {
	if (strne (Memc[CH_COLOR(ch, i)], "")) {
	    call sscan (Memc[CH_COLOR(ch, i)])
	    call gargwrd (Memc[scolor], SZ_COLORSTRING)
	    colortype = strdic (Memc[scolor], Memc[scolor], SZ_COLORSTRING,
			        COLORS) - 1
	    call gargstr (Memc[equation], SZ_LINE)
	    call eval_expr (Memc[equation], db, index, result, junk, TY_BOOL,
			    false)
	    for (j = 1; j <= nselected; j = j+1)
		if (Memb[result+j-1])
		    color[j] = colortype
	    # Write title
	    if (len > 0 && len+strlen(Memc[scolor])+strlen(Memc[equation])+6 > LEN_TITLE) {
		call strcat ("\n", Memc[title], SZ_TITLE)
		len = 0
	    }
	    if (len > 0) {
	    	call strcat ("    ", Memc[title], SZ_TITLE)
		len = len + 4
	    }
	    call strcat (Memc[scolor], Memc[title], SZ_TITLE)
	    call strcat (": ", Memc[title], SZ_TITLE)
	    call strcat (Memc[equation], Memc[title], SZ_TITLE)
	    len = len + strlen(Memc[scolor])+strlen(Memc[equation])+3
	}
    }

    # Set title
    do i = 1, CH_NGKEYS
    	call gt_sets (gt[i], GTSUBTITLE, Memc[title])
    call sfree (sp)
    CH_NSELECTED(ch) = nselected
    return (nselected)
end

# SIZE_UP -- Calculate the marker size for each database entry

define	S_BLANK		1	# Blank expression
define	S_SAME		2	# Expression == "SAME"
define	S_SCALED_POS	3	# Poitively scaled
define	S_SCALED_NEG	4	# Negatively scaled
define	S_VALUE		5	# Not scaled
define	S_ERROR		6	# Expression == "error" -- error bars

procedure size_up (ch, db, gp, index, gt, xfunc, yfunc,sizex, sizey, nselected)
pointer	ch
pointer	db
pointer gp
int	index
pointer	gt
char	xfunc[ARB]
char	yfunc[ARB]
real	sizex[ARB]
real	sizey[ARB]
int	nselected

pointer	sp, comments
int	size_axis()
int	xcode, ycode, ipx, ipy

begin
    call smark (sp)
    call salloc (comments, SZ_LINE, TY_CHAR)

    xcode = size_axis (xfunc, ch, db, index, sizex, nselected, ipx)
    ycode = size_axis (yfunc, ch, db, index, sizey, nselected, ipy)

    call strcpy ("", Memc[comments], SZ_LINE)
    if (xcode == S_BLANK && ycode == S_BLANK) {
	# Nothing needs doing
    } else if (xcode == S_SAME && ycode == S_SAME) {
	call amovkr (MSIZE, sizex, nselected)
	call amovkr (MSIZE, sizey, nselected)
    } else if (xcode == S_SAME) {
	call amovr (sizey, sizex, nselected)
	if (ycode == S_ERROR)
	    call size_title (ycode, "size", "y-axis error", Memc[comments],
			     SZ_LINE)
	else
	    call size_title (ycode, "size", yfunc[ipy], Memc[comments],SZ_LINE)
    } else if (ycode == S_SAME) {
	call amovr (sizex, sizey, nselected)
	if (xcode == S_ERROR)
	    call size_title (xcode, "size", "x-axis error", Memc[comments],
			     SZ_LINE)
	else
	    call size_title (xcode, "size", yfunc[ipy], Memc[comments],SZ_LINE)
    } else {
	call size_title (xcode, "xsize", xfunc[ipx], Memc[comments], SZ_LINE)
	call size_title (ycode, "ysize", yfunc[ipy], Memc[comments], SZ_LINE)
    }
    call gt_sets (gt, GTCOMMENTS, Memc[comments])
    call sfree (sp)
end	

# SIZE_AXIS -- Calculate the marker size for each database entry for one axis

define	SMALLMARK   0.005   # Size of marker with INDEF sizing function

int procedure size_axis (expr, ch, db, index, size, nselected, ip)
char	expr[ARB]
pointer	ch
pointer	db
int	index
real	size[ARB]
int	nselected
int	ip

pointer	sp, pout, junk
real	minsize, maxsize, value, ratio
int	i, start
bool	scaled, streq(), errsize()

begin
	ip = 1

	# Blank expression?
	if (streq (expr, "")) {
	    call amovkr (MSIZE, size, nselected)
	    return (S_BLANK)
	}

	# Same as other axis?
	if (streq (expr, "same") || streq (expr, "SAME"))
	    return (S_SAME)

	# Error bars?
	# Negate the values so that gmark plots them as WCS sizes.
	# Multiply sizes by two so that errors specified are radii.
	if (errsize (expr)) {
	    do i = 1, nselected
	    	if (IS_INDEFR(size[i]))
		    size[i] = SMALLMARK
		else
		    size[i] = -2. * size[i]
	    return (S_ERROR)
	}

	# Allocate space for values
	call smark (sp)
	call salloc (pout, nselected, TY_REAL)

	# Determine whether the sizes are scaled or not (~ flag)
	while (IS_WHITE(expr[ip]))
	    ip = ip + 1
	if (expr[ip] == '~') {
	    ip = ip + 1
	    scaled = true
	} else
	    scaled = false

	# Evaluate the size expression
	call eval_expr (expr[ip], db, index, pout, junk, TY_REAL, false)

	# If not scaled data, then returned sizes have the values of the expr.
	# Negate the values so that gmark plots them as WCS sizes.
	# Multiply sizes by two so that size specified was radius.
	if (! scaled) {
	    do i = 1, nselected
	    	if (IS_INDEFR(Memr[pout+i-1]))
		    size[i] = SMALLMARK
		else
		    size[i] = -2. * Memr[pout+i-1]
	    call sfree (sp)
	    return (S_VALUE)
	}

	# If scaled data, then scale sizes in range MIN_SIZE to MAX_SIZE
	start = NO
	minsize = 0
	maxsize = 0
	do i = 1, nselected {
	    value = Memr[pout+i-1]
	    if (! IS_INDEFR(value)) {
		if (start == NO) {
		    start = YES
		    minsize = value
		    maxsize = value
		    next
		}
		if (value < minsize)
		    minsize = value
		else if (value > maxsize)
		    maxsize = value
	    }
	}
	if (abs(maxsize - minsize) < EPSILONR) {
	    minsize = abs((CH_MAXSIZE(ch) - CH_MINSIZE(ch)) / 2)
	    call amovkr (minsize, size, nselected)
	} else {
	    ratio = (CH_MAXSIZE(ch) - CH_MINSIZE(ch)) / (maxsize - minsize)
	    do i = 1, nselected
	    	if (IS_INDEFR(Memr[pout+i-1]))
		    size[i] = SMALLMARK
		else
		    size[i] = (Memr[pout+i-1]-minsize) * ratio + CH_MINSIZE(ch)
	}
	call sfree (sp)
	if (CH_MAXSIZE(ch) > CH_MINSIZE(ch))
	    return (S_SCALED_POS)
	else
	    return (S_SCALED_NEG)
end

# SIZE_TITLE -- Create title for sizing function

procedure size_title (code, label, func, outstring, maxchars)
int	code		# Scaled or not
char	label[ARB]	# "size", "xsize", or "ysize"
char	func[ARB]	# Sizing function string
char	outstring	# Title string
int	maxchars	# Maximum number of characters in outstring

int	len, strlen()

begin
    # Nothing done if blank function
    if (code == S_BLANK)
	return

    # Determine needed spaces between xlabel and ylabel
    if (strlen(outstring) > 0) {
	len = strlen(outstring) + strlen("    ") + strlen(label) +
	      strlen(": ") + strlen(func)
	if (code == S_SCALED_POS || code == S_SCALED_NEG)
	    len = len + strlen("(~+)")
	if (len > LEN_TITLE)
	    call strcat ("\n", outstring, maxchars)
	else
	    call strcat ("    ", outstring, maxchars)
    }

    # Create label
    call strcat (label, outstring, maxchars)
    if (code == S_SCALED_POS)
    	call strcat ("(~+)", outstring, maxchars)
    else if (code == S_SCALED_NEG)
    	call strcat ("(~-)", outstring, maxchars)
    call strcat (": ", outstring, maxchars)
    call strcat (func, outstring, maxchars)
end

define	DEG2RAD	(3.1415 / 180.)		# Convert degrees to radians

# CUT_FIELD -- Apply field limitations to database.  The weight for any entry
# outside the specified field is set to zero.

procedure cut_field (ch, db, index, nselected, title, length)
pointer	ch
pointer	db
int	index[ARB]
int	nselected
char	title[length]
int	length

pointer	sp, xarray, yarray, temp, format, junk
int	i, x, y, idx_field(), j, strlen()
begin
    switch (CH_FIELD(CH)) {
    case NONE:
    case CELESTIAL:
	x = idx_field (db, "RA")
	y = idx_field (db, "DEC")
	if (x == 0 || y == 0) {
	    call eprintf (0, "Warning: Can't do celestial field cutoff without database fields RA and DEC\n")
	    return
	}
	call smark (sp)
        call salloc (temp, SZ_LINE, TY_CHAR)
        call salloc (format, SZ_LINE, TY_CHAR)
	call salloc (xarray, nselected, TY_DOUBLE)
	call salloc (yarray, nselected, TY_DOUBLE)
	call eval_expr ("RA", db, index, xarray, junk, TY_DOUBLE, false)
	call eval_expr ("DEC", db, index, yarray, junk, TY_DOUBLE, false)
	if (CH_RADIUS(ch) > 0) {
	    j = 0
	    for (i = 1; i <= nselected; i = i + 1) {
	    	if (abs((Memd[xarray+i-1]-CH_XCENTER(ch))*15.*
			 cos(CH_YCENTER(ch)*DEG2RAD)) <= CH_RADIUS(ch) &&
		    abs (Memd[yarray+i-1]-CH_YCENTER(ch)) <= CH_RADIUS(ch)) {
		    j = j + 1
		    index[j] = index[i]
		}
	    index[j+1] = 0
	    }
	} else {
	    j = 0
	    for (i = 1; i <= nselected; i = i + 1) {
		if (((Memd[xarray+i-1]-CH_XCENTER(ch))*15.*
		      cos(CH_YCENTER(ch)*DEG2RAD))**2
		    +(Memd[yarray+i-1]-CH_YCENTER(ch))**2 <=CH_RADIUS(ch)**2) {
		    j = j + 1
		    index[j] = index[i]
		}
	    index[j+1] = 0
	    }
	}
	call sprintf (Memc[format], SZ_LINE,
		      "field: ra = %s   dec = %s   radius = %s")
	    call parg_dbformat (db, x)
	    call parg_dbformat (db, y)
	    call parg_dbformat (db, y)
	call sprintf (Memc[temp], SZ_LINE, Memc[format])
	    call pargd (CH_XCENTER(ch))
	    call pargd (CH_YCENTER(ch))
	    call pargd (CH_RADIUS(ch))
	if (strlen (title) > 0)
	    call strcat ("\n", title, length)
	call strcat (Memc[temp], title, length)
	call sfree (sp)
    case XY:
	x = idx_field (db, "X")
	y = idx_field (db, "Y")
	if (x == 0 || y == 0) {
	    call eprintf (0, "Warning: Can't do 'xy' field cutoff without database fields 'X' and 'Y'\n")
	    return
	}
	call smark (sp)
        call salloc (temp, SZ_LINE, TY_CHAR)
        call salloc (format, SZ_LINE, TY_CHAR)
	call salloc (xarray, nselected, TY_DOUBLE)
	call salloc (yarray, nselected, TY_DOUBLE)
	call eval_expr ("X", db, index, xarray, junk, TY_DOUBLE, false)
	call eval_expr ("Y", db, index, yarray, junk, TY_DOUBLE, false)
	if (CH_RADIUS(ch) > 0) {
	    j = 0
	    for (i = 1; i <= nselected; i = i + 1)
	    	if (abs (Memd[xarray+i-1]-CH_XCENTER(ch)) <= CH_RADIUS(ch) &&
		    abs (Memd[yarray+i-1]-CH_YCENTER(ch)) <= CH_RADIUS(ch)) {
		    j = j + 1
		    index[j] = index[i]
		}
	    index[j+1] = 0
	} else {
	    j = 0
	    for (i = 1; i <= nselected; i = i + 1)
		if ((Memd[xarray+i-1]-CH_XCENTER(ch))**2
		   +(Memd[yarray+i-1]-CH_YCENTER(ch))**2 <= CH_RADIUS(ch)**2) {
		    j = j + 1
		    index[j] = index[i]
		}
	    index[j+1] = 0
	}
	call sprintf (Memc[format], SZ_LINE,
		      "field: x = %s   y = %s   radius = %s")
	    call parg_dbformat (db, x)
	    call parg_dbformat (db, y)
	    call parg_dbformat (db, x)
	call sprintf (Memc[temp], SZ_LINE, Memc[format])
	    call pargd (CH_XCENTER(ch))
	    call pargd (CH_YCENTER(ch))
	    call pargd (CH_RADIUS(ch))
	if (strlen (title) > 0)
	    call strcat ("\n", title, length)
	call strcat (Memc[temp], title, length)
	call sfree (sp)
    }
end
