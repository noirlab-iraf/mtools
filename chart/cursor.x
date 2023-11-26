include <gio.h>
include <gset.h>
#include	<pkg/gtools.h>
include	"gtools.h"
include "chart.h"
include "database.h"

# CURSOR -- Interactively control graphs.

# Formerly xtools$icfit/icgfit.gx

procedure cursor (ch, gp, curse, gt, db, index, marker, color, npts)

pointer	ch			# CHART pointer
pointer	gp			# GIO pointer
char	curse[ARB]		# GIO cursor input
pointer	gt[CH_NGKEYS+1]		# GTOOLS pointer
pointer db			# DATABASE pointer
int	index[npts+1]		# Selected elements index
int	marker[npts]		# Markers
int	color[npts]		# Colors
int	npts			# Number of points

real	wx, wy, radius, temp
int	wcs, key
char	cmd[SZ_LINE]

int	i, newgraph, newhisto, select(), nselected, j, newsample
pointer	sp, usermarks, format, xsize, ysize
pointer xarray, yarray, xplot, yplot, arrow	# x and y axis data arrays

int	gt_gcur1(), scan(), ncols, nlines, gt_geti(), newkey, dbnfields()
int	nearest(), garg_key(), clear_screen, always_clear_screen, dbkey()
int	thekey, gkey, nscan(), line1, line2, parse_key(), oldgkey
bool	ttygetb(), errsize()
pointer	tty, ttyodes(), xszfunc, yszfunc
pointer	sp1, nxaxis, nyaxis, nxunits, nyunits, nxsize, nysize, buffer
real	nxmin, nxmax, nymin, nymax
bool	nxflip, nyflip, nsquare, strne(), xerrOk, yerrOk

# Palomar multislit mask stuff for 'x' option
real	angle, xtemp, scale, xmask, ymask

begin
	# Allocate memory for the data and a copy of the weights.
	# The weights are copied because they are changed when points are
	# marked.

	call smark (sp)
	call salloc (usermarks, npts, TY_INT)
	call salloc (xarray, npts, TY_REAL)
	call salloc (yarray, npts, TY_REAL)
	call salloc (xsize, npts, TY_REAL)
	call salloc (ysize, npts, TY_REAL)
	call salloc (xplot, npts, TY_REAL)
	call salloc (yplot, npts, TY_REAL)
	call salloc (arrow, npts, TY_INT)
	call salloc (format, SZ_LINE, TY_CHAR)
	call salloc (xszfunc, CH_SZFUNCTION, TY_CHAR)
	call salloc (yszfunc, CH_SZFUNCTION, TY_CHAR)

	# Read cursor commands.

	oldgkey = 1
	gkey = 1
	key = 'd'
	nselected = 1 # Gets us past the test at top of loop the first time
	newgraph = NO
	newsample = NO
	newkey = NO

	repeat {
	    # If current sample is empty, limit options
	    if (nselected == 0)
		if (key != ':' && key != 'd' && key != 'q') {
		    call eprintf ("Warning: No objects in current sample -- redefine the sample\n")
		    goto 10
		}
	    # Respond to specified key
	    switch (key) {
	    case '?': # Print help text.
		call gpagefile (gp, CH_DEFHELP, CH_PROMPT)

	    case ':': # List or set parameters
		if (cmd[1] == '/') {
	            call gt_colon (cmd, gp, gt[gkey], newgraph)
		    call gt_seti (gt[gkey], GTTRANSPOSE, NO) # No transposing
		    newgraph = YES
		} else
		    call colon (ch, cmd, newgraph, newkey, newsample, newhisto,
				gp, gt, db, index, marker, nselected)

	    case 'b': # Histogram
		call histogram (db, ch, gp, gt, index, marker, Memi[usermarks],
                                color, npts, nselected, newsample, newkey)
		if (nselected == 0)
		    newgraph = NO
		else
		    newgraph = YES

	    case 'c', 'p': # Print the positions of data points.
		i = nearest (gp, gt[gkey], Memr[xplot], Memr[yplot],
			     Memi[arrow], nselected, wx, wy, CH_PLOTARROWS(ch))

	    	if (i != 0) {
		    call printf ("%s = ")
			call parg_dbname (db, dbkey(db))
		    call dbfprintf (STDOUT, db, index[i], dbkey(db), NO, NO)
		    call printf ("     %s = %.5g     %s = %.5g\n")
		    if (gt_geti (gt[gkey], GTTRANSPOSE) == NO) {
			call pargstr (Memc[CH_AXIS(ch, gkey, 1)])
			call pargr (Memr[xarray+i-1])
			call pargstr (Memc[CH_AXIS(ch, gkey, 2)])
			call pargr (Memr[yarray+i-1])
		    } else {
			call pargstr (Memc[CH_AXIS(ch, gkey, 2)])
			call pargr (Memr[yarray+i-1])
			call pargstr (Memc[CH_AXIS(ch, gkey, 1)])
			call pargr (Memr[xarray+i-1])
		    }
		    call flush (STDOUT)
		    if (key == 'p') {
		    	call gdeactivate (gp, AW_CLEAR)
			call xttysize (ncols, nlines)
			nlines = nlines - 1
			tty = ttyodes ("terminal")
			if (ttygetb (tty, "ns"))
			    always_clear_screen = YES
			else
			    always_clear_screen = NO
			line1 = 1
			line2 = min (nlines, dbnfields(db))
			while (line1 != 0 && line2 != 0) {
		    	    do j = line1, line2 {
				call printf ("%20s = ")
				call parg_dbname (db, j)
				call dbfprintf (STDOUT, db, index[i],j,YES,YES)
				call printf ("\n")
			    }
			    call pager (tty, nlines, dbnfields(db), line1,
					line2, clear_screen)
			    if (clear_screen==YES||always_clear_screen==YES) {
	    			call ttyclear (STDOUT, tty)
	    			call flush (STDOUT)
			    }
		    	}
		    	call greactivate (gp, AW_PAUSE)
		    }
		}

	    case 'd': # New sample
		nselected = select (gt, ch, db, npts, index, marker, color)
		if (nselected == 0) {
		    # No objects in sample -- plot empty graph and warning
		    call gclear (gp)
		    call gswind (gp, -0.001, 0.001, -0.001, 0.001)
		    call gt_labax (gp, gt[gkey])
		    call gtext (gp, 0., 0., "NO OBJECTS IN DEFINED SAMPLE",
				"hjustify=center,vjustify=center")
	    	    call eprintf ("Warning: No objects meet all selection criteria\n")
		} else {
		    # A good new sample
		    call amovi (marker, Memi[usermarks], nselected)
		    newsample = YES
		}

	    case 'f': # Make cursor position next field center
		CH_XCENTER(ch) = wx
		CH_YCENTER(ch) = wy

	    case 'g':	# Set graph axes types.
		call printf ("Graph key to be defined (h, i, j, k, or l): ")
		call flush (STDOUT)
		if (scan() == EOF)
		    goto 10
		call gargc (cmd[1])

		switch (cmd[1]) {
		case 'h', 'i', 'j', 'k', 'l':
		    switch (cmd[1]) {
		    case 'h':
		        thekey = 1
		    case 'i':
		        thekey = 2
		    case 'j':
		        thekey = 3
		    case 'k':
		        thekey = 4
		    case 'l':
		        thekey = 5
		    }

		    call printf ("Graph key description (%s  %s): ")
		        call pargstr (Memc[CH_AXIS(ch,thekey,1)])
		        call pargstr (Memc[CH_AXIS(ch,thekey,2)])
		    call flush (STDOUT)
		    call smark (sp1)
		    call salloc (buffer, 10*CH_SZFUNCTION, TY_CHAR)
		    call salloc (nxaxis, CH_SZFUNCTION, TY_CHAR)
		    call salloc (nyaxis, CH_SZFUNCTION, TY_CHAR)
		    call salloc (nxunits, CH_SZFUNCTION, TY_CHAR)
		    call salloc (nyunits, CH_SZFUNCTION, TY_CHAR)
		    call salloc (nxsize, CH_SZFUNCTION, TY_CHAR)
		    call salloc (nysize, CH_SZFUNCTION, TY_CHAR)
		    if (scan() == EOF) {
			call sfree (sp1)
		        goto 10
		    }
		    call gargstr (Memc[buffer], 10*CH_SZFUNCTION)
		    if (parse_key (Memc[buffer], db, Memc[nxaxis],Memc[nyaxis],
			 Memc[nxunits], Memc[nyunits], Memc[nxsize],
			 Memc[nysize], nsquare, nxflip, nyflip,
			 nxmin, nxmax, nymin, nymax, xerrOk, yerrOk) == ERR) {
			call sfree (sp1)
			goto 10
		    }
		    call strcpy (Memc[nxaxis], Memc[CH_AXIS(ch,thekey,1)],
				 CH_SZFUNCTION)
		    call strcpy (Memc[nyaxis], Memc[CH_AXIS(ch,thekey,2)],
				 CH_SZFUNCTION)
		    call strcpy (Memc[nxunits], Memc[CH_UNIT(ch,thekey,1)],
				 CH_SZFUNCTION)
		    call strcpy (Memc[nyunits], Memc[CH_UNIT(ch,thekey,2)],
				 CH_SZFUNCTION)
		    call strcpy (Memc[nxsize], Memc[CH_AXSIZE(ch,thekey,1)],
				 CH_SZFUNCTION)
		    call strcpy (Memc[nysize], Memc[CH_AXSIZE(ch,thekey,2)],
				 CH_SZFUNCTION)
		    call gt_sets (gt[thekey], GTXLABEL, Memc[nxaxis])
		    call gt_sets (gt[thekey], GTYLABEL, Memc[nyaxis])
		    call gt_sets (gt[thekey], GTXUNITS, Memc[nxunits])
		    call gt_sets (gt[thekey], GTYUNITS, Memc[nyunits])
		    GT_XMIN(gt[thekey]) = nxmin
		    GT_XMAX(gt[thekey]) = nxmax
		    GT_YMIN(gt[thekey]) = nymin
		    GT_YMAX(gt[thekey]) = nymax
		    CH_FLIP(ch,thekey, 1) = nxflip
		    CH_FLIP(ch,thekey, 2) = nyflip
		    CH_ERROK(ch,thekey, 1) = xerrOk
		    CH_ERROK(ch,thekey, 2) = yerrOk
		    CH_SQUARE(ch, thekey) = nsquare
		    CH_DEFINED(ch, thekey) = true
		    call sfree (sp1)
		    if (gkey == thekey)
			newkey = YES

		default:
		    call eprintf ("Warning: Not a graph key (%c)\n")
			call pargc (cmd[1])
		}

	    case 'h':
		if (CH_DEFINED(ch, 1)) {
		    if (gkey != 1) {
			if ((!CH_ERROK(ch,1,1)&&errsize(Memc[CH_XSIZE(ch)])) ||
			    (!CH_ERROK(ch,1,2)&&errsize(Memc[CH_YSIZE(ch)]))) {
			    call eprintf ("Warning: err sizing function not legal: a field lacks errors")
			    goto 10
			}
		    	gkey = 1
		    	newkey = YES
		    }
		} else
		    call eprintf ("Warning: Graph key not defined\n")

	    case 'i':
		if (CH_DEFINED(ch, 2)) {
		    if (gkey != 2) {
			if ((!CH_ERROK(ch,2,1)&&errsize(Memc[CH_XSIZE(ch)])) ||
			    (!CH_ERROK(ch,2,2)&&errsize(Memc[CH_YSIZE(ch)]))) {
			    call eprintf ("Warning: err sizing function not legal: a field lacks errors")
			    goto 10
			}
		    	gkey = 2
		    	newkey = YES
		    }
		} else
		    call eprintf ("Warning: Graph key not defined\n")

	    case 'j':
		if (CH_DEFINED(ch, 3)) {
		    if (gkey != 3) {
			if ((!CH_ERROK(ch,3,1)&&errsize(Memc[CH_XSIZE(ch)])) ||
			    (!CH_ERROK(ch,3,2)&&errsize(Memc[CH_YSIZE(ch)]))) {
			    call eprintf ("Warning: err sizing function not legal: a field lacks errors")
			    goto 10
			}
		    	gkey = 3
		    	newkey = YES
		    }
		} else
		    call eprintf ("Warning: Graph key not defined\n")

	    case 'k':
		if (CH_DEFINED(ch, 4)) {
		    if (gkey != 4) {
			if ((!CH_ERROK(ch,4,1)&&errsize(Memc[CH_XSIZE(ch)])) ||
			    (!CH_ERROK(ch,4,2)&&errsize(Memc[CH_YSIZE(ch)]))) {
			    call eprintf ("Warning: err sizing function not legal: a field lacks errors")
			    goto 10
			}
		    	gkey = 4
		    	newkey = YES
		    }
		} else
		    call eprintf ("Warning: Graph key not defined\n")

	    case 'l':
		if (CH_DEFINED(ch, 5)) {
		    if (gkey != 5) {
			if ((!CH_ERROK(ch,5,1)&&errsize(Memc[CH_XSIZE(ch)])) ||
			    (!CH_ERROK(ch,5,2)&&errsize(Memc[CH_YSIZE(ch)]))) {
			    call eprintf ("Warning: err sizing function not legal: a field lacks errors")
			    goto 10
			}
		    	gkey = 5
		    	newkey = YES
		    }
		} else
		    call eprintf ("Warning: Graph key not defined\n")

	    case 'm': # Mark data points.
		call mark (gp, gt[gkey], Memr[xplot], Memr[yplot], marker,
			   color, Memi[arrow], Memr[xsize], Memr[ysize],
			   nselected, wx, wy, CH_MMARK(ch), CH_PLOTARROWS(ch),
			   CH_DEFMARKER(ch))

	    case 'n': # Show number of points
		call printf ("%d")
		call pargi (nselected)
		call flush (STDOUT)

	    case 'o': # Toggle axis squareness
		CH_SQUARE(ch, gkey) = ! CH_SQUARE(ch, gkey)
		newgraph = YES

	    case 'r': # Redraw the graph
		newgraph = YES

	    case 's': # Put cursor on given search key
		call printf ("Search key: ")
		call flush (STDOUT)
		if (scan() == EOF)
		    goto 10
		i = garg_key (db, index)
		if (nscan() != 1)
		    goto 10
		if (i == 0) {
	    	    call reset_scan()
	    	    call gargwrd (Memc[format], SZ_LINE)
		    call eprintf ("Warning: Search key not in defined sample (%s)\n")
			call pargstr (Memc[format])
		    goto 10
		}
		if (gt_geti (gt[gkey], GTTRANSPOSE) == NO)
		    call gscur (gp, Memr[xplot+i-1], Memr[yplot+i-1])
		else
		    call gscur (gp, Memr[yplot+i-1], Memr[xplot+i-1])
		call printf ("%s = ")
		    call parg_dbname (db, dbkey(db))
		call dbfprintf (STDOUT, db, index[i], dbkey(db), NO, NO)
		call printf ("     %s = %.5g     %s = %.5g\n")
		if (gt_geti (gt[gkey], GTTRANSPOSE) == NO) {
		    call pargstr (Memc[CH_AXIS(ch, gkey, 1)])
		    call pargr (Memr[xarray+i-1])
		    call pargstr (Memc[CH_AXIS(ch, gkey, 2)])
		    call pargr (Memr[yarray+i-1])
		} else {
		    call pargstr (Memc[CH_AXIS(ch, gkey, 2)])
		    call pargr (Memr[yarray+i-1])
		    call pargstr (Memc[CH_AXIS(ch, gkey, 1)])
		    call pargr (Memr[xarray+i-1])
		}

	    case 't': # Draw circle with specified radius
		call printf ("radius (%g): ")
		    call pargr (radius)
		call flush (STDOUT)
		i = scan()
		    call gargr (temp)
		if (nscan() == 1)
		    radius = temp
		if (gt_geti (gt[gkey], GTTRANSPOSE) == NO)
		    call gmark (gp, wx, wy, GM_CIRCLE, -2*radius, -2*radius)
		else
		    call gmark (gp, wy, wx, GM_CIRCLE, -2*radius, -2*radius)

	    case 'u': # Unmark data points.
		call unmark (gp, gt[gkey], Memr[xplot], Memr[yplot], marker,
		    	     Memi[usermarks], color, Memi[arrow], Memr[xsize],
			     Memr[ysize], nselected, wx, wy,CH_MMARK(ch),
			     CH_PLOTARROWS(ch), CH_DEFMARKER(ch))

	    case 'w':  # Window graph
		call gt_window (gt[gkey], gp, curse, newgraph)

	    case 'x': # Remove the header info from all plots
		do i = 1, CH_NGKEYS {
		    call gt_seti (gt[i], GTSYSID, NO)
		    call gt_sets (gt[i], GTPARAMS, "")
		    call gt_sets (gt[i], GTTITLE, "")
		    call gt_sets (gt[i], GTSUBTITLE, "")
		    call gt_sets (gt[i], GTCOMMENTS, "")
		}
		newgraph = YES

	    case 'z': # Draw Palomar multislit mask with specified radius
		call printf ("angle(degrees) scale(arcsec/WCS) (%g %g): ")
		    call pargr (angle)
		    call pargr (scale)
		call flush (STDOUT)
		i = scan()
		    call gargr (temp)
		    call gargr (xtemp)
		if (nscan() >= 1)
		    angle = temp
		if (nscan() == 2)
		    scale = xtemp
		xmask = wx
		ymask = wy
		if (gt_geti (gt[gkey], GTTRANSPOSE) == NO)
		    call palomar (gp, xmask, ymask, angle, scale)
		else
		    call palomar (gp, ymask, xmask, angle, scale)

	    case 'v': # Print out objects falling with Palomar multislit mask
		call mask_objects (ch, gp, gt, db, index, xmask, ymask,
			Memr[xarray], Memr[yarray], nselected, angle, scale,
			Memc[CH_AXIS(ch,gkey,1)], Memc[CH_AXIS(ch,gkey,2)])

	    default: # Ring bell
		call printf ("\07\n")
	    }

	    # Skip graph if empty sample
10	    if (nselected == 0)
		next

	    # Get axis functions if new sample or new graph key
	    if (newsample == YES || newkey == YES) {
		# Determine which sizing function to use
		if (strne (Memc[CH_AXSIZE(ch,gkey,1)], ""))
		    call strcpy (Memc[CH_AXSIZE(ch,gkey,1)], Memc[xszfunc],
				 CH_SZFUNCTION)
		else
		    call strcpy (Memc[CH_XSIZE(ch)], Memc[xszfunc],
				 CH_SZFUNCTION)
		if (strne (Memc[CH_AXSIZE(ch,gkey,2)], ""))
		    call strcpy (Memc[CH_AXSIZE(ch,gkey,2)], Memc[yszfunc],
				 CH_SZFUNCTION)
		else
		    call strcpy (Memc[CH_YSIZE(ch)], Memc[yszfunc],
				 CH_SZFUNCTION)

		# Get x and y axis data points and plot them.
		if (errsize (Memc[xszfunc]))
		    call eval_expr (Memc[CH_AXIS(ch,gkey,1)], db, index,
				    xarray, xsize, TY_REAL, true)
		else
		    call eval_expr (Memc[CH_AXIS(ch,gkey,1)], db, index,
				    xarray, xsize, TY_REAL, false)
		if (errsize (Memc[yszfunc]))
		    call eval_expr (Memc[CH_AXIS(ch,gkey,2)], db, index,
				    yarray, ysize, TY_REAL, true)
		else
		    call eval_expr (Memc[CH_AXIS(ch,gkey,2)], db, index,
				    yarray, ysize, TY_REAL, false)

	    	# Resize markers if current graph key has been redefined
	    	# (which includes changes to parameters xsize, ysize, min_size,
	    	# or max_size), or if a new graph key has been selected,
	    	# or if a new sample has been selected.
		call size_up (ch, db, gp, index, gt[gkey], Memc[xszfunc],
			    Memc[yszfunc], Memr[xsize], Memr[ysize], nselected)
	    }

	    # Redraw the graph if necessary.
	    if (newgraph == YES || newkey == YES || newsample == YES)
	    	call graph (ch, gp, gt[gkey], Memr[xarray], Memr[yarray],
			    Memr[xplot], Memr[yplot], marker, color,
			    Memi[arrow], Memr[xsize], Memr[ysize], nselected,
			    gkey, CH_MMARK(ch), CH_PLOTARROWS(ch))

	    newsample = NO
	    newgraph = NO
	    newkey = NO
	    oldgkey = gkey
	} until (gt_gcur1 (gt[gkey],curse,wx,wy,wcs, key, cmd, SZ_LINE) == EOF)

	call sfree (sp)
end

# ERRSIZE -- Return true if the string is "error", "ERROR", "err", or "ERR".

bool procedure errsize (str)
char	str[ARB]

bool	streq()

begin
    return (streq(str, "error") || streq(str, "ERROR") ||
	streq(str, "err")   || streq(str, "ERR"))
end

