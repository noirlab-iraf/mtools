include <math.h>
include <error.h>
include <gset.h>
include "chart.h"

define	WIDTH	15.	# Slit width in arcsecs
define	HEIGHT	26.2	# Slit height in arcsecs
define	RADIUS	90.	# Search radius in arcsecs

# PALOMAR -- Draw the palomar multislit mask at the specifed position and angle

procedure palomar (gp, xc, yc, angle, scale)
pointer	gp	# GIO pointer
real	xc	# X coordinate of mask center (WCS units)
real	yc	# Y coordinate of mask center (WCS units)
real	angle	# Position angle of mask (degrees)
real	scale	# Scale of mask (arcsec/WCS)

int	i
real	xin, yin, xout, yout, w, h

begin
    # Set width and height
    w = WIDTH / scale
    h = HEIGHT / scale

    # Draw mask
    for (i = 1; i <= 2; i = i + 1) {
	xin = -4 * w
	if (i == 1)
	    yin = -h
	else
	    yin = h
	call rotate (xin, yin, xout, yout, angle)
	call gamove (gp, xc+xout, yc+yout)
	xin = 4 * w
	call rotate (xin, yin, xout, yout, angle)
	call gadraw (gp, xc+xout, yc+yout)
    }
    for (i = 0; i <= 8; i = i + 1) {
	xin = (-4 + i) * w
	yin = h
	call rotate (xin, yin, xout, yout, angle)
	call gamove (gp, xc+xout, yc+yout)
	yin = -h
	call rotate (xin, yin, xout, yout, angle)
	call gadraw (gp, xc+xout, yc+yout)
    }
    call rotate (-0.5 * w, 0., xout, yout, angle)
    call gamove (gp, xc+xout, yc+yout, angle)
    call rotate ( 0.5 * w, 0., xout, yout, angle)
    call gadraw (gp, xc+xout, yc+yout, angle)
end

# ROTATE -- Rotate a point around (0,0) by a specified angle.

procedure rotate (xin, yin, xout, yout, angle)
real	xin	# Input x coordinate
real	yin	# Input y coordinate
real	xout	# Output x coordinate
real	yout	# Output y coordinate
real	angle	# Rotation angle (degrees)

begin
    xout = xin * cos (DEGTORAD(angle)) - yin * sin (DEGTORAD(angle))
    yout = yin * cos (DEGTORAD(angle)) + xin * sin (DEGTORAD(angle))
end

define	SZ_MASKNAME	10	# Maximum number of characters in mask name
define	ARCSEC_PER_MM	2.57	# Arcsecs per millimeter at the slit

# MASK_OBJECTS -- Find objects in mask

procedure mask_objects (ch, gp, gt, db, index, xmask, ymask, xarray, yarray,
			nselected, angle, scale, xlabel, ylabel)
pointer	ch		# CHART pointer
pointer	gp		# GIO pointer
pointer	gt		# GTOOLS pointer
pointer	db		# DATABASE pointer
int	index[ARB]	# Index array
real	xmask		# X-coordinate of mask center
real	ymask		# Y-coordinate of mask center
real	xarray[ARB]	# Array of x-coordinates of all objects
real	yarray[ARB]	# Array of y-coordinates of all objects
int	nselected	# Number of objects in current selected sample
real	angle		# Position angle of mask (degrees)
real	scale		# Arcsec/WCS
char	xlabel[ARB]	# Label of x-axis
char	ylabel[ARB]	# Label of y-axis

real	w, h, radius2, xout, yout, relx, rely
int	i, slitnum, dbkey(), fd, open(), scan(), nscan(), strlen()
char	maskname[SZ_MASKNAME], ofile[SZ_FNAME]
bool	streq()
pointer	sp, marked

begin
    # Allocate space for markers
    call smark (sp)
    call salloc (marked, nselected, TY_INT)

    # Scale mask width, height, and radius
    w = WIDTH / scale
    h = HEIGHT / scale
    radius2 = (RADIUS / scale) ** 2	# Radius in WCS

    # Prompt for mask name
    call printf ("Mask name: ")
    call flush (STDOUT)
    if (scan() == EOF)
	call strcpy ("", maskname, SZ_MASKNAME)
    else
	call gargstr (maskname, SZ_MASKNAME)
    
    # Prompt for output file name
    call printf ("Output file (<cr> for STDOUT): ")
    call flush (STDOUT)
    if (scan() == EOF)
	call strcpy ("STDOUT", ofile, SZ_FNAME)
    else {
	call gargstr (ofile, SZ_FNAME)
	if (nscan() == 0 || strlen (ofile) == 0)
	    call strcpy ("STDOUT", ofile, SZ_FNAME)
    }

    # Open output file
    iferr {
    	fd = open (ofile, NEW_FILE, TEXT_FILE)
    } then {
	call erract (EA_WARN)
	return
    }
    if (streq (ofile, "STDOUT"))
	call gdeactivate (gp, AW_CLEAR)

    # Print header
    call fprintf (fd, "# Mask name: %s\n")
	call pargstr (maskname)
    call fprintf (fd, "# Mask center -- x (WCS): %g   y (WCS):  %g\n")
	call pargr (xmask)
	call pargr (ymask)
    call fprintf (fd, "# Position angle: %6.2f   scale (arcsec/WCS): %g\n")
	call pargr (angle)
	call pargr (scale)
    call fprintf (fd, "#\n")
    call fprintf (fd, "#                millimeters       arcsec           arcsec\n")
    call fprintf (fd, "#nser  slit#    relx    rely    relx    rely   slitx   slity %7s %7s\n")
	call pargstr (xlabel)
	call pargstr (ylabel)

    # Scan through all objects, finding ones that fall within the search radius
    # of the mask.
    do i = 1, nselected {
	# Skip objects outside the mask search radius
	if (IS_INDEF(xarray[i]) || IS_INDEF(yarray[i])) {
	    Memi[marked+i-1] = 0
	    next
	}
	if ((xarray[i] - xmask) ** 2 + (yarray[i] - ymask) ** 2 > radius2) {
	    Memi[marked+i-1] = 0
	    next
	}

	# Mark point for "spitlist"
	Memi[marked+i-1] = CH_MMARK(ch)

	# Determine coordinates relative to the mask
	call rotate (xarray[i]-xmask, yarray[i]-ymask, xout, yout, -angle)
	if (abs(xout) > 4*w || abs(yout) > h) {
	    slitnum = INDEFI
	    relx = INDEFR
	    rely = INDEFR
	} else  {
	    slitnum = int ((xout + 4*w) / w)
	    relx = mod ((xout + 4*w), w)
	    rely = yout
	}

	# Print out results for this object
	call dbfprintf (fd, db, index[i], dbkey(db), NO, NO)
	call fprintf (fd, "  %5d  %6.2f  %6.2f  %6.2f  %6.2f  %6.2f  %6.2f %7.3f %7.3f\n")
	if (IS_INDEFI(slitnum)) {
	    call pargi (INDEFI)
	    call pargr (INDEFR)
	    call pargr (INDEFR)
	    call pargr (INDEFR)
	    call pargr (INDEFR)
	} else {
	    call pargi (slitnum)
	    call pargr (relx * scale / ARCSEC_PER_MM)
	    call pargr (rely * scale / ARCSEC_PER_MM)
	    call pargr (relx * scale)
	    call pargr (rely * scale)
	}
	call pargr (xout * scale)
	call pargr (yout * scale)
	call pargr (xarray[i])
	call pargr (yarray[i])
    }
    call fprintf (fd, "\n\n\n")
    if (streq (ofile, "STDOUT"))
	call greactivate (gp, 0)
    call close (fd)

    # Write out general info
    call spitlist (ch, gp, gt, ofile, db, index, Memi[marked], nselected,
		   "marked")

    # Free space
    call sfree (sp)
end

