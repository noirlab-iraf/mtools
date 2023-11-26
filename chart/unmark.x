include	<gset.h>
include	<mach.h>
include	<pkg/gtools.h>

# UNMARK -- Unmark marked data point nearest the cursor.
# The nearest point to the cursor in NDC coordinates is determined.

# Formerly xtools$icfit/icgundeleter.x

procedure unmark (gp, gt, xarray, yarray, marker, usermarks, color, arrow,
    xsize, ysize, npts, wx, wy, marked, outliers, defmark)

pointer	gp					# GIO pointer
pointer	gt					# GTOOLS pointer
real	xarray[ARB]				# X-axis data points
real	yarray[ARB]				# Y-axis data points
int	marker[ARB], usermarks[ARB]		# Marker arrays
int	color[ARB]				# Color array
int	arrow[ARB]			# Direction of arrow if point off plot
real	xsize[ARB]				# Size arrays
real	ysize[ARB]				# Size arrays
int	npts					# Number of points
real	wx, wy					# Position to be nearest
int	marked					# Marker type for marked points
bool	outliers				# Mark outliers on the plot
int	defmark					# Default marker

int	gt_geti()

begin
	if (gt_geti (gt, GTTRANSPOSE) == NO)
	    call icg_u1r (gp, xarray, yarray, marker, usermarks, color, arrow,
		xsize, ysize, npts, wx, wy, marked, outliers, defmark)
	else
	    call icg_u1r (gp, yarray, xarray, marker, usermarks, color, arrow,
		ysize, xsize, npts, wy, wx, marked, outliers, defmark)
end


# ICG_U1 -- Do the actual undelete.

procedure icg_u1r (gp, x, y, marker, usermarks, color, arrow, xsize, ysize,
		   npts, wx, wy, marked, outliers, defmark)

pointer	gp					# GIO pointer
real	x[ARB], y[ARB]				# Data points
int	marker[ARB], usermarks[ARB]		# Marker arrays
int	color[ARB]				# Color array
int	arrow[ARB]			# Direction of arrow if point off plot
real	xsize[ARB]				# Size arrays
real	ysize[ARB]				# Size arrays
int	npts					# Number of points
real	wx, wy					# Position to be nearest
int	marked					# Marker type for marked points
bool	outliers				# Mark outliers on the plot
int	defmark					# Default marker

int	i, j
real	x0, y0, r2, r2min
int	pmcolor, plcolor, gstati()

begin
	# Transform world cursor coordinates to NDC.

	call gctran (gp, wx, wy, wx, wy, 1, 0)

	# Search for nearest point to a point with zero weight.

	r2min = MAX_REAL
	if (outliers)
	    do i = 1, npts {
	    	if (marker[i] != marked)
		    next
	    	call gctran (gp, x[i], y[i], x0, y0, 1, 0)
	    	r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	    	if (r2 < r2min) {
		    r2min = r2
		    j = i
	    	}
	    }
	else
	    do i = 1, npts {
	    	if (marker[i] != marked || arrow[i] > 0)
		    next
	    	call gctran (gp, x[i], y[i], x0, y0, 1, 0)
	    	r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	    	if (r2 < r2min) {
		    r2min = r2
		    j = i
	    	}
	    }

	# Unmark the deleted point and reset the marker.

	if (j != 0) {
	    call gscur (gp, x[j], y[j])
	    pmcolor = gstati(gp, G_PMCOLOR)
	    plcolor = gstati(gp, G_PLCOLOR)
	    call gseti (gp, G_PMLTYPE, GL_CLEAR)
	    call draw_mark (gp, x[j], y[j], marker[j], color[j], xsize[j],
			    ysize[j], defmark)
	    call gseti (gp, G_PMLTYPE, GL_SOLID)
	    marker[j] = usermarks[j]
	    call draw_mark (gp, x[j], y[j], marker[j], color[j], xsize[j],
			    ysize[j], defmark)
	    if (arrow[j] > 0)
		call plot_arrow (gp, x[j], y[j], arrow[j], color[j])
	    call gseti(gp, G_PMCOLOR, pmcolor)
	    call gseti(gp, G_PLCOLOR, plcolor)
	}
end
