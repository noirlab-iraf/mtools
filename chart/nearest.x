include	<mach.h>
include	<pkg/gtools.h>

# NEAREST -- Find the nearest point to the cursor and return the index.
# The nearest point to the cursor in NDC coordinates is determined.
# The cursor is moved to the nearest point selected.

# Formerly xtools$icfit/icgnearestr.x

int procedure nearest (gp, gt, xarray, yarray, arrow, npts, wx, wy, outliers)

pointer	gp					# GIO pointer
pointer	gt					# GTOOLS pointer
real	xarray[npts]				# X-axis data points (plotted)
real	yarray[npts]				# Y-axis data points (plotted)
int	arrow[npts]				# Direction of arrow
int	npts					# Number of points
real	wx, wy					# Cursor position
bool	outliers				# Mark outliers on the plot

int	pt

int	icg_nr(), gt_geti()

begin
	if (gt_geti (gt, GTTRANSPOSE) == NO)
	    pt = icg_nr (gp, xarray, yarray, arrow, npts, wx, wy, outliers)
	else
	    pt = icg_nr (gp, yarray, xarray, arrow, npts, wy, wx, outliers)
	return (pt)
end

int procedure icg_nr (gp, x, y, arrow, npts, wx, wy, outliers)

pointer	gp					# GIO pointer
real	x[npts], y[npts]			# Data points
int	arrow[npts]				# Direction of arrow
int	npts					# Number of points
real	wx, wy					# Cursor position
bool	outliers				# Mark outliers on the plot

int	i, j
real	x0, y0, r2, r2min

begin
	# Transform world cursor coordinates to NDC.

	call gctran (gp, wx, wy, wx, wy, 1, 0)

	# Search for nearest point.

	j = 0
	r2min = MAX_REAL
	if (outliers)
	    do i = 1, npts {
	    	call gctran (gp, x[i], y[i], x0, y0, 1, 0)
	    	r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	    	if (r2 < r2min) {
		    r2min = r2
		    j = i
	    	}
	    }
	else
	    do i = 1, npts {
	    	if (arrow[i] > 0)
		    next
	    	call gctran (gp, x[i], y[i], x0, y0, 1, 0)
	    	r2 = (x0 - wx) ** 2 + (y0 - wy) ** 2
	    	if (r2 < r2min) {
		    r2min = r2
		    j = i
	    	}
	    }

	# Move the cursor to the selected point and return the index.

	if (j != 0)
	    call gscur (gp, x[j], y[j])

	return (j)
end
