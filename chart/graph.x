include	<gset.h>
#include	<pkg/gtools.h>
include	"gtools.h"
include <error.h>
include "chart.h"
include "markers.h"

# GRAPH -- Graph data.

# Formerly xtools$icfit/icggraph.x

define	RIGHT	1
define	UP	3
define	LEFT	5
define	DOWN	7

procedure graph (ch, gp, gt, xarray, yarray, xplot, yplot, marker, color,
		 arrow, xsize, ysize, npts, key, marked, outliers)

pointer	ch				# CHART pointer
pointer	gp				# GIO pointer
pointer	gt				# GTOOLS pointers
real	xarray[npts]			# X-axis data points
real	yarray[npts]			# Y-axis data points
real	xplot[npts]			# X-axis plotted positions
real	yplot[npts]			# Y-axis plotted positions
int	marker[npts]			# Weights
int	color[npts]			# Marker color
int	arrow[npts]			# Direction of arrow if point off plot
real	xsize[npts]			# Marker x sizes
real	ysize[npts]			# Marker y sizes
int	npts				# Number of points
int	key 	    	    	    	# Graph key
int	marked				# Marker type for marked points
bool	outliers			# Mark outliers on the plot

int	gt_geti(), i
real	xmin, xmax, ymin, ymax

begin
	# Start a new plot.

	call gclear (gp)

	# Set the graph scale and axes.
	iferr {
	    if (IS_INDEF(GT_XMIN(gt)) || IS_INDEF(GT_XMAX(gt))) {
		do i = 1, npts
		    if (! IS_INDEF(xarray[i]))
			break
		if (i > npts)
		    call error (0, "Entire graphics vector is indefinite")
		xmin = xarray[i]
		xmax = xarray[i]
		for (i = i+1; i <= npts; i = i+1) {
		    if (! IS_INDEF(xarray[i])) {
		        if (xsize[i] < 0.) {
			    if (xarray[i] + xsize[i] / 2. < xmin)
			        xmin = xarray[i] + xsize[i] / 2.
			    if (xarray[i] - xsize[i] / 2. > xmax)
			        xmax = xarray[i] - xsize[i] / 2.
		        } else {
			    if (xarray[i] < xmin)
			        xmin = xarray[i]
			    if (xarray[i] > xmax)
			        xmax = xarray[i]
		        }
		    }
		}
		call gswind (gp, xmin, xmax, INDEF, INDEF)
	    }
	    if (IS_INDEF(GT_YMIN(gt)) || IS_INDEF(GT_YMAX(gt))) {
		do i = 1, npts
		    if (! IS_INDEF(yarray[i]))
			break
		if (i > npts)
		    call error (0, "Entire graphics vector is indefinite")
		ymin = yarray[i]
		ymax = yarray[i]
		for (i = i+1; i <= npts; i = i+1) {
		    if (! IS_INDEF(yarray[i])) {
		        if (ysize[i] < 0.) {
			    if (yarray[i] + ysize[i] / 2. < ymin)
			        ymin = yarray[i] + ysize[i] / 2.
			    if (yarray[i] - ysize[i] / 2. > ymax)
			        ymax = yarray[i] - ysize[i] / 2.
		        } else {
			    if (yarray[i] < ymin)
			        ymin = yarray[i]
			    if (yarray[i] > ymax)
			        ymax = yarray[i]
		        }
		    }
		}
		call gswind (gp, INDEF, INDEF, ymin, ymax)
	    }
	} then {
	    call erract (EA_WARN)
	    return
	}
	call gt_swind (gp, gt)

	# Flip axes if requested and min/max aren't explicitly set
	if ((IS_INDEF(GT_XMIN(gt)) || IS_INDEF(GT_XMAX(gt))) &&
	    CH_FLIP(ch, key, 1)) {
	    if (GT_TRANSPOSE(gt) == NO) {
	    	call ggwind (gp, xmin, xmax, ymin, ymax)
	    	call gswind (gp, xmax, xmin, ymin, ymax)
	    } else {
	    	call ggwind (gp, ymin, ymax, xmin, xmax)
	    	call gswind (gp, ymin, ymax, xmax, xmin)
	    }
	}
	if ((IS_INDEF(GT_YMIN(gt)) || IS_INDEF(GT_YMAX(gt))) &&
	    CH_FLIP(ch, key, 2)) {
	    if (GT_TRANSPOSE(gt) == NO) {
	    	call ggwind (gp, xmin, xmax, ymin, ymax)
	    	call gswind (gp, xmin, xmax, ymax, ymin)
	    } else {
	    	call ggwind (gp, ymin, ymax, xmin, xmax)
	    	call gswind (gp, ymax, ymin, xmin, xmax)
	    }
	}
	if (gt_geti (gt, GTTRANSPOSE) == NO)
	    call icg_g1r (ch, gp, gt, xarray, yarray, xplot, yplot, marker,
		    color, arrow, xsize, ysize, npts, key, marked, outliers)
	else
	    call icg_g1r (ch, gp, gt, yarray, xarray, yplot, xplot, marker,
		    color, arrow, ysize, xsize, npts, key, marked, outliers)
end

include	<gio.h>

procedure icg_g1r (ch, gp, gt, x, y, xplot, yplot, marker, color, arrow, xsize,
		   ysize, npts, key, marked, outliers)

pointer	ch				# CHART pointer
pointer	gp				# GIO pointer
pointer	gt				# GTOOLS pointer
real	x[npts]				# Ordinates
real	y[npts]				# Abscissas
real	xplot[npts]			# X-axis plotted positions
real	yplot[npts]			# Y-axis plotted positions
int	marker[npts]			# Markers
int	color[npts]			# Colors
int	arrow[npts]			# Direction of arrow if point off plot
real	xsize[npts]			# Marker sizes
real	ysize[npts]			# Marker sizes
int	npts				# Number of points
int	key 	    	    	    	# Graph key
int	marked				# Marker type for marked points
bool	outliers			# Mark outliers on the plot

real	xmin, xmax, ymin, ymax, temp, aspect_ratio
int 	i, pmcolor, gstati(), plcolor

begin
	# Plot objects beyond graph limits on the edge of the graph

	call ggwind (gp, xmin, xmax, ymin, ymax)

	if (xmin > xmax) {
	    temp = xmax
	    xmax = xmin
	    xmin = temp
	}
	if (ymin > ymax) {
	    temp = ymax
	    ymax = ymin
	    ymin = temp
	}
	do i = 1, npts {
	    arrow[i] = 0
	    if (x[i] > xmax) {
		xplot[i] = xmax
		arrow[i] = -1
	    } else if (x[i] < xmin) {
		xplot[i] = xmin
		arrow[i] = 1
	    } else {
		xplot[i] = x[i]
		arrow[i] = 0
	    }
	    
	    if (y[i] > ymax) {
		yplot[i] = ymax
		arrow[i] = UP + arrow[i]
	    } else if (y[i] < ymin) {
		yplot[i] = ymin
		arrow[i] = DOWN - arrow[i]
	    } else {
		yplot[i] = y[i]
		if (arrow[i] != 0)
		    arrow[i] = UP + 2 * arrow[i]
	    }
	}
	if (CH_SQUARE(ch, key)) {
	    aspect_ratio = (ymax - ymin) / (xmax - xmin)
	    call gsetr (gp, G_ASPECT, aspect_ratio)
	} else
	    call gsetr (gp, G_ASPECT, 0.0)
	call gt_labax (gp, gt)

	# Save current polymark color
	pmcolor = gstati(gp, G_PMCOLOR)
	plcolor = gstati(gp, G_PLCOLOR)

	if (outliers) {
	    do i = 1, npts {
	    	call draw_mark (gp, xplot[i], yplot[i], marker[i], color[i],
				xsize[i], ysize[i], CH_DEFMARKER(ch))
	    	if (arrow[i] > 0)
		    call plot_arrow (gp, xplot[i], yplot[i], arrow[i],
				     color[i])
	    }
	} else {
	    do i = 1, npts
		if (arrow[i] == 0)
	    	    call draw_mark (gp, xplot[i], yplot[i], marker[i],color[i],
				    xsize[i], ysize[i], CH_DEFMARKER(ch))
	}

	# Restore saved polymark color
	call gseti (gp, G_PMCOLOR, pmcolor)
	call gseti (gp, G_PLCOLOR, plcolor)
end

define	NGIOMARKS	10	# Number of GIO markers

# DRAW_MARK -- Draw the marker, either a GIO marker or a user marker

procedure draw_mark (gp, x, y, marker, color, xsize, ysize, defmark)
pointer	gp		# GIO pointer
real	x		# X-axis coordinate
real	y		# Y-axis coordinate
int	marker		# Marker type
int	color		# Marker color
real	xsize		# X-size of marker
real	ysize		# Y-size of marker
int	defmark		# Default marker

int	marks[NGIOMARKS]
data	marks /GM_POINT,GM_BOX,GM_PLUS,GM_CROSS,GM_DIAMOND,GM_HLINE,GM_VLINE,
		GM_HEBAR,GM_VEBAR,GM_CIRCLE/

begin
    call gseti(gp, G_PMCOLOR, color)
    call gseti(gp, G_PLCOLOR, color)
    if (defmark == EBARS) {
	if (marker != EBARS)
	    call gmark (gp, x, y, marks[marker], MSIZE, MSIZE)
	if (xsize != MSIZE)
	    call gmark (gp, x, y, GM_HEBAR, xsize, MSIZE)
	if (ysize != MSIZE)
	    call gmark (gp, x, y, GM_VEBAR, MSIZE, ysize)
    } else if (marker <= NGIOMARKS) {
	if (xsize > 0. && xsize < 1. && ysize > 0. && ysize < 1.)
	    call gmark (gp, x, y, marks[marker], xsize*GP_DEVASPECT(gp), ysize)
	else
	    call gmark (gp, x, y, marks[marker], xsize, ysize)
    } else
	switch (marker) {
	case EBARS: # errorbars
	    call gmark (gp, x, y, GM_HEBAR, xsize, MSIZE)
	    call gmark (gp, x, y, GM_VEBAR, MSIZE, ysize)
	}
end

# PLOT_ARROW -- Draw an arrow emanating from the specified point.

define	ARROW_NPTS	4	# Number of points in arrow
define	PI		3.14157
define	ARROW_SIZE	2.

procedure plot_arrow (gp, x, y, direction, color)

pointer	gp		# GIO pointer
real	x		# X-axis coordinate
real	y		# Y-axis coordinate
int	direction	# Direction arrow should point
int	color		# Marker color

real	xarrow[ARROW_NPTS,8], yarrow[ARROW_NPTS,8]

data	xarrow	/0.50, 0.00, 0.00, 0.50,
		 0.50, 0.00, 0.25, 0.50,
		 0.50, 0.25, 0.75, 0.50,
		 0.50, 0.75, 1.00, 0.50,
		 0.50, 1.00, 1.00, 0.50,
		 0.50, 1.00, 0.75, 0.50,
		 0.50, 0.75, 0.25, 0.50,
		 0.50, 0.25, 0.00, 0.50/

data	yarrow	/0.50, 0.75, 0.25, 0.50,
		 0.50, 0.25, 0.00, 0.50,
		 0.50, 0.00, 0.00, 0.50,
		 0.50, 0.00, 0.25, 0.50,
		 0.50, 0.25, 0.75, 0.50,
		 0.50, 0.75, 1.00, 0.50,
		 0.50, 1.00, 1.00, 0.50,
		 0.50, 1.00, 0.75, 0.50/

begin
	call gseti(gp, G_PMCOLOR, color)
	call gseti(gp, G_PLCOLOR, color)
	call gumark (gp, xarrow[1,direction], yarrow[1,direction], ARROW_NPTS,
		     x, y, ARROW_SIZE, ARROW_SIZE, NO)
end
