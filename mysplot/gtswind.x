# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include <mach.h>
#include <pkg/xtools/gtools.h>
include	"gtools.h"
	
# GT_SWIND -- Set graphics window.

procedure my_gt_swind (gp, gt)

pointer	gp			# GIO pointer
pointer	gt			# GTOOLS pointer

real	xmin, xmax, dx, ymin, ymax, dy

begin
	if (gt != NULL) {
	    if (GT_TRANSPOSE(gt) == NO) {
	        call gseti (gp, G_XTRAN, GT_XTRAN(gt))
	        call gseti (gp, G_YTRAN, GT_YTRAN(gt))
	    } else {
	        call gseti (gp, G_YTRAN, GT_XTRAN(gt))
	        call gseti (gp, G_XTRAN, GT_YTRAN(gt))
	    }
	    call ggwind (gp, xmin, xmax, ymin, ymax)
	    if (GT_XTRAN(gt) == GW_LINEAR)
	    	dx = xmax - xmin
	    else
	    	dx = log10 (xmax) - log10 (xmin)
	    if (GT_YTRAN(gt) == GW_LINEAR)
	    	dy = ymax - ymin
	    else
	    	dy = log10 (ymax) - log10 (ymin)

	    if (IS_INDEF (GT_XMIN(gt))) {
		if (GT_XTRAN(gt) == GW_LINEAR)
		    xmin = xmin - GT_XBUF(gt) * dx
		else
		    xmin = 10. ** (log10 (xmin) - GT_XBUF(gt) * dx)
	    } else
		xmin = GT_XMIN(gt)

	    if (IS_INDEF (GT_XMAX(gt))) {
		if (GT_XTRAN(gt) == GW_LINEAR)
		    xmax = xmax + GT_XBUF(gt) * dx
		else
		    xmax = 10. ** (log10 (xmax) + GT_XBUF(gt) * dx)
	    } else
		xmax = GT_XMAX(gt)

	    if (IS_INDEF (GT_YMIN(gt))) {
		if (GT_YTRAN(gt) == GW_LINEAR)
		    ymin = ymin - GT_YBUF(gt) * dy
		else
		    ymin = 10. ** (log10 (ymin) - GT_YBUF(gt) * dy)
	    } else
		ymin = GT_YMIN(gt)

	    if (IS_INDEF (GT_YMAX(gt))) {
		if (GT_YTRAN(gt) == GW_LINEAR)
		    ymax = ymax + GT_YBUF(gt) * dy
		else
		    ymax = 10. ** (log10 (ymax) + GT_YBUF(gt) * dy)
	    } else
		ymax = GT_YMAX(gt)

	    if (GT_XFLIP(gt) == YES) {
		dx = xmin
		xmin = xmax
		xmax = dx
	    }
	    if (GT_YFLIP(gt) == YES) {
		dy = ymin
		ymin = ymax
		ymax = dy
	    }

	    if (GT_TRANSPOSE(gt) == NO)
	        call gswind (gp, xmin, xmax, ymin, ymax)
	    else
	        call gswind (gp, ymin, ymax, xmin, xmax)
	}
end
