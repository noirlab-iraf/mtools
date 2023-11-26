# MARK -- Mark a feature.

include <smw.h>
include "mark.h"
include <gset.h>
include <pkg/gtools.h>

procedure mark (sh, gp, fname, fdefault, fwave, ftype, ltype, direction, pix)

pointer sh			# Dispersion parameter pointer
pointer	gp			# Graphics pointer
char	fname[SZ_FNAME]		# Feature name
real	fdefault		# Feature rest wavelength in original units
real	fwave			# Feature shifted wavelength in current units
int	ftype			# Feature type (emission or absorption)
int	ltype			# Label type (name or wavelength)
int	direction		# Label direction (horizontal or vertical)
real	pix[ARB]		# Data

real	x, y
real	mx, my, x1, x2, y1, y2, tick, gap
double	shdr_wl()
pointer	sp, format, label

define	TICK	.03	# Tick size in NDC
define	GAP	.02	# Gap size in NDC

begin
	call ggwind (gp, x1, x2, y1, y2)

	x = fwave
	y = pix[max (1, min (SN(sh), nint (shdr_wl (sh, double(x)))))]

	if ((x < min (x1, x2)) || (x > max (x1, x2)))
	    return

	call smark (sp)
	call salloc (format, SZ_LINE, TY_CHAR)
	call salloc (label, SZ_LINE, TY_CHAR)
	switch (ftype) {
	case EMISSION:
	    tick = TICK
	    gap = GAP
	    switch (direction) {
	    case HORIZONTAL:
		call strcpy ("u=90;h=c;v=b;s=0.5", Memc[format], SZ_LINE)
	    case VERTICAL:
		call strcpy ("u=180;h=c;v=b;s=0.5", Memc[format], SZ_LINE)
	    }
	case ABSORPTION:
	    tick = -TICK
	    gap = -GAP
	    switch (direction) {
	    case HORIZONTAL:
		call strcpy ("u=90;h=c;v=t;s=0.5", Memc[format], SZ_LINE)
	    case VERTICAL:
		call strcpy ("u=180;h=c;v=t;s=0.5", Memc[format], SZ_LINE)
	    }
	}

	call gctran (gp, x, y, mx, my, 1, 0)
	call gctran (gp, mx, my + gap, x1, y1, 0, 1)
	call gctran (gp, mx, my + gap + tick, x1, y2, 0, 1)
	call gline (gp, x1, y1, x1, y2)

	call gctran (gp, mx, my + tick + 2 * gap, x1, y2, 0, 1)
	switch (ltype) {
	case NAME:
	    call sprintf (Memc[label], SZ_LINE, "%s")
		call pargstr (fname)
	    call gtext (gp, x1, y2, Memc[label], Memc[format])
	case WAVELENGTH:
	    call sprintf (Memc[label], SZ_LINE, "%4.0f")
		call pargr (fdefault)
	    call gtext (gp, x1, y2, Memc[label], Memc[format])
	default:
	}

	call sfree (sp)
	call gflush (gp)
end
