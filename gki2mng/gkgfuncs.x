# Formerly gio$gki/gkiprint.x

include	<config.h>
include	<mach.h>
include	<gset.h>
include	<gki.h>
include	<gio.h>

# GKG_INSTALL -- Install the GKI to IGI kernel as a graphics kernel
# device driver.  The device table DD consists of an array of the entry
# point addresses for the driver procedures.  If a driver does not implement
# a particular instruction the table entry for that procedure may be set
# to zero, causing the interpreter to ignore the instruction.

procedure gkg_install (dd, out_fd)

int	dd[ARB]			# device table to be initialized
int	out_fd			# output file

int	fd, stream
common	/gkgcom/ fd, stream

extern	gkg_openws(), gkg_closews(), gkg_mftitle(), gkg_clear(), gkg_cancel()
extern	gkg_flush(), gkg_polyline(), gkg_polymarker(), gkg_text()
extern	gkg_fillarea(), gkg_putcellarray(), gkg_setcursor(), gkg_plset()
extern	gkg_pmset(), gkg_txset(), gkg_faset(), gkg_getcursor()
extern	gkg_getcellarray(), gkg_escape(), gkg_setwcs(), gkg_getwcs()
extern	gkg_unknown(), gkg_reactivatews(), gkg_deactivatews()

real	wx1, wx2, wy1, wy2, sx1, sx2, sy1, sy2
int	wcsflag, tsflag, just
common	/gkxcom/ wx1, wx2, wy1, wy2, sx1, sx2, sy1, sy2, wcsflag, tsflag, just

begin
	wcsflag = 0
	tsflag = 0
	just = 3

	# Set the GDC internal parameters.
	fd = out_fd
	stream = NULL

	# Install the device driver.
	call zlocpr (gkg_openws,	dd[GKI_OPENWS])
	call zlocpr (gkg_closews,	dd[GKI_CLOSEWS])
	call zlocpr (gkg_reactivatews,	dd[GKI_REACTIVATEWS])
	call zlocpr (gkg_deactivatews,	dd[GKI_DEACTIVATEWS])
	call zlocpr (gkg_mftitle,	dd[GKI_MFTITLE])
	call zlocpr (gkg_clear,		dd[GKI_CLEAR])
	call zlocpr (gkg_cancel,	dd[GKI_CANCEL])
	call zlocpr (gkg_flush,		dd[GKI_FLUSH])
	call zlocpr (gkg_polyline,	dd[GKI_POLYLINE])
	call zlocpr (gkg_polymarker,	dd[GKI_POLYMARKER])
	call zlocpr (gkg_text,		dd[GKI_TEXT])
	call zlocpr (gkg_fillarea,	dd[GKI_FILLAREA])
	call zlocpr (gkg_putcellarray,	dd[GKI_PUTCELLARRAY])
	call zlocpr (gkg_setcursor,	dd[GKI_SETCURSOR])
	call zlocpr (gkg_plset,		dd[GKI_PLSET])
	call zlocpr (gkg_pmset,		dd[GKI_PMSET])
	call zlocpr (gkg_txset,		dd[GKI_TXSET])
	call zlocpr (gkg_faset,		dd[GKI_FASET])
	call zlocpr (gkg_getcursor,	dd[GKI_GETCURSOR])
	call zlocpr (gkg_getcellarray,	dd[GKI_GETCELLARRAY])
	call zlocpr (gkg_escape,	dd[GKI_ESCAPE])
	call zlocpr (gkg_setwcs,	dd[GKI_SETWCS])
	call zlocpr (gkg_getwcs,	dd[GKI_GETWCS])
	call zlocpr (gkg_unknown,	dd[GKI_UNKNOWN])
end


# GKG_CLOSE -- Close the GKG kernel.

procedure gkg_close()

int	fd, stream
common	/gkgcom/ fd, stream

begin
#	call fprintf (fd, "# GKG_CLOSE\n")
end


# GKG_GRSTREAM -- Set the FD of the graphics stream, from which we shall read
# metacode instructions and to which we shall return cell arrays and cursor
# values.

procedure gkg_grstream (graphics_stream)

int	graphics_stream		# FD of the new graphics stream
int	fd, stream
common	/gkgcom/ fd, stream

begin
	stream = graphics_stream
end


# GKG_OPENWS -- Open the named workstation.

procedure gkg_openws (devname, n, mode)

short	devname[ARB]		# device name
int	n			# length of device name
int	mode			# access mode

int	fd, stream
common	/gkgcom/ fd, stream

begin
#	call fprintf (fd, "# GKG_OPENWS\n")
end


# GKG_CLOSEWS -- Close the named workstation.

procedure gkg_closews (devname, n)

short	devname[ARB]		# device name
int	n			# length of device name

int	fd, stream
common	/gkgcom/ fd, stream

begin
#	call fprintf (fd, "# GKG_CLOSEWS\n")
end


# GKG_REACTIVATEWS -- Reactivate the workstation (enable graphics).

procedure gkg_reactivatews (flags)

int	flags			# action flags
int	fd, stream
common	/gkgcom/ fd, stream

begin
#	call fprintf (fd, "# GKG_REACTIVATEWS\n")
end


# GKG_DEACTIVATEWS -- Deactivate the workstation (disable graphics).

procedure gkg_deactivatews (flags)

int	flags			# action flags
int	fd, stream
common	/gkgcom/ fd, stream

begin
#	call fprintf (fd, "# GKG_DEACTIVATEWS\n")
end


# GKG_MFTITLE -- Metafile title or comment.  A nonfunctional instruction used
# to document a metafile.

procedure gkg_mftitle (title, n)

short	title[ARB]		# title string
int	n			# length of title string
pointer	sp, buf
int	fd, stream
common	/gkgcom/ fd, stream

begin
	call smark (sp)
	call salloc (buf, n, TY_CHAR)

	call achtsc (title, Memc[buf], n)
	Memc[buf+n] = EOS

#	call fprintf (fd, "# MF title %s\n")
#	    call pargstr (Memc[buf])
	
	call sfree (sp)
end


# GKG_CLEAR -- Clear the workstation screen.

procedure gkg_clear (dummy)

int	dummy			# not used at present
int	fd, stream
common	/gkgcom/ fd, stream

begin
	call fprintf (fd, "erase\n")
end


# GKG_CANCEL -- Cancel output.

procedure gkg_cancel (dummy)

int	dummy			# not used at present
int	fd, stream
common	/gkgcom/ fd, stream

begin
#	call fprintf (fd, "# GKG_CANCEL\n")
end


# GKG_FLUSH -- Flush output.

procedure gkg_flush (dummy)

int	dummy			# not used at present
int	fd, stream
common	/gkgcom/ fd, stream

begin
#	call fprintf (fd, "# GKG_FLUSH\n")
end


# GKG_POLYLINE -- Draw a polyline.

procedure gkg_polyline (p, npts)

short	p[ARB]			# points defining line
int	npts			# number of points, i.e., (x,y) pairs
int	fd, stream
common	/gkgcom/ fd, stream

begin
	# Print statistics on polyline.
	call gkg_pstat (fd, p, npts, "polyline")
end


# GKG_POLYMARKER -- Draw a polymarker.

procedure gkg_polymarker (p, npts)

short	p[ARB]			# points defining line
int	npts			# number of points, i.e., (x,y) pairs
int	fd, stream
common	/gkgcom/ fd, stream

begin
	# Print statistics on polymarker.
	call gkg_pstat (fd, p, npts, "polymarker")
end


# GKG_FILLAREA -- Fill a closed area.

procedure gkg_fillarea (p, npts)

short	p[ARB]			# points defining line
int	npts			# number of points, i.e., (x,y) pairs
int	fd, stream
common	/gkgcom/ fd, stream

begin
	# Print statistics on the fillarea polygon.
	call gkg_pstat (fd, p, npts, "fillarea")
end


# GKG_TEXT -- Draw a text string.

procedure gkg_text (x, y, text, n)

int	x, y			# where to draw text string
short	text[ARB]		# text string
int	n			# number of characters

pointer	sp, buf
int	fd, stream
common	/gkgcom/ fd, stream
real	wx1, wx2, wy1, wy2, sx1, sx2, sy1, sy2
int	wcsflag, tsflag, just
common	/gkxcom/ wx1, wx2, wy1, wy2, sx1, sx2, sy1, sy2, wcsflag, tsflag, just
real	xx, yy

begin
	if (wcsflag == 1 && tsflag < 3)
	    return

	call smark (sp)
	call salloc (buf, n, TY_CHAR)

	call achtsc (text, Memc[buf], n)
	Memc[buf+n] = EOS

	xx = (real(x) / GKI_MAXNDC - sx1) * (wx2 - wx1) / (sx2 - sx1) + wx1
	yy = (real(y) / GKI_MAXNDC - sy1) * (wy2 - wy1) / (sy2 - sy1) + wy1
	if (wcsflag == 1 && tsflag == 3)
	    call fprintf (fd, "xlabel %s\n")
	else if (wcsflag == 1 && tsflag == 4)
	    call fprintf (fd, "ylabel %s\n")
	else {
	    call fprintf (fd, "relocate %g  %g\nputlabel %d %s\n")
	    call pargr (xx)
	    call pargr (yy)
	    call pargi (just)
	}
	call pargstr (Memc[buf])
	
	call sfree (sp)
end


# GKG_PUTCELLARRAY -- Draw a cell array, i.e., two dimensional array of pixels
# (greylevels or colors).

procedure gkg_putcellarray (m, nx, ny, x1,y1, x2,y2)

int	nx, ny			# number of pixels in X and Y
short	m[nx,ny]		# cell array
int	x1, y1			# lower left corner of output window
int	x2, y2			# lower left corner of output window

int	fd, stream
common	/gkgcom/ fd, stream

begin
#	call fprintf (fd, "# GKG_PUTCELLARRAY\n")
end


# GKG_GETCELLARRAY -- Input a cell array, i.e., two dimensional array of pixels
# (greylevels or colors).

procedure gkg_getcellarray (nx, ny, x1,y1, x2,y2)

int	nx, ny			# number of pixels in X and Y
int	x1, y1			# lower left corner of input window
int	x2, y2			# lower left corner of input window

int	fd, stream
common	/gkgcom/ fd, stream

begin
#	call fprintf (fd, "# GKG_GETCELLARRAY\n")
end


# GKG_SETCURSOR -- Set the position of a cursor.

procedure gkg_setcursor (x, y, cursor)

int	x, y			# new position of cursor
int	cursor			# cursor to be set
int	fd, stream
common	/gkgcom/ fd, stream

begin
#	call fprintf (fd, "# GKG_SETCURSOR\n")
end


# GKG_GETCURSOR -- Get the position of a cursor.

procedure gkg_getcursor (cursor)

int	cursor
int	fd, stream
common	/gkgcom/ fd, stream

begin
#	call fprintf (fd, "# GKG_GETCURSOR\n")
end


# GKG_PLSET -- Set the polyline attributes.

procedure gkg_plset (gki)

short	gki[ARB]		# attribute structure
int	fd, stream
common	/gkgcom/ fd, stream
real	wx1, wx2, wy1, wy2, sx1, sx2, sy1, sy2
int	wcsflag, tsflag, just
common	/gkxcom/ wx1, wx2, wy1, wy2, sx1, sx2, sy1, sy2, wcsflag, tsflag, just

begin
	if (wcsflag == 1)
	    return
	call fprintf (fd, "ltype %d\nlweight %0.2f\n")
	    call pargs (gki[GKI_PLSET_LT]-1)
	    call pargr (GKI_UNPACKREAL (gki[GKI_PLSET_LW]))
end


# GKG_PMSET -- Set the polymarker attributes.

procedure gkg_pmset (gki)

short	gki[ARB]		# attribute structure
int	fd, stream
common	/gkgcom/ fd, stream

begin
#	call fprintf (fd, "# GKG_PMSET\n")
end


# GKG_FASET -- Set the fillarea attributes.

procedure gkg_faset (gki)

short	gki[ARB]		# attribute structure
int	fd, stream
common	/gkgcom/ fd, stream

begin
#	call fprintf (fd, "# GKG_FASET\n")
end


# GKG_TXSET -- Set the text drawing attributes.

procedure gkg_txset (gki)

short	gki[ARB]		# attribute structure
int	fd, stream
common	/gkgcom/ fd, stream
int	up, i1, i2, ctoi
real	wx1, wx2, wy1, wy2, sx1, sx2, sy1, sy2
int	wcsflag, tsflag, just
common	/gkxcom/ wx1, wx2, wy1, wy2, sx1, sx2, sy1, sy2, wcsflag, tsflag, just

begin
	if (wcsflag == 1) {
	    tsflag = tsflag + 1
	    if (tsflag < 5)
		return
	}

	i1 = 1
	i2 = ctoi (gki[GKI_TXSET_UP], i1, up)
	switch (gki[GKI_TXSET_HJ]) {
	case GT_LEFT:
	    just = 1
	case GT_CENTER:
	    just = 2
	case GT_RIGHT:
	    just = 3
	}
	switch (gki[GKI_TXSET_VJ]) {
	case GT_TOP:
	    just = just
	case GT_CENTER:
	    just = 3 + just
	case GT_BOTTOM:
	    just = 6 + just
	}
	call fprintf (fd, "angle %d\nexpand %0.2f\n")
	    switch (gki[GKI_TXSET_P]) {
	    case GT_RIGHT:
		call pargi (up - 90)
	    case GT_UP:
		call pargi (up)
	    case GT_LEFT:
		call pargi (up + 90)
	    case GT_DOWN:
		call pargi (up + 180)
	    }
	    call pargr (GKI_UNPACKREAL (gki[GKI_TXSET_SZ]))
end


# GKG_ESCAPE -- Device dependent instruction.

procedure gkg_escape (fn, instruction, nwords)

int	fn			# function code
short	instruction[ARB]	# instruction data words
int	nwords			# length of instruction
int	fd, stream
common	/gkgcom/ fd, stream

begin
	call fprintf (fd, "!!")

	# Dump the instruction.
	call gkg_dump (fd, instruction, nwords)
end


# GKG_SETWCS -- Set the world coordinate systems.  Internal GIO instruction.

procedure gkg_setwcs (wcs, nwords)

short	wcs[ARB]		# WCS data
int	nwords			# number of words of data

int	i, nwcs
pointer	sp, wcs_temp, w
int	fd, stream
common	/gkgcom/ fd, stream
real	wx1, wx2, wy1, wy2, sx1, sx2, sy1, sy2
int	wcsflag, tsflag, just
common	/gkxcom/ wx1, wx2, wy1, wy2, sx1, sx2, sy1, sy2, wcsflag, tsflag, just

begin
    if (wcsflag == 0) {
	call smark (sp)
	call salloc (wcs_temp, LEN_WCSARRAY, TY_STRUCT)

	nwcs = nwords * SZ_SHORT / SZ_STRUCT / LEN_WCS
	if (nwcs > 1) {
	    call amovi (wcs, Memi[wcs_temp], nwcs * LEN_WCS)

	    do i = 1, nwcs {
		w = ((i - 1) * LEN_WCS) + wcs_temp
		if ((WCS_WX1(w) > EPSILON) ||
		    (abs(1.0 - WCS_WX2(w)) > EPSILON) ||
		    (WCS_WY1(w) > EPSILON) ||
		    (abs(1.0 - WCS_WY2(w)) > EPSILON)) {

		    call fprintf (fd, "limits %g %g %g %g\n")
			call pargr (WCS_WX1(w))
			call pargr (WCS_WX2(w))
			call pargr (WCS_WY1(w))
			call pargr (WCS_WY2(w))

#		    call fprintf (fd, "location %g %g %g %g\n")
#			call pargr (WCS_SX1(w))
#			call pargr (WCS_SX2(w))
#			call pargr (WCS_SY1(w))
#			call pargr (WCS_SY2(w))

		    if (WCS_XTRAN(w) == YES)
			call fprintf (fd, "xflip\n")
		    if (WCS_YTRAN(w) == YES)
			call fprintf (fd, "yflip\n")

		    call fprintf (fd, "box\n")

		    wx1 = WCS_WX1(w)
		    wx2 = WCS_WX2(w)
		    wy1 = WCS_WY1(w)
		    wy2 = WCS_WY2(w)

		    sx1 = WCS_SX1(w)
		    sx2 = WCS_SX2(w)
		    sy1 = WCS_SY1(w)
		    sy2 = WCS_SY2(w)

		}
	    }
	}

	call sfree (sp)
    }
    wcsflag = mod (wcsflag+1, 2)
    tsflag = 0
end


# GKG_GETWCS -- Get the world coordinate systems.  Internal GIO instruction.

procedure gkg_getwcs (wcs, nwords)

short	wcs[ARB]		# WCS data
int	nwords			# number of words of data
int	fd, stream
common	/gkgcom/ fd, stream

begin
#	call fprintf (fd, "# GKG_GETWCS\n")
end


# GKG_UNKNOWN -- The unknown instruction.  Called by the interpreter whenever
# an unrecognized opcode is encountered.  Should never be called.

procedure gkg_unknown (gki)

short	gki[ARB]		# the GKI instruction
int	fd, stream
common	/gkgcom/ fd, stream

begin
#	call fprintf (fd, "# GKG_UNKNOWN\n")
end


# GKG_PSTAT -- Compute and print on the standard error output a statistical
# summary of a sequence of (x,y) points.  If verbose mode is enabled, follow
# this by the values of the points themselves.

procedure gkg_pstat (fd, p, npts, label)

int	fd			# output file
short	p[npts]			# array of points, i.e., (x,y) pairs
int	npts			# number of points
char	label[ARB]		# type of instruction

int	i
real	wx1, wx2, wy1, wy2, sx1, sx2, sy1, sy2
int	wcsflag, tsflag, just
common	/gkxcom/ wx1, wx2, wy1, wy2, sx1, sx2, sy1, sy2, wcsflag, tsflag, just
real	xx, yy

begin
	if (wcsflag == 1)
	    return
	for (i=1;  i <= npts * 2;  i=i+2) {
	    xx = (real(p[i])  /GKI_MAXNDC - sx1) * (wx2-wx1) / (sx2-sx1) + wx1
	    yy = (real(p[i+1])/GKI_MAXNDC - sy1) * (wy2-wy1) / (sy2-sy1) + wy1
	    if (i == 1)
		call fprintf (fd, "    relocate %g %g\n")
	    else
	    	call fprintf (fd, "    draw     %g %g\n")
	    call pargr (xx)
	    call pargr (yy)
	}
end


# GKG_DUMP -- Print a sequence of metacode words as a table, formatted eight
# words per line, in decimal.

procedure gkg_dump (fd, data, nwords)

int	fd			# output file
short	data[ARB]		# metacode data
int	nwords			# number of words of data
int	i

begin
	if (nwords <= 0)
	    return

	call fprintf (fd, "\t")

	for (i=1;  i <= nwords;  i=i+1) {
	    if (i > 1 && mod (i-1, 8) == 0)
		call fprintf (fd, "\n\t")
	    call fprintf (fd, "%7d")
		call pargs (data[i])
	}

	call fprintf (fd, "\n")
end
