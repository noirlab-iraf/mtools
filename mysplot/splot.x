task mysplot

include <mach.h>
include	<error.h>
include	<imhdr.h>
include	<gset.h>
include	<pkg/gtools.h>
include <smw.h>
include	<units.h>
include "mark.h"

define	SPEED_OF_LIGHT	299792.5	# km/sec

define	KEY		"mysplot$splot.key"
define	HELP		"mysplot$stshelp.key"
define	PROMPT		"mysplot options"

define	OPTIONS	",auto,zero,xydraw,histogram,nosysid,wreset,flip,overplot,"
define	NOPTIONS	8
define	AUTO		1	# Option number for auto graph
define	ZERO		2	# Option number of zero y minimum
define	XYDRAW		3	# Draw connection X,Y pairs
define	HIST		4	# Draw histogram style lines
define	NOSYSID		5	# Don't include system id
define	WRESET		6	# Reset window for each new spectrum
define	FLIP		7	# Flip spectra
define	OVERPLOT	8	# Overplot toggle


# SPLOT -- Plot an image line and play with it - Most appropriate for spectra

procedure mysplot ()

int	list
int	i, j, npts, nline, nband, nap
int	wc, key, keyu
real	wx, wy
double	w1, u1, w2, u2, w0, wpc
real	avg_pix, sigma_pix

int	fd1, fd2, ng, hline, hlines
int	newgraph, newimage, overplot, options[NOPTIONS]
pointer	sp, image, units, units1, units2, units3, cmd, save1, save2
pointer	gp, gt, im, mw, x, y, sh, xg, yg, sg, lg, pg, hptr
bool	wave_scl, fnu

pointer	gopen(), gt_init()
int	clgcur(), imtopen(), imtgetim(), imaccess(), gt_geti(), nowhite()
real	clgetr(), gt_getr()
double	clgetd(), shdr_wl()
bool	streq(), fp_equald()
errchk	getimage, fun_do, ans_hdr, un_changer

### Start my additional definitions
define	MAX_FEATURES		15000	# maximum number of lines

char	coordlist[SZ_FNAME]		# name of coordinate list file
char	funits[SZ_UNITS]		# Units for features
char	fname[SZ_FNAME, MAX_FEATURES]	# name of feature
real	frest[MAX_FEATURES]		# rest wavelengths of features
					# in current units
real	fdefault[MAX_FEATURES]		# rest wavelengths of features
					# in default units (funits)
int	ftype[MAX_FEATURES]		# emission or absorption
int	ltype[MAX_FEATURES]		# name or wavelength label
int	ztype[MAX_FEATURES]		# shifted or non-shifted line
bool	visible[MAX_FEATURES]		# is feature visible?
int	nfeatures			# number of features read
int	nshifted			# number of features that shift with z
int	nextra				# number of extra user marked features
bool	show_shifted			# flag to show shifted features
bool	show_unshifted			# flag to show unshifted features
int	direction			# direction to plot features (hor,vert)
int	label_type			# type of label (name, wavelength)
real	z				# redshift
real	vobs				# velocity (km/s)
real	obs_lambda			# center wavelength of last EW measure
real	delta, rest_wavelength, myeqwidth(), myeqwidth_cp(), mygfit()
int	feature, scan(), strlen(), nearest()
bool	reply, clgetb()
char	label[SZ_FNAME], tlabel[SZ_FNAME]
int	clgwrd(), xscaling
pointer	pfunits, un_open()
### End my additional definitions

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (units, SZ_FNAME, TY_CHAR)
	call salloc (units1, SZ_FNAME, TY_CHAR)
	call salloc (units2, SZ_FNAME, TY_CHAR)
	call salloc (units3, SZ_FNAME, TY_CHAR)
	call salloc (cmd, SZ_FNAME, TY_CHAR)
	call salloc (save1, SZ_FNAME, TY_CHAR)
	call salloc (save2, SZ_FNAME, TY_CHAR)

	# Get task parameters.

	call clgstr ("images", Memc[image], SZ_FNAME)
	list = imtopen (Memc[image])
	call clgstr ("save_file", Memc[save1], SZ_FNAME)
	call clgstr ("options", Memc[save2], SZ_FNAME)
	call xt_gids (Memc[save2], OPTIONS, options, NOPTIONS)
	call clgstr ("units", Memc[units], SZ_FNAME)
	call mktemp ("tmp$splot", Memc[save2], SZ_FNAME)

	# Allocate space for User area
	x = NULL
	y = NULL

	# Initialize
	im = NULL
	sh = NULL
	fd1 = NULL
	fd2 = NULL
	hptr = NULL
	ng = 0
	hline = 1
	nline = 0
	nband = 0
	if (nowhite (Memc[units], Memc[units1], SZ_FNAME) == 0)
	    call strcpy ("display", Memc[units], SZ_FNAME)
	call strcpy (Memc[units], Memc[units1], SZ_FNAME)
	call strcpy (Memc[units], Memc[units2], SZ_FNAME)
	w0 = INDEFD
	wpc = INDEFD

	call clgstr ("graphics", Memc[cmd], SZ_FNAME)
	gp = gopen (Memc[cmd], NEW_FILE+AW_DEFER, STDGRAPH)
	call gseti (gp, G_WCS, 1)
#	call gseti (gp, G_YNMINOR, 0)

	gt = gt_init()
	call gt_setr (gt, GTXMIN, clgetr ("xmin"))
	call gt_setr (gt, GTXMAX, clgetr ("xmax"))
	call gt_setr (gt, GTYMIN, clgetr ("ymin"))
	call gt_setr (gt, GTYMAX, clgetr ("ymax"))
	if (options[ZERO] == YES)
	    call gt_setr (gt, GTYMIN, 0.)
	if (options[HIST] == YES)
	    call gt_sets (gt, GTTYPE, "histogram")
	else
	    call gt_sets (gt, GTTYPE, "line")
	call gt_seti (gt, GTXFLIP, options[FLIP])
	if (options[NOSYSID] == YES)
	    call gt_seti (gt, GTSYSID, NO)

	### Start my additions
	# Read in coordinate list
	z = 0
	nfeatures = 0
	call clgstr ("shifted_lines", coordlist, SZ_FNAME)
	call linelist (coordlist, nfeatures, fname, frest, ftype, MAX_FEATURES)
	nshifted = nfeatures
	call clgstr ("unshifted_lines", coordlist, SZ_FNAME)
	call clgstr ("line_units", funits, SZ_UNITS)
	call linelist (coordlist, nfeatures, fname, frest, ftype, MAX_FEATURES)

	# Set feature defaults
	direction = VERTICAL
	label_type = clgwrd ("label_type", coordlist, SZ_FNAME, LABELS)
	show_shifted = clgetb ("show_shifted")
	show_unshifted = clgetb ("show_unshifted")
	do i = 1, nshifted {
	    visible[i] = true
	    ltype[i] = label_type
	    ztype[i] = SHIFTED
	}	    
	do i = nshifted+1, nfeatures {
	    visible[i] = true
	    ltype[i] = NAME
	    ztype[i] = NOTSHIFTED
	}	    
	nextra = 0
	call strcpy ("CR", label, SZ_FNAME)
	call amovr (frest, fdefault, nfeatures)
	pfunits = un_open (funits)
	xscaling = GW_LINEAR
	### End my additions

	while (imtgetim (list, Memc[image], SZ_FNAME) != EOF) {

	    # Initialize to plot a wavelength scale
	    wave_scl = true

	    # Open image and get pixels
	    if (imaccess (Memc[image], READ_ONLY) == NO) {
		call eprintf ("Can't get image %s\n")
		    call pargstr (Memc[image])
		next
	    }
	    call getimage (Memc[image], nline, nband, nap, wave_scl,
		w0, wpc, Memc[units], im, mw, sh, gt)
	    x = SX(sh)
	    y = SY(sh)
	    npts = SN(sh)
	    newimage = YES
	    overplot = options[OVERPLOT]
	    ### Start my additions
	    nextra = 0
	    ### End my additions

	    # Enter cursor loop with 'r' redraw.
	    key = 'r'
	    repeat {
	        switch (key) {
		### Start my additions
		case '!': # Toggle log/linear x-axis
		    if (xscaling == GW_LINEAR) {
			xscaling = GW_LOG
			call gt_sets (gt, GTXTRAN, "log")
			call printf ("x-axis scaled in log wavelength")
		    }
		    else {
			xscaling = GW_LINEAR
			call gt_sets (gt, GTXTRAN, "linear")
			call printf ("x-axis scaled linearly in wavelength")
		    }
		    newgraph = YES

		case '*': # Mark a cosmic ray, etc
		    nextra = nextra + 1
		    i = max (1, min (npts, nint (shdr_wl (sh, double(wx)))))
		    if (wy > Memr[y+i-1] ) {
		    	do j = max(1, i-2), min(npts, i+2)
			    if (Memr[y+j-1] > Memr[y+i-1])
			    	i = j
		    	ftype[nfeatures+nextra] = EMISSION
		    } else {
		    	do j = max(1, i-2), min(npts, i+2)
			    if (Memr[y+j-1] < Memr[y+i-1])
			    	i = j
		    	ftype[nfeatures+nextra] = ABSORPTION
		    }
		    frest[nfeatures+nextra] = wx
		    call un_changer (UN(sh), funits, wx, 1, NO)
		    fdefault[nfeatures+nextra] = wx
		    ztype[nfeatures+nextra] = NOTSHIFTED
		    ltype[nfeatures+nextra] = NAME
		    visible[nfeatures+nextra] = true
		    call printf ("label (%s): ")
		    call pargstr (label)
		    call flush (STDOUT)
		    if (scan() != EOF)
		    	call gargstr (tlabel, SZ_FNAME)
		    if (strlen (tlabel) > 0)
			call strcpy (tlabel, label, SZ_FNAME)
		    call strcpy (label, fname[1, nfeatures+nextra], SZ_FNAME)
		    call mark (sh,gp,fname[1,nfeatures+nextra],
			       fdefault[nfeatures+nextra],
			       frest[nfeatures+nextra],ftype[nfeatures+nextra],
			       ltype[nfeatures+nextra], direction, Memr[y])

		case '_': # Remove feature
		    feature = nearest (wx, frest, visible, ztype, show_shifted,
				       show_unshifted, z, nfeatures+nextra)
		    if (feature > 0)
			visible[feature] = false
		    newgraph = YES

		case '+': # Make all features visible
		    do i = 1, nfeatures+nextra
			visible[i] = true
		    show_shifted = true
		    show_unshifted = true
		    newgraph = YES

		case ']': # Toggle showing of shifted features
		    show_shifted = ! show_shifted
		    newgraph = YES

		case '[': # Toggle showing of unshifted features
		    show_unshifted = ! show_unshifted
		    newgraph = YES

		case '{': # Toggle direction of feature labels (horiz, vert)
		    direction = mod (direction, 2) + 1
		    newgraph = YES

		case '}': # Toggle type of feature label (name, wavelength)
		    label_type = mod (label_type, 2) + 1
		    do i = 1, nshifted
			ltype[i] = label_type
		    newgraph = YES

		case '^': # Enter a redshift or observed velocity
		    call printf ("z: ")
		    call flush (STDOUT)
		    if (scan() != EOF) {
		        call gargr (z)
			if (z > 10.)
			    z = sqrt((1 + z / SPEED_OF_LIGHT) / 
				     (1 - z / SPEED_OF_LIGHT)) - 1
		    }
		    if (show_shifted)
			newgraph = YES

		case '|': # Print current redshift
		    vobs = SPEED_OF_LIGHT * ((1+z)*(1+z)-1) / ((1+z)*(1+z)+1)
		    call printf ("z = %8.5f, vobs = %.2f km/s")
			call pargr (z)
			call pargr (vobs)
		    call flush (STDOUT)

		case '@':
		    # line identification
		    feature = nfeatures + 1
		    while (feature > nfeatures) {
			call printf ("Rest wavelength: ")
			call flush (STDOUT)
			if (scan() == EOF)
			    break
			call gargr (rest_wavelength)
			delta = MAX_REAL
			for (i=1;i<=nshifted; i=i+1)
			    if (abs(rest_wavelength - fdefault[i]) < delta) {
				delta = abs(rest_wavelength - fdefault[i])
				feature = i
			    }
		    }
		    z = (obs_lambda - fdefault[feature]) / fdefault[feature]
		    vobs = SPEED_OF_LIGHT * ((1+z)*(1+z)-1) / ((1+z)*(1+z)+1)
		    call printf ("z = %8.5f, vobs = %.2f km/s     Save (%b): ")
			call pargr (z)
			call pargr (vobs)
			call pargb (reply)
		    call flush(STDOUT)
	    	    if (scan() != EOF)
			call gargb(reply)
		    if (reply) {
			call ans_hdr (sh, newimage, key, Memc[save1],
				      Memc[save2], fd1, fd2)
			if (fd1 != NULL) {
			   call fprintf(fd1,"rest_wavelength: %4d  z: %8.5f  v: %.2f\n")
			    	call pargi (int(fdefault[feature]))
			    	call pargr (z)
				call pargr (vobs)
			}
			if (fd2 != NULL) {
			   call fprintf(fd2,"rest_wavelength: %4d  z: %8.5f  v: %.2f\n")
			    	call pargi (int(fdefault[feature]))
			    	call pargr (z)
				call pargr (vobs)
			}
		    }
		    if (show_shifted)
			newgraph = YES
		### End my additions

	        case ':':
		    if (Memc[cmd] == '/')
		        call gt_colon (Memc[cmd], gp, gt, newgraph)
		    else {
			call splot_colon (Memc[cmd], options, gp, gt, sh,
			    wx, wy, Memc[units], Memc[save1], Memc[save2],
			    fd1, fd2,  newgraph)
			overplot = options[OVERPLOT]
			if (sh == NULL) {
			    call getimage (Memc[image], nline, nband, nap,
				wave_scl, w0, wpc, Memc[units], im, mw, sh, gt)
			    x = SX(sh)
			    y = SY(sh)
			    npts = SN(sh)
			    newgraph = YES
			    newimage = YES
			}
		    }

	        case 'a': # Autoexpand
		    call auto_exp (gp, gt, key, wx, Memr[x], Memr[y], npts)

	        case 'b': # Toggle base to 0.0
		    if (options[ZERO] == NO) {
			call gt_setr (gt, GTYMIN, 0.)
			options[ZERO] = YES
		    } else {
			call gt_setr (gt, GTYMIN, INDEF)
			options[ZERO] = NO
		    }
		    newgraph = options[AUTO]
		    overplot = NO

	        case 'c':
		    call gt_setr (gt, GTXMIN, INDEF)
		    call gt_setr (gt, GTXMAX, INDEF)
		    call gt_setr (gt, GTYMIN, INDEF)
		    call gt_setr (gt, GTYMAX, INDEF)
		    if (options[ZERO] == YES)
			call gt_setr (gt, GTYMIN, 0.)
		    newgraph = YES
		    overplot = NO
		    
	        case 'd': # De-blend a group of lines
		    call ans_hdr (sh, newimage, key, Memc[save1], Memc[save2],
			fd1, fd2)
		    call sp_deblend (sh, gp, wx, wy, Memr[x], Memr[y], npts,
			fd1, fd2, xg, yg, sg, lg, pg, ng)
		    newimage = NO
    
	        case 'k': # Fit gaussian
		    call ans_hdr (sh, newimage, key, Memc[save1], Memc[save2],
			fd1, fd2)
		    obs_lambda = mygfit(sh, gp, wx, wy, Memr[x], Memr[y], npts,
			fd1, fd2, xg, yg, sg, lg, pg, ng)
		    call un_changer (UN(sh), funits, obs_lambda, 1, NO)
		    newimage = NO

	        case 'e': # Equivalent width
		    call ans_hdr (sh, newimage, key, Memc[save1], Memc[save2],
			fd1, fd2)
		    obs_lambda = myeqwidth (sh, gp, wx, wy, Memr[x], Memr[y], npts,
			fd1, fd2)
		    call un_changer (UN(sh), funits, obs_lambda, 1, NO)
				call pargstr (Memc[cmd])
		    newimage = NO

	        case 'v':
		    iferr {
			if (UN_CLASS(UN(sh)) == UN_VEL) {
			    call strcpy (Memc[units1], Memc[units], SZ_FNAME)
			    call strcpy (Memc[units2], Memc[units3], SZ_FNAME)
			} else {
			    call strcpy (Memc[units], Memc[units1], SZ_FNAME)
			    call strcpy (UNITS(sh), Memc[units2], SZ_FNAME)
			    call un_changer (UN(sh), "angstroms", wx, 1, NO)
			    call sprintf (Memc[units], SZ_FNAME,
				"km/s %g angstroms")
				call pargr (wx)
			    call strcpy (Memc[units], Memc[units3], SZ_FNAME)
			}
			wx = gt_getr (gt, GTXMIN)
			if (!IS_INDEF(wx)) {
			    call un_changer (UN(sh), Memc[units3], wx, 1, NO)
			    call gt_setr (gt, GTXMIN, wx)
			}
			wx = gt_getr (gt, GTXMAX)
			if (!IS_INDEF(wx)) {
			    call un_changer (UN(sh), Memc[units3], wx, 1, NO)
			    call gt_setr (gt, GTXMAX, wx)
			}
			call un_changer (UN(sh), Memc[units3], Memr[x], npts,
			    YES)
			call gt_sets (gt, GTXLABEL, UN_LABEL(UN(sh)))
			call gt_sets (gt, GTXUNITS, UN_UNITS(UN(sh)))
			newgraph = YES
			overplot = NO
		    } then
			call erract (EA_WARN)

	        case 'h': # Equivalent widths -- C. Pilachowski style
		    call ans_hdr (sh, newimage, key, Memc[save1], Memc[save2],
			fd1, fd2)
		    repeat {
			switch (key) {
			case 'a', 'b', 'c': # Continuum at cursor width at 1/2
		            obs_lambda = myeqwidth_cp (sh, gp, wx, wy, INDEF,
				Memr[y], npts, key, fd1, fd2, xg, yg, sg,
				lg, pg, ng)
			    call un_changer (UN(sh), funits, obs_lambda, 1, NO)
			    break
			case 'l', 'r', 'k': # Continuum at 1
		            obs_lambda = myeqwidth_cp (sh, gp, wx, 1., wy,
			        Memr[y], npts, key, fd1, fd2, xg, yg, sg,
				lg, pg, ng)
			    call un_changer (UN(sh), funits, obs_lambda, 1, NO)
			    break
			default:
		    	    call printf (
				"Set cursor and type a, b, c, l, r, or k:")
			}
		    } until (clgcur ("cursor", wx, wy, wc, key, Memc[cmd],
			SZ_FNAME) == EOF)
		    newimage = NO

		case 'o': # Set overplot
		    overplot = YES

	        case 'g', '#', '%', '(', ')': # Get new image to plot
		    i = nline
		    j = nband

		    switch (key) {
		    case '(':
			if (IM_LEN(im,2) > 1) {
			    nline = max (1, min (IM_LEN(im,2), nline-1))
			    nap = INDEFI
			} else if (IM_LEN(im,3) > 1) {
			    nband = max (1, min (IM_LEN(im,3), nband-1))
			}
		    case ')':
			if (IM_LEN(im,2) > 1) {
			    nline = max (1, min (IM_LEN(im,2), nline+1))
			    nap = INDEFI
			} else if (IM_LEN(im,3) > 1) {
			    nband = max (1, min (IM_LEN(im,3), nband+1))
			}
		    case '#':
			nline = 0
		    case '%':
			nband = 0
		    default:
			call clgstr ("next_image", Memc[cmd], SZ_FNAME)
			if (streq (Memc[image], Memc[cmd])) {
			    call shdr_close (sh)
			} else if (imaccess (Memc[cmd], READ_ONLY) == YES) {
			    call shdr_close (sh)
			    call smw_close (mw)
			    call imunmap (im)
			    newimage = YES
			} else {
			    call eprintf ("Can't get %s\n")
				call pargstr (Memc[cmd])
			    next
			}
			call strcpy (Memc[cmd], Memc[image], SZ_FNAME)
			nline = 0
			nband = 0
		    }

		    call getimage (Memc[image], nline, nband, nap, wave_scl,
			w0, wpc, Memc[units], im, mw, sh, gt)
		    x = SX(sh)
		    y = SY(sh)
		    npts = SN(sh)
		    ### Start my additions
		    nextra = 0
		    ### End my additions

		    if (options[WRESET] == YES && overplot == NO) {
			call gt_setr (gt, GTXMIN, clgetr ("xmin"))
			call gt_setr (gt, GTXMAX, clgetr ("xmax"))
			call gt_setr (gt, GTYMIN, clgetr ("ymin"))
			call gt_setr (gt, GTYMAX, clgetr ("ymax"))
			if (options[ZERO] == YES)
			    call gt_setr (gt, GTYMIN, 0.)
		    }
			
		    if (nline != i || nband != j)
			newimage = YES
		    newgraph = YES

	        case 'w': # Window the graph
		    call gt_window (gt, gp, "cursor", newgraph)
		    if (newgraph == YES) {
			newgraph = options[AUTO]
			overplot = NO
		    }
		
	        case 'l': # Convert to f-lambda - issue warning if not a
			  # calibrated image
		    if (FC(sh) == FCNO)
		        call eprintf (
			    "Warning: (>flam) spectrum not calibrated\n")

		    call conflam (sh)
		    call gt_setr (gt, GTYMIN, INDEF)
		    call gt_setr (gt, GTYMAX, INDEF)
		    if (options[ZERO] == YES)
			call gt_setr (gt, GTYMIN, 0.)
		    newgraph = options[AUTO]
		    overplot = NO

	        case 'f': # Function operators
		    call fun_help ()
		    while (clgcur ("cursor", wx, wy, wc, key, Memc[cmd], 
		        SZ_FNAME) != EOF) {
			switch (key) {
			case '?':
			    call fun_help ()
			case 'q':
			    break
			case 'I':
			    call fatal (0, "Interrupt")
			default:
			    iferr {
		                call fun_do (key, sh, Memr[y], npts, w0, wpc)
				call gt_setr (gt, GTYMIN, INDEF)
				call gt_setr (gt, GTYMAX, INDEF)
				if (options[ZERO] == YES)
				    call gt_setr (gt, GTYMIN, 0.)
		                if (options[AUTO] == YES)
			            call replot (gp, gt, Memr[x], Memr[y],
					npts, YES)
				overplot = NO
		                call fun_help ()
			    } then {
			        call erract (EA_WARN)
				call tsleep (2)
				call fun_help ()
			    }
			}
		    }
		    call printf ("\n")

	        case 'm': # Signal-to-noise
		    call ans_hdr (sh, newimage, key, Memc[save1], Memc[save2],
			fd1, fd2)
		    call avgsnr (sh, wx, wy, Memr[y], npts, fd1, fd2)
		    newimage = NO

	        case 'n': # Convert to f-nu
		    if (FC(sh) == FCNO)
		        call eprintf (
			    "Warning: (>fnu) spectrum not calibrated\n")

		    call confnu (sh)
		    call gt_setr (gt, GTYMIN, INDEF)
		    call gt_setr (gt, GTYMAX, INDEF)
		    if (options[ZERO] == YES)
			call gt_setr (gt, GTYMIN, 0.)
		    newgraph = options[AUTO]
		    overplot = NO

		case 'q':
		    if (options[WRESET] == YES) {
			call gt_setr (gt, GTXMIN, clgetr ("xmin"))
			call gt_setr (gt, GTXMAX, clgetr ("xmax"))
			call gt_setr (gt, GTYMIN, clgetr ("ymin"))
			call gt_setr (gt, GTYMAX, clgetr ("ymax"))
			if (options[ZERO] == YES)
			    call gt_setr (gt, GTYMIN, 0.)
		    }
			
		    if (nline != i || nband != j)
			newimage = YES
		    newgraph = YES
		    break

	        case 'r': # Replot
		    newgraph = YES
		    overplot = NO

	        case 's': # Smooth
		    call smooth (Memr[y], npts)
		    newgraph = options[AUTO]

	        case 't': # FlaTTen spectrum
		    call flatten (gp, gt, Memr[x], Memr[y], npts)
		    call gt_setr (gt, GTYMIN, INDEF)
		    call gt_setr (gt, GTYMAX, INDEF)
		    if (options[ZERO] == YES)
			call gt_setr (gt, GTYMIN, 0.)
		    newgraph = options[AUTO]
		    overplot = NO

	        case 'p', 'u': # Set user coordinates
		    if (!wave_scl) {
			call shdr_system (sh, "world")
			wave_scl = true
			call gt_sets (gt, GTXLABEL, UN_LABEL(UN(sh)))
			call gt_sets (gt, GTXUNITS, UN_UNITS(UN(sh)))
		    }
		    switch (key) {
		    case 'p':
			keyu = 'l'
			w1 = Memr[x]
			u1 = clgetd ("wstart")
			w2 = Memr[x+npts-1]
			u2 = clgetd ("wend")
			if (IS_INDEFD(u1)) {
			    u1 = clgetd ("dw")
			    u1 = u2 - (npts - 1) * u1
			} else if (IS_INDEFD(u2)) {
			    u2 = clgetd ("dw")
			    u2 = u1 + (npts - 1) * u2
			}
		    case 'u':
			call printf (
	"Set cursor and select correction: d(oppler), z(eropoint), l(inear)\n")
			call flush (STDOUT)
			i = clgcur ("cursor", wx, wy, wc, keyu, Memc[cmd],
			    SZ_FNAME)
			w1 = wx
			u1 = clgetd ("wavelength")
			if (keyu == 'l') {
			    repeat {
				call printf ("Set cursor to second position:")
				call flush (STDOUT)
				i = clgcur ("cursor", wx, wy, wc, key,
				    Memc[cmd], SZ_FNAME)
				w2 = wx
				if (!fp_equald (w1, w2)) {
				    u2 = clgetd ("wavelength")
				    break
				}
				call printf ("Cursor not moved: ")
			    }
			}
		    }
		    call usercoord (sh, keyu, w1, u1, w2, u2)
		    w0 = Memr[x]
		    wpc = Memr[x+1] - w0
		    call gt_setr (gt, GTXMIN, INDEF)
		    call gt_setr (gt, GTXMAX, INDEF)
		    call gt_sets (gt, GTXLABEL, UN_LABEL(UN(sh)))
		    call gt_sets (gt, GTXUNITS, UN_UNITS(UN(sh)))
		    newgraph = options[AUTO]
		    overplot = NO

	        case 'i': # Write image spectrum out
		    call sp_wrspect (sh)
		    im = IM(sh)
		    mw = MW(sh)

	        case 'j': # Fudge (fix) a data point
		    call fudgept (sh, gp, Memr[x], Memr[y], npts, wx, wy)

	        case 'x': # Fudge eXtended over a line
		    call fudgex (sh, gp, Memr[x], Memr[y], npts, wx, wy,
			options[XYDRAW])

	        case 'y': # Over plot standard star data
		    # Estimate data is fnu or flambda: cutoff around dexp[-20]
		    fnu = false
		    call aavgr (Memr[y], npts, avg_pix, sigma_pix)
		    if (log10 (avg_pix) < -19.5)
		        fnu = true
		    call plot_std (sh, gp, fnu)
		    call printf ("\n")

	        case 'z': # Zoom x region to larger range
		    call auto_exp (gp, gt, key, wx, Memr[x], Memr[y], npts)

	        case '-': # Subtract deblended fit
		    call subblend (sh, gp, Memr[x], Memr[y], npts, wx, wy,
			xg, yg, sg, lg, pg, ng)

	        case '.': # Slide upward
		    call auto_exp (gp, gt, key, wx, Memr[x], Memr[y], npts)

	        case ',': # Slide downward
		    call auto_exp (gp, gt, key, wx, Memr[x], Memr[y], npts)

	        case '$': # Toggle wavelength scale
		    if (wave_scl) {
			call shdr_system (sh, "physical")
			wave_scl = false
			call gt_sets (gt, GTXLABEL, "Pixel")
			call gt_sets (gt, GTXUNITS, "")
		    } else {
			call shdr_system (sh, "world")
			wave_scl = true
			call gt_sets (gt, GTXLABEL, UN_LABEL(UN(sh)))
			call gt_sets (gt, GTXUNITS, UN_UNITS(UN(sh)))
		    }
		    call gt_setr (gt, GTXMIN, INDEF)
		    call gt_setr (gt, GTXMAX, INDEF)
		    call gt_setr (gt, GTYMIN, INDEF)
		    call gt_setr (gt, GTYMAX, INDEF)
		    if (options[ZERO] == YES)
			call gt_setr (gt, GTYMIN, 0.)
		    newgraph = options[AUTO]
		    overplot = NO

	        case '/': # Help on status line
		    call sts_help (hline, hlines, HELP, hptr)
		    hline = mod (hline, hlines) + 1

	        case '?': # Help screen
		    call gpagefile (gp, KEY, PROMPT)

		case 'I': # Interrupt
		    call fatal (0, "Interrupt")

	        default: # Default - print cursor info
		    i = max (1, min (npts, nint (shdr_wl (sh, double(wx)))))
		    call printf ("x,y,z(x): %10.3f %10.4g %10.4g\n")
		        call pargr (wx)
		        call pargr (wy)
		        call pargr (Memr[y+i-1])
	        }


	        if (newgraph == YES) {
		    if (overplot == YES) {
		        call printf ("Overplotting: %s")
			    call pargstr (Memc[image])
			if (nline > 0) {
			    if (nband > 0) {
				call printf ("(%d,%d)")
				    call pargi (nline)
				    call pargi (nband)
			    } else {
				call printf ("(%d)")
				    call pargi (nline)
			    }
			}
		        call flush (STDOUT)
			i = gt_geti (gt, GTLINE)
			j = gt_geti (gt, GTCOLOR)
			if (options[OVERPLOT] == NO) {
			    call gt_seti (gt, GTLINE, i+1)
			    call gt_seti (gt, GTCOLOR, j+1)
			}
			call replot (gp, gt, Memr[x], Memr[y], npts, NO)
			call gt_seti (gt, GTLINE, i)
			call gt_seti (gt, GTCOLOR, j)
		    } else
			call replot (gp, gt, Memr[x], Memr[y], npts, YES)
		    ### Start my additions
		    if (show_shifted) {
			do i = 1, nshifted
			    if (visible[i]) {
				wx = fdefault[i] * (1. + z)
				call un_changer (pfunits, UNITS(sh), wx,1,NO)
			    	call mark (sh, gp, fname[1,i], fdefault[i],
				  wx, ftype[i], ltype[i], direction, Memr[y])
			    }
		    }
		    if (show_unshifted) {
			do i = nshifted + 1, nfeatures+nextra
			    if (visible[i]) {
				wx = fdefault[i]
				call un_changer (pfunits, UNITS(sh), wx,1,NO)
			    	call mark (sh, gp, fname[1,i], fdefault[i],
				  wx, ftype[i], ltype[i], direction, Memr[y])
			    }
		    }
		    ### End my additions
		    newgraph = NO
		    overplot = options[OVERPLOT]
	        }
	    } until (clgcur ("cursor",wx,wy,wc,key,Memc[cmd],SZ_FNAME) == EOF)
	    if (im != ERR) {
		call shdr_close (sh)
		call smw_close (mw)
	        call imunmap (im)
	    }
	}

	call gclose (gp)
	if (fd1 != NULL)
	    call close (fd1)
	if (fd2 != NULL) {
	    call close (fd2)
	    call delete (Memc[save2])
	}
	if (hptr != NULL)
	    call mfree (hptr, TY_CHAR)
	if (ng > 0) {
	    call mfree (xg, TY_REAL)
	    call mfree (yg, TY_REAL)
	    call mfree (sg, TY_REAL)
	    call mfree (lg, TY_REAL)
	    call mfree (pg, TY_INT)
	}
	call smw_daxis (NULL, NULL, 0, 0, 0)
	call gt_free (gt)
	call imtclose (list)
	### My addition
	call un_close (pfunits)
	### End my addition
end
