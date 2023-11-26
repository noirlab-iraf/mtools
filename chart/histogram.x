include <pkg/gtools.h>
include <mach.h>
include	<gset.h>
include <error.h>
include "chart.h"

define	MAXHIST	4	# maximum number of histograms that can be overplotted
define	SZ_SUBTITLE	(4*SZ_LINE)  # Size of subtitle string (long enough
				     #for four histogram sample definitions

define	KEYHELP	"chart$histogram.key"
define	PROMPT	"histogram options"

# HISTOGRAM -- Compute and plot the histogram of a list file

# Plagiarized from IMHISTOGRAM

procedure histogram(db,ch,gp,gt,index,marker,usermarks,color,npts,
		    nselected, newsample, newkey)
pointer	db		# DATABASE pointer
pointer	ch		# CHART pointer
pointer	gp		# GIO pointer
pointer	gt[CH_NGKEYS+1] # GTOOLS pointer
int	index[ARB]	# Selected elements index
int	marker[ARB]	# Marker type array
int	usermarks[ARB]	# Saved marker type array
int	color[ARB]	# Marker color array
int	npts		# Number of entries in the database
int	nselected	# Number of selected entries
int	newsample, newkey

double	z1, z2, tmpd, dz
int	z1l, z2l, tmpl
int	nbins, i, nscan(), dtype, nlevels, nwide, test_expr(), nindef, j, junk
pointer sp, hgm, hgmr, function, newfunction, dbuf, ibuf, ebuf

bool	fp_equald(), strne(), streq(), integrated
char	command[SZ_LINE]
real	wx, wy
int	wc, key, newgraph, gt_gcur1(), newparams, gkey
int	newhisto, datatype, tdatatype, select(), overplot, nhist
pointer	buffer
pointer	subtitle, comments, title

begin
	call smark (sp)
	call salloc (function, CH_SZFUNCTION, TY_CHAR)
	call salloc (newfunction, CH_SZFUNCTION, TY_CHAR)
	call salloc (buffer, SZ_LINE, TY_CHAR)
	call salloc (subtitle, SZ_SUBTITLE, TY_CHAR)
	call salloc (title, SZ_LINE, TY_CHAR)
	call salloc (comments, SZ_LINE, TY_CHAR)
	call salloc (dbuf, npts, TY_DOUBLE)
	call salloc (ibuf, npts, TY_INT)
	call malloc (hgm,  1, TY_INT)
	call malloc (hgmr, 1, TY_REAL)

	gkey = CH_NGKEYS+1
	nhist = 1
	overplot = NO
	newparams = NO
	newhisto = NO
	newgraph = NO
	integrated = false
	call strcpy ("", Memc[function], CH_SZFUNCTION)
	key = 'f'
	repeat {
	    if (nselected == 0)
		if (key != ':' && key != 'd' && key != 'q') {
		    call eprintf ("Warning: No objects in current sample -- redefine the sample\n")
		    goto 10
		}
	    switch (key) {
	    case 'a': # Toggle autoscale switch
		CH_AUTOSCALE(ch) = ! CH_AUTOSCALE(ch)
		switch (datatype) {
		case TY_INT:
		    if (CH_AUTOSCALE(ch))
			dtype = TY_INT
		    else
			dtype = TY_DOUBLE
		    newparams = YES
		case TY_DOUBLE:
		}

	    case ':': # List or set parameters
		if (command[1] == '/') {
	            call gt_colon (command, gp, gt[gkey], newgraph)
		    call gt_seti (gt[gkey], GTTRANSPOSE, NO) # No transposing
		    newgraph = YES
		} else {
		    call sscan (command)
		    call gargwrd (Memc[buffer], SZ_LINE)
		    if (streq (Memc[buffer], "list")) {
			call gargwrd (Memc[buffer], SZ_LINE)
			if (nscan() == 1)
			    call strcpy ("STDOUT", Memc[buffer], SZ_LINE)
		    	call hlist (gp,gt[gkey],z1,z2,nbins,nhist,Memi[hgm],
				  Memc[function],Memc[buffer],dtype,integrated)
		    } else {
		    	call colon (ch, command, junk, newkey, newsample,
				    newparams, gp, gt,
				    db, index, marker, nselected)
		    }
		}

	    case 'd': # New sample
		nselected = select (gt, ch, db, npts, index, marker, color)
		if (nselected == 0) {
		    # No objects in sample -- plot empty graph and warning
		    call gclear (gp)
		    call gt_sets (gt[gkey],  GTSUBTITLE, "")
		    call gt_sets (gt[gkey],  GTCOMMENTS, "")
		    call gswind (gp, -0.001, 0.001, -0.001, 0.001)
		    call gt_labax (gp, gt[gkey])
		    call gtext (gp, 0., 0., "NO OBJECTS IN DEFINED SAMPLE",
				"hjustify=center,vjustify=center")
	    	    call eprintf ("Warning: No objects meet all selection criteria\n")
		    goto 10
		} else {
		    # A good new sample
		    call amovi (marker, usermarks, nselected)
		    newsample = YES
		    newhisto = YES
		    # Read selected points
		    if (dtype == TY_INT) {
		    	call eval_expr (Memc[function],db,index,ibuf,ebuf,
			    TY_INT, false)
			call achtid (Memi[ibuf], Memd[dbuf], nselected)
		    } else
		    	call eval_expr (Memc[function],db,index,dbuf,ebuf,
			    TY_DOUBLE, false)
		}

	    case 'f': # Bin new function
		call printf ("Function to bin (%s): ")
	    	    call pargstr (Memc[function])
		call flush (STDOUT)
		call scan()
		if (nscan() != EOF) {
	    	    call gargstr (Memc[newfunction], CH_SZFUNCTION)
	    	    if (strne (Memc[newfunction], "")) {
			tdatatype = test_expr (Memc[newfunction], db, true)
			switch (tdatatype) {
			case TY_INT:
		    	    call strcpy (Memc[newfunction], Memc[function],
				     	 CH_SZFUNCTION)
		    	    call eval_expr (Memc[function],db,index,ibuf, ebuf,
					    TY_INT, false)
			    call achtid (Memi[ibuf], Memd[dbuf], nselected)
			    datatype = tdatatype
			    if (CH_AUTOSCALE(ch))
				dtype = TY_INT
			    else
				dtype = TY_DOUBLE
			    newhisto = YES
			    newparams = YES
			case TY_DOUBLE:
		    	    call strcpy (Memc[newfunction], Memc[function],
				     	 CH_SZFUNCTION)
		    	    call eval_expr (Memc[function],db,index,dbuf,ebuf,
					    TY_DOUBLE, false)
			    datatype = tdatatype
			    dtype = TY_DOUBLE
			    newhisto = YES
			    newparams = YES
			case TY_CHAR, TY_BOOL:
			    call eprintf ("Warning: Expression is not numeric (%s)\n")
				call pargstr (Memc[newfunction])
			case ERR:
			}
		    }
		}
		# Return if function to bin undefined
		if (streq (Memc[function], "")) {
		    call sfree (sp)
		    call mfree (hgm,  TY_INT)
		    call mfree (hgmr, TY_REAL)
		    return
		}

	    case 'i': # Toggle the integrate switch
		integrated = ! integrated
		newhisto = YES

	    case 'l': # Toggle log switch
		CH_LOG(ch) = ! CH_LOG(ch)
		newgraph = YES

	    case 'n': # Show number of points
		call printf ("%d")
		call pargi (nselected)
		call flush (STDOUT)

	    case 'o': # Overplot the next histogram
		if (nhist == MAXHIST) {
		    call eprintf ("Warning: Exceeds maximum number of overplotted histograms\n")
		    goto 10
		} else
		    overplot = YES

	    case 'p': # Toggle plot type
		CH_PLOTTYPE(ch) = mod (CH_PLOTTYPE(ch), 2) + 1
		newgraph = YES

	    case 'q':
		break

	    case 'r':
		newgraph = YES

	    case 't': # Toggle top_closed switch
		CH_TOPCLOSED(ch) = ! CH_TOPCLOSED(ch)
		newparams = YES

	    case 'w':  # Window graph
		call gt_window (gt[gkey], gp, "cursor", newgraph)

	    case 'z': # Zero the graph bottom
		call gt_setr (gt[gkey], GTYMIN, 0.)
		newgraph = YES

	    case '?': # Help screen
		call gpagefile (gp, KEYHELP, PROMPT)

	    default: # Default = 'c'
		call printf ("x,y: %10.3f %10.4g\n")
		    call pargr (wx)
		    call pargr (wy)
	    }

	    # Overplotting turned off if histogram parameters are changed
	    if (newparams == YES) {
		overplot = NO
		nhist = 1
	    }

	    # Determine new histogram parameters if necessary
	    if (newparams == YES || (newhisto == YES && overplot == NO)) {
		newgraph = YES
		# Read binning parameters
		z1 = CH_Z1(ch)
		z2 = CH_Z2(ch)
		nbins = CH_NBINS(ch)
		if (dtype == TY_INT) {
		    # Test for at least one defined value
		    j = 1
		    while (IS_INDEFI(Memi[ibuf+j-1])) {
			j = j+1
			if (j > nselected) {
		    	    call eprintf (
			      "Warning: Sample (%s) all INDEF values.\n")
			    call pargstr (Memc[function])
		    	    call sfree (sp)
		    	    call mfree (hgm,  TY_INT)
		    	    call mfree (hgmr, TY_REAL)
	    	    	    return
		    	}
		    }
		    # Get histogram range.
		    if (IS_INDEFD(z1)) {
		        z1l = Memi[ibuf+j-1]
		    	do i = j+1, nselected {
		            if (! IS_INDEFI(Memi[ibuf+i-1]) &&
			        z1l > Memi[ibuf+i-1])
			    	z1l = Memi[ibuf + i - 1]
		    	}
		    } else
			z1l = nint (z1)
		    if (IS_INDEFD(z2)) {
		    	z2l = Memi[ibuf+j-1]
		    	do i = j+1, nselected {
		            if (! IS_INDEFI(Memi[ibuf+i-1]) &&
			        z2l < Memi[ibuf + i - 1])
			    	z2l = Memi[ibuf + i - 1]
		    	}
		    } else
			z2l = nint (z2)
		    if (z1l > z2l) {
			tmpl = z1l
			z1l = z2l
			z2l = tmpl
		    }
		    nlevels = z2l - z1l
		    nwide = max (1, nint (real (nlevels) / real (nbins)))
		    nbins = max (1, nint (real (nlevels) / real (nwide)))
		    z2l = z1l + nbins * nwide
		    z1 = z1l
		    z2 = z2l
		} else {
		    # Test for at least one defined value
		    j = 1
		    while (IS_INDEFD(Memd[dbuf+j-1])) {
			j = j+1
			if (j > nselected) {
		    	    call eprintf (
			      "Warning: Sample (%s) all INDEF values.\n")
			    call pargstr (Memc[function])
		    	    call sfree (sp)
		    	    call mfree (hgm,  TY_INT)
		    	    call mfree (hgmr, TY_REAL)
		    	    return
		    	}
		    }
		    # Get histogram range.
		    if (IS_INDEFD(z1)) {
		    	z1 = Memd[dbuf+j-1]
		    	do i = j+1, nselected {
		            if (! IS_INDEFD(Memd[dbuf+i-1]) &&
				z1 > Memd[dbuf+i-1])
			    	z1 = Memd[dbuf+i-1]
		    	}
		    }
		    if (IS_INDEFD(z2)) {
		    	z2 = Memd[dbuf+j-1]
		    	do i = j+1, nselected {
		            if (! IS_INDEFD(Memd[dbuf+i-1]) &&
		                z2 < Memd[dbuf + i - 1])
			    	z2 = Memd[dbuf + i - 1]
		    	}
		    }
		    if (z1 > z2) {
			tmpd = z1
			z1 = z2
			z2 = tmpd
		    }
		}
		dz = (z2 - z1) / double(nbins)

		# Allocate histogram space
		call realloc (hgmr, nbins*MAXHIST, TY_REAL)
		call realloc (hgm,  nbins*MAXHIST, TY_INT)

		# Test for constant valued function, which causes zero divide
		# in ahgm.
		if (fp_equald (z1, z2)) {
		    call eprintf (
		"Warning: Constant valued function (%s) has no data range.\n")
			call pargstr (Memc[function])
		    call sfree (sp)
		    call mfree (hgm,  TY_INT)
		    call mfree (hgmr, TY_REAL)
		    return
		}

		# Set graph parameters
		call gt_setr (gt[gkey], GTXMIN, real(z1))
		call gt_setr (gt[gkey], GTXMAX, real(z2))
		call gt_setr (gt[gkey], GTYMIN, 0.)
		call gt_setr (gt[gkey], GTYMAX, INDEF)

		# Stuff the histogram parameters into the header
		if (dtype == TY_INT) {
		    call sprintf (Memc[comments], SZ_LINE,
			"z1 = %-6d    z2 = %-6d    nbins = %-4d    %s")
			if (IS_INDEFD(CH_Z1(ch)))
			    call pargi (INDEFI)
			else
		    	    call pargi (z1l)
			if (IS_INDEFD(CH_Z2(ch)))
			    call pargi (INDEFI)
			else
		    	    call pargi (z2l)
		} else {
		    call sprintf (Memc[comments], SZ_LINE,
			"z1 = %-6g    z2 = %-6g    nbins = %-4d    %s")
		    	call pargd (CH_Z1(ch))
		    	call pargd (CH_Z2(ch))
		}
		call pargi (nbins)
		if (CH_TOPCLOSED(ch))
		    call pargstr ("top closed")
		else
		    call pargstr ("top open")
	    }

	    # Calculate new histogram
	    if (newparams == YES || newhisto == YES) {
		newgraph = YES
		# Check for overplotting
		if (overplot == YES) {
		    overplot = NO
		    nhist = nhist + 1
		} else
		    nhist = 1
		# Create the histogram
		if (dtype == TY_INT)
		    call histoi (Memi[ibuf], nselected,
				 Memi[hgm+(nhist-1)*nbins],
				 nbins, z1l, z2l, CH_TOPCLOSED(ch), nindef)
		else
		    call histod (Memd[dbuf], nselected,
				 Memi[hgm+(nhist-1)*nbins],
				 nbins, z1, z2, CH_TOPCLOSED(ch), nindef)
		# Integrate the histogram if desired
		if (integrated)
		    do i = 2, nbins
			Memi[hgm+((nhist-1)*nbins)+i-1] = Memi[hgm+((nhist-1)*nbins)+i-1] + Memi[hgm+((nhist-1)*nbins)+i-2]
		# Set the plotting histogram and plot parameters
		call achtir (Memi[hgm+(nhist-1)*nbins],
			     Memr[hgmr+(nhist-1)*nbins], nbins)
	    }

	    # Draw the histogram
	    if (newgraph == YES) {
		# No overplot -- make complete label if needed
		if (nhist == 1)
		    if (newparams == YES || newhisto == YES) {
			# Get title string (selection criteria) from one of
			# the other gtools, since this one may be blank after
			# an overplot -- This also save it for later use
			# in an nhist == 2 overplot
			call gt_gets (gt[1], GTTITLE, Memc[title], SZ_LINE)
			call gt_sets (gt[gkey], GTTITLE, Memc[title])
		    	call gt_sets (gt[gkey], GTSUBTITLE, "")
		        call gt_sets (gt[gkey], GTCOMMENTS, Memc[comments])
		    	call gt_sets (gt[gkey], GTXLABEL, Memc[function])
		    	if (integrated)
		    	    call gt_sets (gt[gkey],GTYLABEL,"integrated count")
		    	else
		    	    call gt_sets (gt[gkey], GTYLABEL, "count")
		    }
		# First overplot -- prepend "solid:" to first sample definition
		if (nhist == 2 && newhisto == YES) {
		    call sprintf (Memc[subtitle], SZ_SUBTITLE, "solid: %s")
			call pargstr (Memc[title])
		    call gt_sets (gt[gkey], GTSUBTITLE, Memc[subtitle])
		}
		# Add new sample definition to label
		if (nhist > 1 && newhisto == YES) {
		    call gt_gets (gt[gkey], GTTITLE, Memc[title], SZ_LINE)
		    call gt_gets (gt[gkey], GTSUBTITLE, Memc[buffer], SZ_LINE)
		    switch (nhist) {
		    case 2:
			call sprintf(Memc[subtitle],SZ_SUBTITLE,"%s\ndash: %s")
		    case 3:
			call sprintf(Memc[subtitle], SZ_SUBTITLE,"%s\ndot: %s")
		    case 4:
			call sprintf(Memc[subtitle],SZ_SUBTITLE,"%s\ndash-dot: %s")
		    }
		    call pargstr (Memc[buffer])
		    if (key == 'i') {
			if (integrated)
			    call pargstr ("integrated")
			else
			    call pargstr ("differential")
		    } else
		    	call pargstr (Memc[title])
		    call gt_sets (gt[gkey], GTSUBTITLE, Memc[subtitle])
		    call gt_sets (gt[gkey], GTTITLE, "")
		}
		# Re-scale and draw graph
		call gclear (gp)
		if (CH_LOG(ch))
		    call gt_sets (gt[gkey], GTYTRAN,"logarithmic")
		else
		    call gt_sets (gt[gkey], GTYTRAN,"linear")
		call gswind (gp, real(z1), real(z2), INDEF, INDEF)
		call gascale (gp, Memr[hgmr], nbins*nhist, 2)
		call gt_swind (gp, gt[gkey])
		call gt_labax (gp, gt[gkey])
		do j = 1, nhist {
		    call gseti (gp, G_PLTYPE, j)
		    if (CH_PLOTTYPE(ch) == HGM_BOX) {
		    	call gamove (gp, real(z1), 0.)
		    	do i = 1, nbins {
		    	    call gadraw (gp, real(z1+(i-1)*dz),
					 Memr[hgmr+(nbins*(j-1))+i-1])
		    	    call gadraw (gp, real(z1+i*dz),
					 Memr[hgmr+(nbins*(j-1))+i-1])
		    	}
		    	call gadraw (gp, real(z2), 0.)
		    } else
		    	call gvline (gp, Memr[hgmr+(nbins*(j-1))], nbins,
				     real(z1 + dz/2.), real(z2 - dz/2.))
		}
		call gseti (gp, G_PLTYPE, 1)
	    }
	    # Clear flags
10	    newhisto = NO
	    newparams = NO
	    newgraph = NO
	}  until (gt_gcur1 (gt[gkey],"cursor",wx,wy,wc,key,command,SZ_LINE) == EOF)

	# Shutdown.
	call sfree (sp)
	call mfree (hgm,  TY_INT)
	call mfree (hgmr, TY_REAL)
end

# HLIST -- List the histogram.

procedure hlist (gp,gt,z1,z2,nbins,nhist,histogram,function,filename,dtype,
		 integrated)

pointer	gp				# GIO pointer
pointer	gt				# GTOOLS pointer
double	z1				# Lower bound
double	z2				# Upper bound
int	nbins				# Number of bins
int	nhist				# Number of histograms
int	histogram[nbins]		# The histogram
char	function[ARB]
char	filename[ARB]
int	dtype				# Datatype of function (int or double)
bool	integrated			# Integrated counts?

int	i
int	z1l, z2l, dzl

bool	streq()
double	dz
int	fd, open()

begin
    # Open the output file.
    iferr {
        fd = open (filename, NEW_FILE, TEXT_FILE)
    } then {
        call erract (EA_WARN)
        return
    }
    if (streq ("STDOUT", filename))
        call gdeactivate (gp, AW_CLEAR)
    call hheader (gt, fd)
    call fprintf (fd, "#\n#  Histogram of %s\n")
        call pargstr (function)
    if (integrated)
    	call fprintf (fd, "#\n#    lower       upper   integrated count\n")
    else
    	call fprintf (fd, "#\n#    lower       upper   count\n")
    if (dtype == TY_INT) {
        z1l = nint (z1)
        z2l = nint (z2)
        dzl = (z2l - z1l) / nbins
        do i = 1, nbins*nhist {
	    if (mod (i, nbins) == 1 && i != 1)
		call fprintf (fd, "\n")
            call fprintf (fd, "%10d  %10d  %6d\n")
        	call pargi (z1l+(i-1)*dzl)
        	call pargi (z1l+i*dzl)
        	call pargi (histogram[i])
        }
    } else {
        dz = (z2 - z1) / nbins
        do i = 1, nbins*nhist {
	    if (mod (i, nbins) == 1 && i != 1)
		call fprintf (fd, "\n")
            call fprintf (fd, "%10g  %10g  %6d\n")
        	call pargd (z1+(i-1)*dz)
        	call pargd (z1+i*dz)
        	call pargi (histogram[i])
        }
    }
    if (streq ("STDOUT", filename))
        call greactivate (gp, AW_PAUSE)
    call close (fd)
end

# HHEADER -- Print a header to a file, which is a number of comment lines
# specifying the database and sample definitions.

define	SZ_TITLE	(5*SZ_LINE)

procedure hheader (gt, fd)
pointer	gt	# GTOOLS pointer
int	fd	# Output file descriptor

pointer	sp, comment
int	len, strlen(), i
begin
    call smark (sp)
    call salloc (comment, SZ_TITLE, TY_CHAR)

    call sysid (Memc[comment], SZ_TITLE)
    call fprintf (fd, "#  %s\n")
        call pargstr (Memc[comment])
    call gt_gets (gt, GTPARAMS, Memc[comment], SZ_TITLE)
    call fprintf (fd, "#  database: %s\n")
        call pargstr (Memc[comment])
    call gt_gets (gt, GTTITLE, Memc[comment], SZ_TITLE)
    len = strlen (Memc[comment])
    call fprintf (fd, "#  ")
    do i = 1, len {
	call putc (fd, Memc[comment+i-1])
	if (Memc[comment+i-1] == '\n')
    	    call fprintf (fd, "#  ")
    }
    call fprintf (fd, "\n")
    call gt_gets (gt, GTSUBTITLE, Memc[comment], SZ_TITLE)
    len = strlen (Memc[comment])
    call fprintf (fd, "#  ")
    do i = 1, len {
	call putc (fd, Memc[comment+i-1])
	if (Memc[comment+i-1] == '\n')
    	    call fprintf (fd, "#  ")
    }
    call fprintf (fd, "\n")
    call gt_gets (gt, GTCOMMENTS, Memc[comment], SZ_TITLE)
    call fprintf (fd, "#  %s\n")
        call pargstr (Memc[comment])
    call sfree (sp)
end

# HISTO -- Accumulate the histogram of the input vector.

# Originally sys$vops/ahgm.gx

procedure histod (data, npix, hgm, nbins, z1, z2, topclosed, nindef)

double 	data[ARB]		# data vector
int	npix			# number of pixels
int	hgm[ARB]		# output histogram
int	nbins			# number of bins in histogram
double	z1, z2			# greyscale values of first and last bins
bool	topclosed		# close the top bin?
int	nindef			# Number of indefinite objects excluded from
				# the histogram

double	dz
int	bin, i, nbins1

begin
	if (nbins < 1)
	    return
	call aclri (hgm, nbins)
	dz = double (nbins) / double (z2 - z1)
	nbins1 = nbins + 1
	nindef = 0

	do i = 1, npix {
	    if (IS_INDEFD(data[i])) {
		nindef = nindef + 1
		next
	    }
	    bin = int ((data[i] - z1) * dz) + 1
	    if (topclosed && bin == nbins1)
		if (data[i] <= z2 + EPSILOND)
		    bin = nbins
	    if (bin <= 0 || bin > nbins)
		next
	    hgm[bin] = hgm[bin] + 1
	}
end

procedure histoi (data, npix, hgm, nbins, z1, z2, topclosed, nindef)

int 	data[ARB]		# data vector
int	npix			# number of pixels
int	hgm[ARB]		# output histogram
int	nbins			# number of bins in histogram
int	z1, z2			# greyscale values of first and last bins
bool	topclosed		# close the top bin?
int	nindef			# Number of indefinite objects excluded from
				# the histogram

int	dz
int	bin, i, nbins1

begin
	if (nbins < 1)
	    return
	call aclri (hgm, nbins)
	dz = (z2 - z1) / nbins
	nbins1 = nbins + 1
	nindef = 0

	do i = 1, npix {
	    if (IS_INDEFI(data[i])) {
		nindef = nindef + 1
		next
	    }
	    bin = ((data[i] - z1) / dz) + 1
	    if (topclosed && bin == nbins1)
		if (data[i] == z2)
		    bin = nbins
	    if (bin <= 0 || bin > nbins)
		next
	    hgm[bin] = hgm[bin] + 1
	}
end
