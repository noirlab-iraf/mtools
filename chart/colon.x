include	<ctype.h>
include	<gset.h>
include "chart.h"
include "cutoff.h"
include "markers.h"

# List of colon commands.
define	CMDS "|xx|yy|marked|unmarked|rkeys|wkeys|cutoff1|cutoff2|cutoff3|cutoff4|logic|marker1|marker2|marker3|marker4|def_marker|xsize|ysize|field|xcenter|ycenter|radius|max_size|min_size|outformat|show|all|replace|rmarks|gmarked|gunmarked|gall|nbins|z1|z2|log|c1|c2|c3|c4|m1|m2|m3|m4|outliers|sorter|mmarker|marker5|marker6|marker7|marker8|m5|m6|m7|m8|color1|color2|color3|color4|color5|color6|color7|color8|l1|l2|l3|l4|l5|l6|l7|l8|"

define	XX		1	# No longer used
define	YY		2	# No longer used
define	MARKED		3	# Write marked points in PDS format
define	UNMARKED	4	# Write unmarked points in PDS format
define	RKEYS		5	# Read graph keys file
define	WKEYS		6	# Write graph keys file
define	CUTOFF1		7
define	CUTOFF2		8
define	CUTOFF3		9
define	CUTOFF4		10
define	LOGIC		11
define	MARKER1		12
define	MARKER2		13
define	MARKER3		14
define	MARKER4		15
define	DEFMARKER	16
define	XSIZE		17
define	YSIZE		18
define	FIELD		19
define	XCENTER		20
define	YCENTER		21
define	RADIUS		22
define	MAXSIZE		23
define	MINSIZE		24
define	OUTFORMAT	25
define	SHOW		26
define	ALL		27
define	REPLACE		28
define	RMARKS		29
define	GMARKED		30
define	GUNMARKED	31
define	GALL		32
define	NBINS		33
define	Z1		34
define	Z2		35
define	LOG		36  	# Not used anymore
define	C1		37
define	C2		38
define	C3		39
define	C4		40
define	M1		41
define	M2		42
define	M3		43
define	M4		44
define	OUTLIERS	45
define	SORTER	    	46
define	MMARKER	    	47
define	MARKER5		48
define	MARKER6		49
define	MARKER7		50
define	MARKER8		51
define	M5		52
define	M6		53
define	M7		54
define	M8		55
define  COLOR1		56
define  COLOR2		57
define  COLOR3		58
define  COLOR4		59
define  COLOR5		60
define  COLOR6		61
define  COLOR7		62
define  COLOR8		63
define	L1		64
define	L2		65
define	L3		66
define	L4		67
define	L5		68
define	L6		69
define	L7		70
define	L8		71

# COLON -- Processes colon commands.  The common flags and newgraph
# signal changes in fitting parameters or the need to redraw the graph.

# Formerly xtool$icfit/icgcolonr.x

procedure colon (ch, cmdstr, newgraph, newkey, newsample, newhisto, gp, gt,
		 db, index, marker, nselected)

pointer ch				# CHART pointer
char	cmdstr[ARB]			# Command string
int	newgraph			# New graph?
int	newkey				# New graph key description?
int	newsample			# New sample?
int	newhisto			# New histogram?
pointer	gp				# GIO pointer
pointer	gt[CH_NGKEYS+1]			# GTOOLS pointer
pointer	db				# DATABASE pointer
int	index[ARB]			# Selected elements index
int	marker[ARB]			# Marker type array
int	nselected			# Number of selected points

char	cmd[SZ_LINE]
int	ncmd, i, j
real	rval1

int	nscan(), strdic(), test_marker(), get_keys(), test_color()
pointer	sp, filename
int	get_strdic(), replace()
bool	streq(), bval, bool_expr(), size_expr(), num_expr()
int	ival
double	dval

begin
	# Use formated scan to parse the command string.
	# The first word is the command and it may be minimum match
	# abbreviated with the list of commands.

	call sscan (cmdstr)
	call gargwrd (cmd, SZ_LINE)
	ncmd = strdic (cmd, cmd, SZ_LINE, CMDS)

	switch (ncmd) {
	case ALL: # :all filename [format_file]
	    call gargstr (cmd, SZ_LINE)
	    call spitlist (ch, gp, gt[1],cmd,db,index, marker, nselected,
			   "all")

	case MARKED: # :marked filename [format_file]
	    call gargstr (cmd, SZ_LINE)
	    call spitlist (ch, gp, gt[1],cmd,db,index, marker, nselected,
			   "marked")

	case UNMARKED: # :unmarked filename [format_file]
	    call gargstr (cmd, SZ_LINE)
	    call spitlist (ch, gp, gt[1],cmd,db,index, marker, nselected,
			   "unmarked")

	case GALL: # :gall filename [format_file]
	    call gargstr (cmd, SZ_LINE)
	    call gspitlist (ch, gp, gt[1],cmd,db,index, marker, nselected,
			   "all")

	case GMARKED: # :gmarked filename [format_file]
	    call gargstr (cmd, SZ_LINE)
	    call gspitlist (ch, gp, gt[1],cmd,db,index, marker, nselected,
			   "marked")

	case GUNMARKED: # :gunmarked filename [format_file]
	    call gargstr (cmd, SZ_LINE)
	    call gspitlist (ch, gp, gt[1],cmd,db,index, marker, nselected,
			   "unmarked")

	case RKEYS:
	    call smark (sp)
	    call salloc (filename, SZ_FNAME, TY_CHAR)
	    call gargwrd (Memc[filename], SZ_FNAME)
	    if (nscan() == 1)
		call strcpy ("STDIN", Memc[filename], SZ_FNAME)
	    if (get_keys (ch, db, gt, Memc[filename]) > 0) {
	        call strcpy (Memc[filename], Memc[CH_KEYS(ch)], SZ_FNAME)
		newkey = YES
	    }
	    call sfree (sp)

	case WKEYS:
	    call smark (sp)
	    call salloc (filename, SZ_FNAME, TY_CHAR)
	    call gargwrd (Memc[filename], SZ_FNAME)
	    if (nscan() == 1)
		call strcpy ("STDOUT", Memc[filename], SZ_FNAME)
	    if (streq (Memc[filename], "STDOUT"))
		call gdeactivate (gp, AW_CLEAR)
	    call put_keys (ch, gt, Memc[filename])
	    if (streq (Memc[filename], "STDOUT"))
		call greactivate (gp, AW_PAUSE)
	    call sfree (sp)

	case CUTOFF1,CUTOFF2,CUTOFF3,CUTOFF4,C1,C2,C3,C4: # :cutoffX
	    i = strdic (cmd, cmd, SZ_LINE,
			"|cutoff1|cutoff2|cutoff3|cutoff4|c1|c2|c3|c4|")
	    i = mod (i, CH_NCUTOFFS)
	    if (i == 0)
		i = CH_NCUTOFFS
	    call gargstr (cmd, SZ_LINE)
	    for (j = 1; IS_WHITE(cmd[j]); j = j + 1)
		;
	    if (cmd[j] == EOS) {
	        call printf ("cutoff%1d = %s\n")
		    call pargi (i)
		    call pargstr (Memc[CH_CUTOFF(ch, i)])
	    } else {
		if (streq (cmd[j], "\"\"") || streq (cmd[j], "''"))
		    call strcpy ("", Memc[CH_CUTOFF(ch, i)], SZ_LINE)
	    	else
		    if (bool_expr (cmd[j], db, true))
			call strcpy (cmd[j], Memc[CH_CUTOFF(ch, i)], SZ_LINE)
	    }

	case LOGIC: # :logic - List or set the logic parameter.
	    call gargwrd (cmd, SZ_LINE)
	    if (cmd[1] == EOS) {
		i = get_strdic (LOGIC_LIST, CH_LOGIC(ch), cmd, SZ_LINE)
		call printf ("logic = %s\n")
		    call pargstr (cmd)
	    } else {
		i = strdic (cmd, cmd, SZ_LINE, LOGIC_LIST)
		if (i > 0)
		    CH_LOGIC(ch) = i
		else {
		    call eprintf ("Warning: Illegal logic parameter (%s)\n")
			call pargstr (cmd)
		}
	    }

	case MARKER1,MARKER2,MARKER3,MARKER4,MARKER5,MARKER6,MARKER7,MARKER8,M1,M2,M3,M4,M5,M6,M7,M8: # :markerX
	    i = strdic (cmd, cmd, SZ_LINE, "|marker1|marker2|marker3|marker4|marker5|marker6|marker7|marker8|m1|m2|m3|m4|m5|m6|m7|m8|")
	    i = mod (i, CH_NMARKERS)
	    if (i == 0)
		i = CH_NMARKERS
	    call gargstr (cmd, SZ_LINE)
	    for (j = 1; IS_WHITE(cmd[j]); j = j + 1)
		;
	    if (cmd[j] == EOS) {
	        call printf ("marker%1d = %s\n")
		    call pargi (i)
		    call pargstr (Memc[CH_MARKER(ch, i)])
	    } else {
		if (streq (cmd[j], "\"\"") || streq (cmd[j], "''"))
		    call strcpy ("", Memc[CH_MARKER(ch, i)], SZ_LINE)
	    	else
	    	    if (test_marker (cmd[j], db, CH_MMARK(ch)) == OK)
			call strcpy (cmd[j], Memc[CH_MARKER(ch, i)], SZ_LINE)
	    }

	case COLOR1,COLOR2,COLOR3,COLOR4,COLOR5,COLOR6,COLOR7,COLOR8,L1,L2,L3,L4,L5,L6,L7,L8: # :colorX
	    i = strdic (cmd, cmd, SZ_LINE, "|color1|color2|color3|color4|color5|color6|color7|color8|l1|l2|l3|l4|l5|l6|l7|l8|")
	    i = mod (i, CH_NCOLORS)
	    if (i == 0)
		i = CH_NCOLORS
	    call gargstr (cmd, SZ_LINE)
	    for (j = 1; IS_WHITE(cmd[j]); j = j + 1)
		;
	    if (cmd[j] == EOS) {
	        call printf ("color%1d = %s\n")
		    call pargi (i)
		    call pargstr (Memc[CH_COLOR(ch, i)])
	    } else {
		if (streq (cmd[j], "\"\"") || streq (cmd[j], "''"))
		    call strcpy ("", Memc[CH_COLOR(ch, i)], SZ_LINE)
	    	else
	    	    if (test_color (cmd[j], db) == OK)
			call strcpy (cmd[j], Memc[CH_COLOR(ch, i)], SZ_LINE)
	    }

	case DEFMARKER: # :def_marker - List or set the default marker.
	    call gargwrd (cmd, SZ_LINE)
	    if (cmd[1] == EOS) {
		i = get_strdic (MARKS, CH_DEFMARKER(ch), cmd, SZ_LINE)
		call printf ("def_marker = %s\n")
		    call pargstr (cmd)
	    } else {
		i = strdic (cmd, cmd, SZ_LINE, MARKS)
		if (i == 0) {
		    call eprintf("Warning: Unrecognized marker (%s)\n")
			call pargstr (cmd)
		} else if (i == CH_MMARK(ch))
		    call eprintf ("Warning: Can't be the same as the marked objects marker\n")
		else
		    CH_DEFMARKER(ch) = i
	    }

	case XSIZE: # :xsize - List or set x sizing parameter.
	    call gargstr (cmd, CH_SZFUNCTION)
	    for (j = 1; IS_WHITE(cmd[j]); j = j + 1)
		;
	    if (cmd[j] == EOS) {
	        call printf ("xsize = %s\n")
		    call pargstr (Memc[CH_XSIZE(ch)])
	    } else {
		if (streq (cmd[j], "\"\"") || streq (cmd[j], "''")) {
		    call strcpy ("", Memc[CH_XSIZE(ch)], CH_SZFUNCTION)
		    newkey = YES
	    	} else
		    if (size_expr (cmd[j], db, true)) {
			call strcpy (cmd[j], Memc[CH_XSIZE(ch)], CH_SZFUNCTION)
			newkey = YES
		    }
	    }

	case YSIZE: # :ysize - List or set y sizing parameter.
	    call gargstr (cmd, CH_SZFUNCTION)
	    for (j = 1; IS_WHITE(cmd[j]); j = j + 1)
		;
	    if (cmd[j] == EOS) {
	        call printf ("ysize = %s\n")
		    call pargstr (Memc[CH_YSIZE(ch)])
	    } else {
		if (streq (cmd[j], "\"\"") || streq (cmd[j], "''")) {
		    call strcpy ("", Memc[CH_YSIZE(ch)], CH_SZFUNCTION)
		    newkey = YES
	    	} else
		    if (size_expr (cmd[j], db, true)) {
			call strcpy (cmd[j], Memc[CH_YSIZE(ch)], CH_SZFUNCTION)
			newkey = YES
		    }
	    }

	case FIELD: # :field - List or set the field parameter.
	    call gargwrd (cmd, SZ_LINE)
	    if (cmd[1] == EOS) {
		i = get_strdic (FIELD_LIST, CH_FIELD(ch), cmd, SZ_LINE)
		call printf ("field = %s\n")
		    call pargstr (cmd)
	    } else {
		i = strdic (cmd, cmd, SZ_LINE, FIELD_LIST)
		if (i > 0)
		    CH_FIELD(ch) = i
		else {
		    call eprintf ("Warning: Illegal field parameter (%s)\n")
			call pargstr (cmd)
		}
	    }

	case XCENTER: # :xcenter - List or set xcenter parameter.
	    call gargd (dval)
	    if (nscan() == 1) {
		call printf ("xcenter = %g\n")
		    call pargd (CH_XCENTER(ch))
	    } else
		CH_XCENTER(ch) = dval

	case YCENTER: # :ycenter - List or set ycenter parameter.
	    call gargd (dval)
	    if (nscan() == 1) {
		call printf ("ycenter = %g\n")
		    call pargd (CH_YCENTER(ch))
	    } else
		CH_YCENTER(ch) = dval

	case RADIUS: # :radius - List or set radius parameter.
	    call gargd (dval)
	    if (nscan() == 1) {
		call printf ("radius = %g\n")
		    call pargd (CH_RADIUS(ch))
	    } else
		CH_RADIUS(ch) = dval

	case MAXSIZE: # :max_size - List or set maximun marker size.
	    call gargr (rval1)
	    if (nscan() == 1) {
		call printf ("maxsize = %g\n")
		    call pargr (CH_MAXSIZE(ch))
	    } else {
		CH_MAXSIZE(ch) = rval1
		newkey = YES
	    }

	case MINSIZE: # :min_size - List or set minimum marker size..
	    call gargr (rval1)
	    if (nscan() == 1) {
		call printf ("minsize = %g\n")
		    call pargr (CH_MINSIZE(ch))
	    } else {
		CH_MINSIZE(ch) = rval1
		newkey = YES
	    }

	case OUTFORMAT: # :outformat - List or set the output format file.
	    call gargwrd (cmd, SZ_FNAME)
	    if (cmd[1] == EOS) {
		call printf ("outformat = %s\n")
		    call pargstr (Memc[CH_OUTFORMAT(ch)])
	    } else {
		call strcpy (cmd, Memc[CH_OUTFORMAT(ch)], SZ_FNAME)
	    }

	case NBINS: # :nbins - List or set number of histogram bins.
	    call gargi (ival)
	    if (nscan() == 1) {
		call printf ("nbins = %d\n")
		    call pargi (CH_NBINS(ch))
	    } else {
		CH_NBINS(ch) = ival
	    	newhisto = YES
	    }

	case Z1: # :z1 - List or set minimum histogram intensity.
	    call gargd (dval)
	    if (nscan() == 1) {
		call printf ("z1 = %g\n")
		    call pargd (CH_Z1(ch))
	    } else {
		CH_Z1(ch) = dval
	    	newhisto = YES
	    }

	case Z2: # :z2 - List or set minimum histogram intensity.
	    call gargd (dval)
	    if (nscan() == 1) {
		call printf ("z2 = %g\n")
		    call pargd (CH_Z2(ch))
	    } else {
		CH_Z2(ch) = dval
	    	newhisto = YES
	    }

	case SHOW: # :show [file] -Show the values of the selection parameters.
	    call gargwrd (cmd, SZ_LINE)
	    if (nscan() == 1)
		call strcpy ("STDOUT", cmd, SZ_FNAME)
	    if (streq (cmd, "STDOUT")) {
		call gdeactivate (gp, AW_CLEAR)
		call ch_show (ch, cmd)
		call greactivate (gp, AW_PAUSE)
	    } else {
		call ch_show (ch, cmd)
	    }

	case REPLACE: # :replace file var1 var2 var3 ...
	    call gargstr (cmd, SZ_LINE)
	    if (replace (gp, db, index, cmd) > 0) {
		call printf ("Save replacements with \":all\"\n")
		newsample = YES
	    }

	case RMARKS: # :rmarks file
	    call gargwrd (cmd, SZ_FNAME)
	    call read_marks (gp, db, index, marker, cmd, CH_MMARK(ch))
	    newgraph = YES

	case OUTLIERS: # :outliers - List or set whether to plot outliers.
	    call gargb (bval)
	    if (nscan() == 1) {
		call printf ("outliers = %b\n")
		    call pargb (CH_PLOTARROWS(ch))
	    } else {
		CH_PLOTARROWS(ch) = bval
	    	newgraph = YES
	    }

	case SORTER: # :sorter - List or set the sorting function.
	    call gargstr (cmd, CH_SZFUNCTION)
	    for (j = 1; IS_WHITE(cmd[j]); j = j + 1)
		;
	    if (cmd[j] == EOS) {
	        call printf ("sorter = %s\n")
		    call pargstr (Memc[CH_SORTER(ch)])
	    } else {
		if (streq (cmd[j], "\"\"") || streq (cmd[j], "''"))
		    call strcpy ("", Memc[CH_SORTER(ch)], CH_SZFUNCTION)
		else if (num_expr (cmd[j], db, true))
		    call strcpy (cmd[j], Memc[CH_SORTER(ch)],CH_SZFUNCTION)
	    }

	case MMARKER: # :mmarker - List or set the maker objects marker.
	    call gargwrd (cmd, SZ_LINE)
	    if (cmd[1] == EOS) {
		i = get_strdic (MARKS, CH_MMARK(ch), cmd, SZ_LINE)
		call printf ("mmarker = %s\n")
		    call pargstr (cmd)
	    } else {
		i = strdic (cmd, cmd, SZ_LINE, MARKS)
		if (i == 0) {
		    call eprintf("Warning: Unrecognized marker (%s)\n")
			call pargstr (cmd)
		} else if (i == CH_DEFMARKER(ch)) {
		    call eprintf ("Warning: Can't be the same as the default marker\n")
		} else {
		    do j = 1, nselected
			if (marker[j] == CH_MMARK(ch))
			    marker[j] = i
		    CH_MMARK(ch) = i
		    newgraph = YES
		}
	    }

	default:
	    call eprintf ("Warning: Colon command not recognized (%s)\n")
		call pargstr (cmd)
	}
end
