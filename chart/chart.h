include "pointer.h"

# The CHART data structure

define	CH_SZFUNCTION	80  	    	# Maximum size of function strings
define	CH_NCUTOFFS	4		# Maximum Number of cutoffs
define	CH_NMARKERS	8		# Maximum number of marker definitions
define	CH_NCOLORS	8		# Maximum number of color definitions
define	CH_NGKEYS	5		# Number of graph keys
define	CH_SZKEY    	12  	    	# Size of key substructure
define	CH_OFFSET	(24+CH_NMARKERS+CH_NCUTOFFS+CH_NCOLORS) # offset 1
define	CH_LENSTRUCT    (CH_OFFSET+CH_SZKEY*CH_NGKEYS+6) #Size of the CHART str

# Selection parameters

define	CH_XCENTER	Memd[P2D($1)]	# x-axis center for field cutoff
define	CH_YCENTER	Memd[P2D($1+2)]	# y-axis center for field cutoff
define	CH_RADIUS	Memd[P2D($1+4)]	# radius for field cutoff
define	CH_Z1	    	Memd[P2D($1+6)] # lowest value for histogram
define	CH_Z2	    	Memd[P2D($1+8)] # highest value for histogram
define	CH_LOGIC	Memi[$1+10] 	# logic used to combine sample cutoffs
define	CH_DEFMARKER	Memi[$1+11] 	# default marker
define	CH_MMARK   	Memi[$1+12]	# Mark type for marked points
define	CH_XSIZE	Memp[$1+13] 	# function used to size markers in x
define	CH_YSIZE	Memp[$1+14] 	# function used to size markers in y
define	CH_SORTER   	Memp[$1+15] 	# function used to sort lists
define	CH_FIELD	Memi[$1+16] 	# type of field cutoff to use
define	CH_MAXSIZE	Memr[P2R($1+17)]# maximum marker size in NDC units
define	CH_MINSIZE	Memr[P2R($1+18)]# minimum marker size in NDC units
define	CH_KEYS		Memp[$1+19] 	# graph key description file
define	CH_DATABASE  	Memp[$1+20] 	# database file
define	CH_DBFORMAT  	Memp[$1+21] 	# database format file
define	CH_OUTFORMAT  	Memp[$1+22] 	# format file for output
define	CH_NBINS    	Memi[$1+23] 	# number of bins in histogram
define	CH_CUTOFF	Memp[$1+24+($2-1)]  # sample cutoff definitions
define	CH_MARKER	Memp[$1+24+CH_NCUTOFFS+($2-1)]	# marker definitions
define	CH_COLOR	Memp[$1+24+CH_NCUTOFFS+CH_NMARKERS+($2-1)] # color def

# User display parameters  CH_XXX(ch, gkey, axis)
define	CH_DEFINED Memb[$1+CH_OFFSET+CH_SZKEY*($2-1)]            # Key defined?
define	CH_SQUARE  Memb[$1+CH_OFFSET+CH_SZKEY*($2-1)+1]	         # Square axis?
define	CH_AXIS	   Memp[$1+CH_OFFSET+CH_SZKEY*($2-1)+2+($3-1)] # axis functn
define	CH_UNIT    Memp[$1+CH_OFFSET+CH_SZKEY*($2-1)+4+($3-1)] # Axis units
define	CH_AXSIZE  Memp[$1+CH_OFFSET+CH_SZKEY*($2-1)+6+($3-1)] # Axis sizer
define	CH_FLIP	   Memb[$1+CH_OFFSET+CH_SZKEY*($2-1)+8+($3-1)] # Flip axis?
define	CH_ERROK   Memb[$1+CH_OFFSET+CH_SZKEY*($2-1)+10+($3-1)] # Err size OK?

# Miscellaneous
define	CH_NSELECTED	Memi[$1+CH_OFFSET+CH_SZKEY*CH_NGKEYS]   #Sample size
define	CH_LOG	    	Memb[$1+CH_OFFSET+CH_SZKEY*CH_NGKEYS+1] #log histogram?
define	CH_PLOTARROWS 	Memb[$1+CH_OFFSET+CH_SZKEY*CH_NGKEYS+2] #plot outliers
define	CH_AUTOSCALE	Memb[$1+CH_OFFSET+CH_SZKEY*CH_NGKEYS+3] #autoscale hist
define	CH_TOPCLOSED	Memb[$1+CH_OFFSET+CH_SZKEY*CH_NGKEYS+4] #close top bin?
define	CH_PLOTTYPE 	Memi[$1+CH_OFFSET+CH_SZKEY*CH_NGKEYS+5] #hist line ty

# Default help file and prompt
define	CH_DEFHELP	"chart$chart.key"
define	CH_PROMPT	"chart cursor options"

# Histogram plot types
define	HGM_TYPES   	"|line|box|"
define	HGM_LINE   	1   	    	# line vectors for histogram plot
define	HGM_BOX	    	2   	    	# box vectors for histogram plot
