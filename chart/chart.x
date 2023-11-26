task chart

# formerly pkg$utilities/t_curfit.x

include <iraf.h>
include	<fset.h>
include	<pkg/gtools.h>
include "chart.h"

# CHART -- CHART database analysis program.

procedure chart ()

pointer	db, index, marker, color
pointer	gt[CH_NGKEYS+1], ch
char	device[SZ_FNAME]
pointer	gt_init()
int	fstati(), read_db(), i, nrecords
pointer	gp, gopen()
#bool	streq()
#int	read_table()

begin
	# First get cl parameters.

	do i = 1, CH_NGKEYS+1
	    gt[i] = gt_init ()

	call ch_open (ch, db, gt)

	if (fstati (STDIN, F_REDIR) == YES) {
	    call strcpy ("STDIN", Memc[CH_DATABASE(ch)], SZ_FNAME)
	} else {
	    call clgstr ("database", Memc[CH_DATABASE(ch)] , SZ_FNAME)
	}

	call clgstr ("device", device, SZ_FNAME)

#	if (streq (Memc[CH_DBFORMAT(ch)], "STSDAS"))
#	    nrecords = read_table (db, Memc[CH_DATABASE(ch)])
#	else
	    nrecords = read_db (db, Memc[CH_DATABASE(ch)])
	if (nrecords == 0)
	    call fatal (0, "Database is empty")

	call malloc (index, nrecords+1, TY_INT)
	call malloc (marker, nrecords, TY_INT)
	call malloc (color, nrecords, TY_INT)

	do i = 1, CH_NGKEYS+1
            call gt_sets (gt[i], GTPARAMS, Memc[CH_DATABASE(ch)])

	gp = gopen (device, NEW_FILE, STDGRAPH)

	call cursor (ch, gp, "cursor", gt, db, Memi[index],
	    Memi[marker], Memi[color], nrecords)

	call gclose (gp)

	call flush (STDOUT)
	call mfree (index, TY_INT)
	call mfree (marker, TY_INT)
	     
	call close_format(db)
	call ch_close(ch)
	do i = 1, CH_NGKEYS+1
	    call gt_free (gt[i])
end
