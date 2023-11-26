#{ Package script task for the MTOOLS package.

cl < "mtools$lib/zzsetenv.def"
package mtools, bin = mtoolsbin$

task	airchart	= "mtools$airchart/x_airchart.e"
task	chart		= "mtools$chart/x_chart.e"
task	$defitize	= "mtools$misc/defitize.cl"
task	$fitize	 	= "mtools$misc/fitize.cl"
task	format		= "mtools$misc/x_format.e"
task	gki2mng		= "mtools$gki2mng/x_gki2mng.e"
task	mysplot		= "mtools$mysplot/x_mysplot.e"
task	pca		= "mtools$pca/x_pca.e"

softools

clbye()
