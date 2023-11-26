#++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#  output array
#
#------------------------------------------------------
procedure outmat(n,m,array)
int n, m
real array[n,m]

int k1, k2

	 begin

         do k1 = 1, n {
	    do k2 = 1, m {
		call printf ("  %8.4f")
		call pargr (array[k1,k2])
	    }
	    call printf ("\n")
	 }
         end
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#  output half of (symmetric) array.
#
#-------------------------------------------------------
procedure outhmt(itype,ndim,array)
int itype,ndim
real array[ndim,ndim]

int k1, k2

	 begin

         if (itype == 1)
            call printf ("\nsums of squares & cross-products matrix follows.\n\n")
         if (itype == 2)
	    call printf ("\ncovariance matrix follows.\n\n")
         if (itype == 3)
 	    call printf ("\ncorrelation matrix follows.\n\n")

         do k1 = 1, ndim {
	    do k2 = 1, k1 {
		call printf ("  %8.4f")
		call pargr (array[k1,k2])
	    }
	    call printf ("\n")
	 }
         end
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#  output eigenvalues in order of decreasing value.
#
#-------------------------------------------------------
procedure outevl(n,nvals,vals)
int n, nvals
real       vals[nvals]

real tot, cum, vpc, vcpc
int k, m

	 begin

         tot = 0.0
         do k = 1, nvals
            tot = tot + vals[k]

	 call printf ("\neigenvalues follow.\n\n")
         cum = 0.0
         k = nvals + 1

         m = nvals

#        (we only want min(nrows,ncols) eigenvalues output:)
         m = min(n,nvals)

         call printf (" eigenvalues        as percentages    culum. percentages\n")
         call printf (" -----------        --------------    ------------------\n")
	 repeat {
         k = k - 1
         cum = cum + vals[k]
         vpc = vals[k] * 100.0 / tot
         vcpc = cum * 100.0 / tot
	 call printf ("%13.4f       %10.4f          %10.4f\n")
	 call pargr (vals[k])
	 call pargr (vpc)
	 call pargr (vcpc)
         vals[k] = vcpc
         if (k <= nvals-m+1)
	    break
	 }
         end
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#          output first seven eigenvectors associated with
#          eigenvalues in descending order.
#
#----------------------------------------------------------
procedure outevc(n,ndim,vecs)
int n, ndim
real       vecs[ndim,ndim]

int num, k1, k2

	 begin

         num = min(min(n,ndim),7)

         call printf ("\neigenvectors follow.\n\n")
         call printf ("  vble.   ev-1    ev-2    ev-3    ev-4    ev-5    ev-6    ev-7\n")
         call printf (" ------  ------  ------  ------  ------  ------  ------  ------\n")
         do k1 = 1, ndim {
	    call printf ("%5d  ")
	    call pargi (k1)
	    do k2 = 1, num {
		call printf ("%8.4f")
		call pargr (vecs[k1,ndim-k2+1])
	    }
	    call printf ("\n")
	 }
         end
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#  output projections of row-points on first 7 principal components.
#
#-----------------------------------------------------------
procedure outprx(n,m,prjn)
int n, m
real    prjn[n,m]

int num, k, j

	 begin

         num = min(m,7)
	 call printf ("\nprojections of row-points follow.\n\n")
         call printf (" object  proj-1  proj-2  proj-3  proj-4  proj-5  proj-6  proj-7\n")
         call printf (" ------  ------  ------  ------  ------  ------  ------  ------\n")
         do k = 1, n {
	    call printf ("%5d  ")
	    call pargi (k)
	    do j = 1, num {
	    	call printf ("%8.4f")
		call pargr (prjn[k,j])
	    }
	    call printf ("\n")
	 }
         end
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#  output projections of columns on first 7 principal components.
#
#-----------------------------------------------------------
procedure outpry(m,prjns)
int m
real    prjns[m,m]

int num, k, j

	 begin

         num = min(m,7)
	 call printf ("\nprojections of column-points follow.\n\n")
         call printf ("  vble.  proj-1  proj-2  proj-3  proj-4  proj-5  proj-6  proj-7\n")
         call printf (" ------  ------  ------  ------  ------  ------  ------  ------\n")
         do k = 1, m {
	    call printf ("%5d  ")
	    call pargi (k)
	    do j = 1, num {
	    	call printf ("%8.4f")
		call pargr (prjns[k,j])
	    }
	    call printf ("\n")
	 }
         end
