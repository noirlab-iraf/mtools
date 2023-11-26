task	pca

define	IBUFLEN		1000

# The supported methods		# Analysis on ...
define	METHODS		"|sums|covariance|correlation|"
define	SUMS_OF_SQUARES	1	# ... sums of squares & cross-products matrix
define	COVARIANCE	2	# ... covariance matrix
define	CORRELATION	3	# ... correlation matrix
 
procedure pca ()

real	tmp
int	n, m, method, iprint, buflen, len, ierr, i, j, scan(), nscan(),clgwrd()
pointer	data, tdata, a1, w1, w2, a2
bool	verbose, clgetb()
char	buffer[SZ_LINE]

begin

# Get cl parameters
method = clgwrd ("method", buffer, SZ_LINE, METHODS)
if (method == 0)
    call fatal (0, "Unrecognized method")
verbose = clgetb ("verbose")
if (verbose)
    iprint = 3
else
    iprint = 2

# Allocate space
call malloc (data, IBUFLEN, TY_REAL)
len = IBUFLEN

# Read in first line, determining m
# Skip comment lines
repeat {
    if (scan() == EOF)
    	call error (0, "No input")
    call gargwrd (buffer, SZ_LINE)
    if (buffer[1] != '#') {
	call reset_scan()
	break
    }
}
m = 0
call gargr (tmp)
while (nscan() == m + 1) {
    m = m + 1
    if (m > len) {
	call realloc (data, len+IBUFLEN, TY_REAL)
	len = len + IBUFLEN
    }
    Memr[data+m-1] = tmp
    call gargr (tmp)
}

# Read the rest of the data
buflen = 10 * m
len = buflen
call realloc (data, len, TY_REAL)
n = 1
while (scan() != EOF) {
    n = n + 1
    if (n*m > len) {
	call realloc (data, len+buflen, TY_REAL)
	len = len + buflen
    }
    do i = 1, m
	call gargr (Memr[data+(n-1)*m+i-1])
    if (nscan() != m)
	call error (0,"Incomplete input line")
}

# Now must transpose the matrix, as it was read in wrong
call realloc (data, n*m, TY_REAL)
call malloc (tdata, n*m, TY_REAL)
do i = 1, m
    do j = 1, n
	Memr[tdata+(i-1)*n+j-1] = Memr[data+(j-1)*m+i-1]
call mfree (data, TY_REAL)

# Allocate the additional arrays
call malloc (a1, m*m, TY_REAL)
call malloc (w1, m, TY_REAL)
call malloc (w2, m, TY_REAL)
call malloc (a2, m*m, TY_REAL)

# Do the principal component analysis
call mh (n, m, Memr[tdata], method, iprint, Memr[a1], Memr[w1], Memr[w2],
          Memr[a2], ierr)

# Clear memory
call mfree (tdata, TY_REAL)
call mfree (a1, TY_REAL)
call mfree (w1, TY_REAL)
call mfree (w2, TY_REAL)
call mfree (a2, TY_REAL)

end
