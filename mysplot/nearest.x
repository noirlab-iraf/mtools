# NEAREST -- Find the nearest feature to a given coordinate.

include "mark.h"

# Taken from onedspec$identify/idnearest.x

int procedure nearest (fitnear, frest, visible, ztype, show_shifted,
		       show_unshifted, z, nfeatures)
real	fitnear			# Coordinate to find nearest feature
real	frest[ARB]		# Array of wavelengths
bool	visible[ARB]		# Is line visible?
int	ztype[ARB]		# Shifted or non-shifted line
bool	show_shifted		# Show the shifted lines?
bool	show_unshifted		# Show the unshifted lines?
real	z			# Current redshift
int	nfeatures		# Number of features

int	i, current
real	delta, delta1

begin
	if (nfeatures < 1)
	    return (0)

	# Find first visible feature
	for (current = 1; current <= nfeatures; current = current + 1)
	    if (visible[current] &&
		((show_shifted   && ztype[current] == SHIFTED) ||
		 (show_unshifted && ztype[current] == NOTSHIFTED)))
		break
	if (current > nfeatures)
	    return (0)
	delta = abs (frest[current] * (1. + z) - fitnear)

	# Find closest visible feature
	do i = current+1, nfeatures {
	    if (! visible[current] ||
		((! show_shifted   && ztype[current] == SHIFTED) ||
		 (! show_unshifted && ztype[current] == NOTSHIFTED)))
		next
	    delta1 = abs (frest[i] * (1. + z) - fitnear)
	    if (delta1 < delta) {
		current = i
		delta = delta1
	    }
	}
	return (current)
end
