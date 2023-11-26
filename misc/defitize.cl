# DEFITIZE -- Convert all fits files (those appended with ".fit") to images,
#	      restoring the old IRAF names for the images.

procedure defitize ()

begin

char	temp, in, out
int	len

temp = mktemp ("tmp")
rmfiles("-nv", ".", "-only", ".fit", > temp)
list = temp
while (fscan(list, in) != EOF) {
	len = strlen(in)
	out = substr(in, 1, len-4)
	print(in)
	rfits (in,, out, make_image=yes, long_header=no, short_header=no,
	  datatype="", blank=0., scale=yes, oldirafname=no, offset=0)
	delete (in, verify-, allversions+)
}
delete (temp, verify=no, allversions+)
end
