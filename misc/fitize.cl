# FITIZE -- Write all images in the currect directory, and its subdirectories,
#           to FITS images, and then delete the images.
#
# The suffix ".fit" will be appended to each image name for naming the
# created FITS files (i.e.  "image.imh" --> "image.fit").

procedure fitize ()

begin

char	temp, in, out
int	len

temp = mktemp ("tmp")
rmfiles("-nv", ".", "-only", ".imh") | match("/..", "STDIN", stop+, 
	print_file_names-, metacharacters-, > temp)
list = temp
while (fscan(list, in) != EOF) {
	len = strlen(in)
	out = substr(in, 1, len-3) + "fit"
	print(in)
	wfits (in, out, yes, 1., 0., make_image=yes, long_header=no,
	  short_header=no, bitpix=0, blocking_fac=1,
	  scale=yes, autoscale=yes)
	imdelete (in, verify-)
}
delete (temp, verify=no, allversions+)
end
