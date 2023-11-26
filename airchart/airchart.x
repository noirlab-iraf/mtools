task	airchart

include <math.h>
include <ctype.h>
include <gio.h>
include <gset.h>

define	PRE_TWILIGHT	0.5	# Time before twilight for observing (hours)
define	MAX_AIRMASS	2.5	# Maximum airmass to plot
define	ZD_AST_TWI	108.	# Sun's zenith distance for astronomical
				# twilight (degrees).
define	SZ_DAY_OF_WEEK	3	# Size of character string for day_of_week
define	SZ_TITLE	4*SZ_LINE	# Size of title string
define	SZ_SITE		10	# Size of site string
define	NITERATIONS	10	# Number of iterations for rise/set times

# RISE_SET procedure options
define	RISE		1
define	SET		2

define	SZ_MONTH	10
define	MONTHS	"|January|February|March|April|May|June|July|August|September|October|November|December|"

define	NYINTERVALS 	24  	# Number of label intervals along y-axis

procedure airchart ()

int	year		# UT Year
int	month		# UT Month
int	day		# UT Day of the month
real	longitude	# Earth west longitude
real	latitude	# Earth latitude
int	time_zone	# Hours less than Greenwich time
char	site[SZ_SITE]	# Site name

int	clgeti(), clgwrd(), fd, open(), ip, getline(), nscan(), i, nsteps, junk
double	epoch, ut, dlongitude, ast_mst(), jd, ast_julday(), mut, mepoch
real	dawn, dusk, rise_set(), airmass(), zd_airmass(), max_zd, mass, oldmass
real	sun_ra, sun_dec, ra, dec, clgetr(), rise, set, st, time, moonset
real	lmst1, lmst2, moon_ra, moon_dec, parallax, semidiameter, moonrise
real	illuminated, moon_long, moon_lat, sun_long, illuminated_fraction()
pointer	lbuf, sp, gp, gopen()
int	nticks, first, need_label, first_y, label_y, array_y[NYINTERVALS]
real	first_time, first_mass, first_st
bool	streq()
char	filename[SZ_FNAME], device[SZ_FNAME], object[SZ_FNAME], buffer[SZ_LINE]
char	day_of_week[SZ_DAY_OF_WEEK], month_name[SZ_MONTH], title[SZ_TITLE]
double	rain, decin, equinoxin, raout, decout

begin
    call smark (sp)
    call salloc (lbuf, SZ_LINE, TY_CHAR)

    # Get parameters
    call clgstr ("input", filename, SZ_FNAME)
    year = clgeti ("year")
    month = clgwrd ("month", month_name, SZ_MONTH, MONTHS)
    if (month == 0)
	call fatal ("Ilegal value for month parameter")
    day = clgeti ("day")
    if (day < 1 || (month == 2 && day > 29) || (month == 2 && ! (mod (year,4)
	== 0 && mod (year, 400) != 0) && day > 28) || (day > 31) ||
	((month == 4 || month == 6 || month == 9 || month == 11) && day > 30))
	call fatal ("Ilegal value for day parameter")
    time_zone = clgeti ("time_zone")
    longitude = clgetr ("longitude")
    dlongitude = longitude
    latitude = clgetr ("latitude")
    call clgstr ("site", site, SZ_SITE)
    call clgstr ("device", device, SZ_FNAME)

    # Compute the Julian epoch for midnight zone time, then the
    # astronomical twilights. Iterate a few times to get a good
    # position of the sun at the times of dawn and dusk.
    ut = time_zone
    call ast_date_to_epoch (year, month, day, ut, epoch)
    jd = ast_julday (epoch)
    call ast_day_of_week (jd, junk, day_of_week, SZ_DAY_OF_WEEK)
    dawn = 0.
    dusk = 0.
    for (i = 1; i <= NITERATIONS; i = i + 1) {
	mut = dawn + time_zone
    	call ast_date_to_epoch (year, month, day, mut, mepoch)
    	call sun  (mepoch, sun_ra, sun_dec, sun_long)
    	dawn = rise_set (sun_ra, sun_dec, longitude, latitude, mepoch,
			 time_zone, ZD_AST_TWI, RISE)
	mut = dusk + time_zone
    	call ast_date_to_epoch (year, month, day, mut, mepoch)
    	call sun  (mepoch, sun_ra, sun_dec, sun_long)
    	dusk = rise_set (sun_ra, sun_dec, longitude, latitude, mepoch,
			 time_zone, ZD_AST_TWI, SET )
	dusk = dusk - 24.
    }
    dawn = dawn + PRE_TWILIGHT
    dusk = dusk - PRE_TWILIGHT
    call sun  (epoch, sun_ra, sun_dec, sun_long)

    # Calculate the moonrise and moonset. Iterate a few times to get a good
    # position of the moon at the times of moonrise and moonset.
    moonrise = time_zone
    moonset  = time_zone
    for (i = 1; i <= NITERATIONS; i = i + 1) {
	mut = moonrise
	call ast_date_to_epoch (year, month, day, mut, mepoch)
	call moon (mepoch, moon_ra, moon_dec, moon_long, moon_lat, parallax,
		   semidiameter)
	moonrise = rise_set (moon_ra,moon_dec,longitude, latitude, mepoch,
		   time_zone, 90.+(34./60.)+semidiameter-parallax, RISE)
	mut = moonset
	call ast_date_to_epoch (year, month, day, mut, mepoch)
	call moon (mepoch, moon_ra, moon_dec, moon_long, moon_lat, parallax,
		   semidiameter)
	moonset  = rise_set (moon_ra,moon_dec,longitude, latitude, mepoch,
		   time_zone, 90.+(34./60.)+semidiameter-parallax, SET)
	if (moonrise > dawn)
	    moonrise = moonrise - 24.
	if (moonset > dusk + 24.)
	    moonset  = moonset  - 24.
    }
    if (moonrise < 0.)
	moonrise = moonrise + 24.
    if (moonset  < 0.)
	moonset  = moonset  + 24.

    # Calculate the ra and dec of the moon, and its disk's illuminated
    # fraction, at midnight.
    call moon (epoch, moon_ra, moon_dec, moon_long, moon_lat, parallax,
	       semidiameter)
    illuminated = illuminated_fraction (moon_long, moon_lat, sun_long)

    # Compute the zenith distance corresponding to the maximum airmass to plot
    max_zd = zd_airmass (MAX_AIRMASS)

    # Set up graph
    gp = gopen (device, NEW_FILE, STDGRAPH)
    call sysid (title, SZ_TITLE)
    call sprintf (buffer, SZ_LINE, "Airmass chart for UT date %s %s %d, %d at %s (time zone = %d h)\n")
	call pargstr (day_of_week)
	call pargstr (month_name)
	call pargi (day)
	call pargi (year)
	call pargstr (site)
	call pargi (time_zone)
    call strcat ("\n", title, SZ_TITLE)
    call strcat (buffer, title, SZ_TITLE)
    call sprintf (buffer, SZ_LINE,
	    "evening ast. twilight = %5.0m    morning ast. twilight = %5.0m\n")
	call pargr (dusk + PRE_TWILIGHT + 24.)
	call pargr (dawn - PRE_TWILIGHT)
    call strcat (buffer, title, SZ_TITLE)
    call sprintf (buffer, SZ_LINE,
	    "moon: ra = %.0m  dec = %.0m  rise = %.0m  set = %.0m  ill. frac. = %.2f")
	call pargr (moon_ra)
	call pargr (moon_dec)
	call pargr (moonrise)
	call pargr (moonset)
	call pargr (illuminated)
    call strcat (buffer, title, SZ_TITLE)
    call strcat ("\n\n\n\n", title, SZ_TITLE)

    # Draw top and side axes, labelled in sidereal time units
    nticks = int((MAX_AIRMASS - 1) / 0.5 + 0.2) + 1
    call gseti (gp, G_YNMAJOR, nticks)
    call gseti (gp, G_YNMINOR, 0)
    call gseti (gp, G_XDRAWAXES, 2)
    call gseti (gp, G_YDRAWAXES, 3)
    ut = dusk + time_zone
    call ast_date_to_epoch (year, month, day, ut, mepoch)
    lmst1 = ast_mst (mepoch, dlongitude)
    ut = dawn + time_zone
    call ast_date_to_epoch (year, month, day, ut, mepoch)
    lmst2 = ast_mst (mepoch, dlongitude)
    if (lmst2 < lmst1)
	lmst1 = lmst1 - 24.
    nticks = int(lmst2) - int(lmst1)
    call gseti (gp, G_XNMAJOR, nticks)
    call gseti (gp, G_XNMINOR, 0)
    call gswind (gp, lmst1, lmst2, 1., MAX_AIRMASS)
    call myglabax (gp, title, "local sidereal time", "airmass")
 
    # Draw bottom axis
    call gswind (gp, dusk, dawn, 1., MAX_AIRMASS)
    call gseti (gp, G_YDRAWAXES, 0)
    call gseti (gp, G_XDRAWAXES, 1)
    nticks = int(dawn) - int(dusk)
    call gseti (gp, G_XNMAJOR, nticks)
    call gseti (gp, G_XNMINOR, 0)
    call myglabax (gp, "", "zone time", "")

    # Process each set of ra and dec
    do i = 1, NYINTERVALS
	if (mod(i, 3) == 2)
	    array_y[i] = NO
	else
	    array_y[i] = YES
    call gflush (gp)
    fd = open (filename, READ_ONLY, TEXT_FILE)
    if (streq(filename, "STDIN")) {
	call printf ("Object (name ra dec equinox): ")
	call flush (STDOUT)
    }
    while (getline (fd, Memc[lbuf]) != EOF) {
	if (streq(filename, "STDIN")) {
	    call printf ("Object (name ra dec equinox): ")
	    call flush (STDOUT)
	}
	# Skip comment lines and blank lines.
	if (Memc[lbuf] == '#')
	    next
	for (ip=lbuf;  IS_WHITE(Memc[ip]);  ip=ip+1)
	    ;
	if (Memc[ip] == '\n' || Memc[ip] == EOS)
	    next

	# Decode the points to be plotted.
	call sscan (Memc[ip])
	    call gargwrd (object, SZ_FNAME)
	    call gargd (rain)
	    call gargd (decin)
	    call gargd (equinoxin)

	if (nscan() < 4) {
	    call gdeactivate (gp, 0)
	    call eprintf ("ERROR: skipping illegal line -- %s")
		call pargstr(Memc[lbuf]);
	    call greactivate (gp, 0)
	    next
	}

	call ast_precess(rain, decin, equinoxin, raout, decout, epoch)
	ra = raout
	dec = decout
	
	rise = dusk
	set = dawn
	nsteps = int (10 * (set - rise))
	first = YES
	need_label = NO
	do i = 0, nsteps {
	    time = rise + i * (set - rise) / nsteps
	    mut = time + time_zone
	    call ast_date_to_epoch (year, month, day, mut, mepoch)
	    st = ast_mst (mepoch, dlongitude)
	    mass = airmass (st, ra, dec, latitude)
	    if (mass < MAX_AIRMASS) {
	      if (first == YES) {
		need_label = YES
		first_y = nint(NYINTERVALS * (mass - 1) / (MAX_AIRMASS - 1))
		first_time = time
		first_st = st
		if (i > 0) {
		    time = time - ((MAX_AIRMASS - mass) / (oldmass - mass)) * 
			   (set - rise) / nsteps
		    mass = MAX_AIRMASS
		}
		call gamove (gp, time, mass)
		first = NO
	      } else
		call gadraw (gp, time, mass)
	      label_y = nint(NYINTERVALS * (mass - 1) / (MAX_AIRMASS - 1))
	      if (need_label == YES && array_y[label_y] == NO) {
		first_st = st + 0.25
		first_mass = airmass (first_st, ra, dec, latitude)
		if (first_mass >= mass)	# Object is setting
		  call gtext (gp, time + (dawn - dusk) / 500.,
			       mass + (MAX_AIRMASS - 1) / 1000., object,"v=t")
		else	 	# Object is rising
		  call gtext (gp, time + (dawn - dusk) / 500.,
			       mass + (MAX_AIRMASS - 1) / 1000., object,"v=b")
		array_y[label_y] = YES
		need_label = NO
	      }
	    } else if (first == NO) {
		time = time - ((mass - MAX_AIRMASS) / (mass - oldmass)) *
		       (set - rise) / nsteps
		call gadraw (gp, time, MAX_AIRMASS)
		first = YES
		if (need_label == YES) {
		  if (first_st > ra) 	# Object is setting
		    call gtext (gp, first_time + (dawn - dusk) / 500.,
			   first_mass + (MAX_AIRMASS - 1) / 1000., object,"v=t")
		  else	 	# Object is rising
		    call gtext (gp, first_time + (dawn - dusk) / 500.,
			  first_mass + (MAX_AIRMASS - 1) / 1000., object,"v=b")
		  need_label = NO
		}
	    }
	    oldmass = mass
	}
        call gflush (gp)
	if (streq(filename, "STDIN")) {
	    call printf ("Object (name ra dec): ")
	    call flush (STDOUT)
	}
    }
    call close (fd)
    call gclose (gp)
    call sfree (sp)    
end

# SUN -- Compute the ra and dec of the sun for the specified Julian
# epoch.  Returns the right ascension in hours and declination in degrees,
# accurate to 0.01 degrees (formula from Astronomical Almanac 1989, p. C24).

define	JD_2000		2451545.D0

procedure sun (epoch, ra, dec, longitude)
double	epoch		# Julian epoch
real	ra		# sun's right ascension for given epoch
real	dec		# sun's declination for given epoch
real	longitude	# sun's ecliptic longitude (degrees)

double	mlong, elong, anomaly, obliquity
double	jd, ast_julday(), n

begin
	jd = ast_julday (epoch)		  # Julian day
	n = jd - JD_2000
	mlong = 280.460D0 + 0.9856474D0 * n # mean longitude
	anomaly = 357.528D0 + 0.9856003D0 * n  # Mean anomaly
	if (mlong < 0.D0)
		while (mlong < 0.D0)
			mlong = mlong + 360.D0
	else
		mlong = mod (mlong, 360.D0)
	if (anomaly < 0.D0)
		while (anomaly < 0.D0)
			anomaly = anomaly + 360.D0
	else
		anomaly = mod (anomaly, 360.D0)
	elong = mlong + 1.915D0 * sin (DEGTORAD(anomaly)) + 0.020D0 *
		sin (DEGTORAD(2.D0 * anomaly)) # ecliptic longitude
	if (elong < 0.D0)
		while (elong < 0.D0)
			elong = elong + 360.D0
	else
		elong = mod (elong, 360.D0)
	longitude = elong
	obliquity = 23.439D0 - 0.000004D0 * n
	ra  = RADTODEG(atan(cos(DEGTORAD(obliquity)) * tan(DEGTORAD(elong))))
	if (ra < 0.)
		ra = ra + 180.
	if (abs(ra - elong) > 90.D0)
		if (ra > elong)
			ra = ra - 180.
		else
			ra = ra + 180.
	ra = ra / 15.
	dec = RADTODEG(asin(sin(DEGTORAD(obliquity)) * sin(DEGTORAD(elong))))
end

# MOON -- Compute the ra and dec of the moon for the specified Julian
# epoch.  Returns the right ascension in hours and declination in degrees,
# accurate to 0.01 degrees (formula from Astronomical Almanac 1989, p. D46).

procedure moon (epoch, ra, dec, longitude, latitude, parallax, semidiameter)
double	epoch		# Julian epoch
real	ra		# Moon's right ascension for given epoch
real	dec		# Moon's declination for given epoch
real	longitude	# Moon's ecliptic longitude for given epoch
real	latitude	# Moon's ecliptic latitude for given epoch
real	parallax	# Horizontal paraalax
real	semidiameter	# Semidiameter

double	jd, ast_julday(), t, l, m, n

begin
    jd = ast_julday (epoch)		  # Julian day
    t = (jd - JD_2000) / 36525.
    longitude = 218.32 + 481267.883 * t +
		6.29 * sin (DEGTORAD(134.9 + 477198.85 * t)) -
		1.27 * sin (DEGTORAD(259.2 - 413335.38 * t)) +
		0.66 * sin (DEGTORAD(235.7 + 890534.23 * t)) +
		0.21 * sin (DEGTORAD(269.9 + 954397.70 * t)) -
		0.19 * sin (DEGTORAD(357.5 +  35999.05 * t)) -
		0.11 * sin (DEGTORAD(186.6 + 966404.05 * t))
    latitude  = 5.13 * sin (DEGTORAD( 93.3 + 483202.03 * t)) +
		0.28 * sin (DEGTORAD(228.2 + 960400.87 * t)) -
		0.28 * sin (DEGTORAD(318.3 +   6003.18 * t)) -
		0.17 * sin (DEGTORAD(217.6 - 407332.20 * t))
    longitude = mod (longitude, 360.)
    latitude = mod (latitude, 360.)
    l = cos (DEGTORAD(latitude)) * cos (DEGTORAD(longitude))
    m = 0.9175 * cos (DEGTORAD(latitude)) * sin (DEGTORAD(longitude)) -
	0.3978 * sin (DEGTORAD(latitude))
    n = 0.3978 * cos (DEGTORAD(latitude)) * sin (DEGTORAD(longitude)) +
	0.9175 * sin (DEGTORAD(latitude))
    parallax = 0.9508 +
		0.0518 * cos (DEGTORAD(134.9 + 477198.85 * t)) +
		0.0095 * cos (DEGTORAD(259.2 - 413335.38 * t)) +
		0.0078 * cos (DEGTORAD(235.7 + 890534.23 * t)) +
		0.0028 * cos (DEGTORAD(269.9 + 954397.70 * t))
    semidiameter = 0.2725 * parallax
    ra = RADTODEG(atan2 (m, l)) / 15.
    while (ra < 0.)
	ra = ra + 24.
    dec = RADTODEG(asin (n))
end

# ILLUMINATED_FRACTION -- Compute the illuminated fraction of the moon's disk
# (formula from Jean Meeus's Astronomical Formula for Calculators, 3rd Edition,
# pp. 155).

real procedure illuminated_fraction (moon_long, moon_lat, sun_long)
real	moon_long	# Moon's ecliptic longitude (degrees)
real	moon_lat	# Moon's ecliptic latitude (degrees)
real	sun_long	# Sun's longitude (degrees)

real	d

begin
    d = acos (cos (DEGTORAD(moon_long - sun_long)) * cos (DEGTORAD(moon_lat)))
    d = RADTODEG(d)
    return ((1 + cos (DEGTORAD(180. - d - 0.1468 * sin (DEGTORAD(d))))) / 2)
end

# RISE_SET -- Returns the local time of rise or set for an object with the
# specified ra and dec,at a site with the specified latitide and longitude
# and time zone, for the given Julian epoch.  Rising and setting are defined
# with respect to the specified zenith distance.  Returns -1 if object
# doesn't rise or set (formula from Astronomical Formulae
# for Calculators, 3rd edition, Jean Meeus, p. 47 and
# Astronomical Almanac 1989, p. A12).

real procedure rise_set (ra, dec, longitude, latitude, epoch, time_zone, zd,
			 which)
real	ra		# Right ascension (hours)
real	dec		# declination (degrees)
real	longitude	# Earth west longitude (degrees)
real	latitude	# Earth latitude (degrees)
double	epoch		# Julian epoch
int	time_zone	# Time zone
real	zd		# Zenith distance defining rising and setting
int	which		# Which phenomenom (RISE or SET)

int	year, month, day
double	ut, zepoch, ast_mst()
real	time, ha, hour_angle()

begin
	# Compute GMST at UT = 0 h
	call ast_epoch_to_date (epoch, year, month, day, ut)
	ut = 0.d0
	call ast_date_to_epoch (year, month, day, ut, zepoch)

	# Compute the hour angle of rise/set
	ha = hour_angle (zd, dec, latitude)
	if (ha < 0)	# No phenomenom
	    return (-1.)
	if (which == RISE)
	    ha = - ha

	# Return the appropriate time
	time = ra - (360. - longitude) / 15. + ha - ast_mst (zepoch, 0.d0) -
	       time_zone
	if (time > 24.)
	    time = mod (time, 24.)
	while (time < 0.)
	    time = time + 24.
	return (time)
end

# HOUR_ANGLE -- Returns the hour angle (in hours) for the specified
# declination, earth latitide, and zenith distance.  Returns -1 if
# no solution (object never reaches that hour angle).
# Formula from Astronomical Formulae for Calculators, 3rd edition,
# Jean Meeus, p. 47.

real procedure hour_angle (zd, dec, latitude)
real	zd		# Zenith distance (degrees from pole)
real	dec		# declination (degrees)
real	latitude	# Earth latitude (degrees)

real	ha, cosha

begin
	# Compute the hour angle
	cosha = (sin(DEGTORAD(90. - zd)) -
		 sin(DEGTORAD(latitude)) * sin(DEGTORAD(dec))) /
		(cos(DEGTORAD(latitude)) * cos(DEGTORAD(dec)))
	if (cosha >= 1. || cosha <= -1.)
	    return (-1.)
	ha = RADTODEG(acos(cosha)) / 15.
	return (abs(ha))
end

# AIRMASS -- Return the airmass for the specified parameters.
# Formula from Allen's Astrophysical Qualntities, 1973, pp. 125 and 133.

define	SCALE	750.	# Approximate atmospheric scale height

real procedure airmass (st, ra, dec, latitude)
real	st		# Sidereal time
real	ra		# Right ascension (hours)
real	dec		# Declination (degrees)
real	latitude	# Site latitude (degrees)

real	ha, coszd

begin
    ha = (st - ra) * 15.
    coszd = sin (DEGTORAD(latitude)) * sin (DEGTORAD(dec)) +
	    cos (DEGTORAD(latitude)) * cos (DEGTORAD(dec)) * cos (DEGTORAD(ha))
    return (sqrt ((SCALE * coszd) ** 2 + 2 * SCALE + 1) - SCALE * coszd)
end

# ZD_AIRMASS -- Returns the zenith distance for the given airmass.

real procedure zd_airmass (airmass)
real	airmass

real	coszd

begin
    coszd = (2 * SCALE + 1 - airmass ** 2) / (2 * SCALE * airmass)
    return (RADTODEG(acos(coszd)))
end
