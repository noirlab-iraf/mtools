# PAGER -- Page a specified number of output lines.

# Based on etc$pagefiles.x

include <chars.h>
include <fset.h>

define  UKEYS           "ukey"          # CL parameter for keyboard input
define	SZ_KEYSTR	1
define	PROMPT		"catalog entry information"

define  HELPTXT "[q=quit,d=dn,u=up,f|sp=fpg,b=bpg,j|cr=dnln,k=upln,.=bof,G|g=tof]"

define  HELP            '?'             # print helptxt
define  QUIT            'q'             # return to CL
define  FWD_SCREEN      'f'             # forward one full screen
define  BACK_SCREEN	'b'             # back one full screen
define  SCROLL_DOWN     'd'             # forward half a screen
define  SCROLL_UP       'u'             # back half a screen
define  PREV_LINE       'k'             # back one line
define  NEXT_LINE       'j'             # forward one line
define  TO_BOF          '.'             # to beginning of file
define  TO_EOF          'G'             # to end of file
define  TO_EOF_ALT      'g'             # to end of file
define  REDRAW          '\014'          # redraw screen

procedure pager (tty, screensize, nlines, line1, line2, clear_screen)
pointer	tty		# tty file descriptor
int	screensize	# number of lines on the screen (less the status line)
int	nlines		# number of lines to print
int	line1		# on putput, first line to print
int	line2		# on input, the last line printed --
			# on output, the last line to print
int	clear_screen	# clear the screen?

int	cmd, pg_getcmd2(), newkey, currentline

begin
    currentline = line2
    cmd = pg_getcmd2 (tty, PROMPT, nlines, currentline)
    newkey = NO
    repeat {
	switch (cmd) {
	case HELP:
	    cmd = pg_getcmd2 (tty, HELPTXT, 0, 0)
	    clear_screen = NO
	    newkey = YES
	case QUIT:
	    line1 = 0
	    line2 = 0
	    clear_screen = NO
	case FWD_SCREEN, BLANK:
	    if (currentline == nlines) {
	    	cmd = pg_getcmd2 (tty, "end of record", 0, 0)
		newkey = YES
	    } else {
	    	line1 = currentline+1
	    	line2 = min (nlines, currentline+screensize-1)
		clear_screen = NO
	    }
	case BACK_SCREEN:
	    line1 = max (1, currentline-screensize+1-screensize+1)
	    line2 = min (nlines, line1+screensize-1)
	    clear_screen = YES
	case SCROLL_DOWN:
	    line1 = currentline+1
	    line2 = min (nlines, currentline+screensize/2)
	    clear_screen = NO
	case SCROLL_UP:
	    line1 = max (1, currentline-screensize+1-screensize/2)
	    line2 = min (nlines, line1+screensize-1)
	    clear_screen = YES
	case PREV_LINE:
	    line1 = max (1, currentline-screensize+1-1)
	    line2 = min (nlines, line1+screensize-1)
	    clear_screen = YES
	case NEXT_LINE, RETURN:
	    line1 = currentline+1
	    line2 = min (nlines, currentline+1)
	    clear_screen = NO
	case TO_BOF:
	    line1 = 1
	    line2 = min (screensize, nlines)
	    clear_screen = YES
	case TO_EOF, TO_EOF_ALT:
	    line2 = nlines
	    line1 = max (1, line2-screensize+1)
	    clear_screen = YES
	case REDRAW:
	    line2 = currentline
	    line1 = max (1, line2-screensize+1)
	    clear_screen = YES
	default:
	    call eprintf ("\07")
	    call flush (STDERR)
	    cmd = pg_getcmd2 (tty, PROMPT, nlines, currentline)
	    clear_screen = NO
	    newkey = YES
	}
	if (newkey == NO)
	    break
	newkey = NO
    }
end

# PG_GETCMD2 -- Query the user for a single character command keystroke.
# A prompt naming the current file and our position in it is printed,
# we read the single character command keystroke in raw mode, and then
# the prompt line is cleared and we return.

# This is a simplified version of pg_getcmd in etc$pagefiles.
# The primary difference is that multiple files aren't supported, and the
# percentage of the file read is based on lines rather than characters.

int procedure pg_getcmd2 (tty, fname, nlines, lineno)

pointer tty                     # tty descriptor
char    fname[ARB]              # prefix string
int	nlines			# number of lines to print
int     lineno                  # current line number

int     key
char    keystr[SZ_KEYSTR]
#common  /pgucom/ key, keystr
int     clgkey(), fstati()

begin
        # If the standard output is redirected, skip the query and just go on
        # to the next page.

        if (fstati (STDOUT, F_REDIR) == YES)
            return (FWD_SCREEN)

        # Ensure synchronization with the standard output.
        call flush (STDOUT)

        # Print query in standout mode, preceded by %done info.
        call ttyso (STDERR, tty, YES)
        call eprintf ("%s")
            call pargstr (fname)
        if (nlines > 0) {
            if (lineno == nlines)
                call eprintf ("-(EOF)")
            else {
                call eprintf ("-(%02d%%)")
                    call pargi (max (0, min (100,
                        nint (real(lineno) / real(nlines) * 100.0))))
            }
        }
        if (lineno > 0) {
            call eprintf ("-line %d")
                call pargi (lineno)
        }
        call ttyso (STDERR, tty, NO)
        call flush (STDERR)

        call fseti (STDIN, F_SETREDRAW, REDRAW)

        # Read the user's response, normally a single keystroke.
        if (clgkey (UKEYS, key, keystr, SZ_KEYSTR) == EOF)
            key = INTCHAR

        call fseti (STDIN, F_SETREDRAW, 0)

        if (key == INTCHAR)
            key = QUIT

        # Erase the prompt and return.
        call eprintf ("\r")
        call ttyclearln (STDERR, tty)
        call flush (STDERR)

        return (key)
end
