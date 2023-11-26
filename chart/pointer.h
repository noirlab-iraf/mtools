# SPP doesn't have dynamic pointer storage (i.e. Memp).  Pointers are simply
# integers in SPP.  Nonetheless, since this will change, pointers will be
# declared, but treated as integers.  Thus Memp -> Memi.

define	Memp	    	Memi
