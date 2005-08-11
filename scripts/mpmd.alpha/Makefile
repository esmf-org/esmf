#
#  2003 Gerhard Theurich (gjt) NASA/NCCS and SGI Professional Services
#  <theurich@zot.gsfc.nasa.gov> or <gtheurich@sgi.com>
#

ifndef INSTALLDIR
export INSTALLDIR = ./
endif

all: mpimpmdrun pprun pprunlaunch

mpimpmdrun: mpimpmdrun.c
	cc -o mpimpmdrun mpimpmdrun.c -DINSTALLDIR=\"$(INSTALLDIR)\"
pprun: pprun.c
	cc -o pprun pprun.c -DINSTALLDIR=\"$(INSTALLDIR)\"
pprunlaunch: pprunlaunch.c
	cc -o pprunlaunch pprunlaunch.c -DINSTALLDIR=\"$(INSTALLDIR)\"

.phony clean:
	rm -f *.o mpimpmdrun pprun pprunlaunch
