# $Id: makefile,v 1.2 2003/03/05 20:58:57 nscollins Exp $

# leave alone.
ALL: lib 

# don't set these unless absolutely necessary - they've got to be
#  architecture and compiler independent and should be inherited
#  from the upper level makefile if possible.
CFLAGS    =
FFLAGS    =

# if you have C files in this directory, list them all on the line below.
SOURCEC	  = ESMC_XPacket_F.C ESMC_Route_F.C
# if you have F90 files in this directory, list them all on the line below.
SOURCEF	  = ESMF_XPacket.F90 ESMF_Route.F90
# if you have .h include files in this dir, list them all on the line below.
SOURCEH	  = 
# list all .h files which should be copied to common include dir
STOREH	  = 

# leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix .tex, $(basename $(SOURCEF)))

# is MANSEC something about documentation?
MANSEC	  = Routeobj
LIBBASE	  = libesmf

# leave the following line blank.
DIRS	  = 

# the following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/Route/interface

# leave these alone 
CLEANFILES = $(LIBNAME) $(addprefix doc/, $(addsuffix .tex, $(basename $(SOURCEF))))

include ${ESMF_DIR}/makefile


