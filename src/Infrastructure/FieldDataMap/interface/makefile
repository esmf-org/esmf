# $Id: makefile,v 1.2 2004/08/03 19:33:48 nscollins Exp $

# leave alone.
ALL: build_here 

# don't set these unless absolutely necessary - they've got to be
#  architecture and compiler independent and should be inherited
#  from the upper level makefile if possible.
CFLAGS    =
FFLAGS    =

# if you have C files in this directory, list them all on the line below.
SOURCEC	  = ESMC_FieldDataMap.C
# if you have F90 files in this directory, list them all on the line below.
SOURCEF	  = 
# if you have .h include files in this dir, list them all on the line below.
SOURCEH	  = 
# list all .h files which should be copied to common include dir
STOREH	  = 

# leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

# is MANSEC something about documentation?
MANSEC	  = FieldDataMapobj
LIBBASE	  = libesmf

# the following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/FieldDataMap/interface

# leave these alone 
CLEANFILES = 

include ${ESMF_DIR}/makefile

# leave the following line blank.
DIRS	  = 





