# $Id: makefile,v 1.10 2005/05/16 23:00:21 nscollins Exp $

# leave alone.
ALL: build_here 

# don't set these unless absolutely necessary - they've got to be
#  architecture and compiler independent and should be inherited
#  from the upper level makefile if possible.
CFLAGS    =
FFLAGS    =

# if you have C files in this directory, list them all on the line below.
SOURCEC	  = 
# if you have F90 files in this directory, list them all on the line below.
SOURCEF	  = ESMF_Init.F90 ESMF.F90 
# list all .h files which should be copied to common include dir
STOREH	  = 

# leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

# is MANSEC something about documentation?
MANSEC	  = Baseobj
LIBBASE	  = libesmf

# the following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Superstructure/ESMFMod/src

CLEANDIRS  =
CLEANFILES =
CLOBBERDIS =

include ${ESMF_DIR}/makefile

# leave the following line blank.
DIRS	  = 


# if you have class files which need to be compiled in a particular
# order, list the dependencies below, with no rules following.
#

ESMF.o:  ESMF_Init.o 

