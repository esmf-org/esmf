# $Id: makefile,v 1.7 2008/09/26 04:16:46 cdeluca Exp $

# leave alone.
ALL: build_here 

# if you have C files in this directory, list them all on the line below.
SOURCEC	  = 
# if you have F90 files in this directory, list them all on the line below.
SOURCEF	  =  ESMF_Config.F90

# if you have .h include files in this dir, list them all on the line below.
SOURCEH	  = 
# list all .h files which should be copied to common include dir
STOREH	  = 

# leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix .tex, $(basename $(SOURCEF)))

LIBBASE	  = libesmf

# the following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/Config/src

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =

include ${ESMF_DIR}/makefile

# leave the following line blank.
DIRS	  = 

