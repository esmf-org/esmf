# $Id: makefile,v 1.1 2004/03/17 18:23:28 nscollins Exp $

# leave alone.
ALL: build_here

# if you have C files in this directory, list them all on the line below.
SOURCEC	  = ESMC_ArraySpec.C
# if you have F90 files in this directory, list them all on the line below.
SOURCEF	  =  
# if you have .h include files in this dir, list them all on the line below.
SOURCEH	  = 
# include files which go into the public src dir
STOREH	  =  ESMC_ArraySpec.h

# leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = ArraySpecobj
LIBBASE	  = libesmf



# the following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/ArraySpec/interface


include $(ESMF_DIR)/makefile

# leave the following line blank.
DIRS	  = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =
