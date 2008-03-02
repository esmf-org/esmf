# $Id: makefile,v 1.1.10.2 2008/03/02 05:03:37 theurich Exp $

# leave alone.
ALL: build_here

# if you have C files in this directory, list them all on the line below.
SOURCEC	  = ESMCI_ArraySpec.C
# if you have F90 files in this directory, list them all on the line below.
SOURCEF	  =  ESMF_ArraySpec_C.F90
#SOURCEF	  = 
# if you have .h include files in this dir, list them all on the line below.
SOURCEH	  = 
# include files which go into the public src dir
STOREH	  =  ESMCI_ArraySpec.h

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
