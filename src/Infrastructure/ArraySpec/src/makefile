# $Id: makefile,v 1.4 2008/08/06 17:35:35 tjcnrl Exp $

# leave alone.
ALL: build_here 

# if you have C files in this directory, list them all on the line below.
SOURCEC	  =  
# if you have F90 files in this directory, list them all on the line below.
SOURCEF	  =  ESMF_ArraySpec.F90
# if you have .h include files in this dir, list them all on the line below.
STOREH	  = 

# leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = ArraySpecobj
LIBBASE	  = libesmf


# the following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/ArraySpec/src

include $(ESMF_DIR)/makefile

# leave the following line blank.
DIRS	  = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

