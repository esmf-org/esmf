# $Id: makefile,v 1.3 2008/08/06 17:35:37 tjcnrl Exp $

# Leave alone.
ALL: build_here

# Don't set these unless absolutely necessary - they've got to be
# architecture and compiler independent and should be inherited
# from the upper level makefile if possible.
CFLAGS    = ${MPI_INCLUDE}
FFLAGS    =

# If you have C files in this directory, list them all on the line below.
SOURCEC	  =  ESMC_InternArrayComm.C 
# If you have F90 files in this directory, list them all on the line below.
SOURCEF	  =  
# If you have .h include files in this dir, list them all on the line below.
STOREH	  = ESMC_InternArrayComm.h

# Leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = InternArrayobj
LIBBASE	  = libesmf



# The following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/InternArrayComm/src

include $(ESMF_DIR)/makefile

# Leave the following line blank.
DIRS	  = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

