# $Id: makefile,v 1.3 2004/02/11 23:22:08 nscollins Exp $

# Leave alone.
ALL: build_here 

# Don't set these unless absolutely necessary - they've got to be
# architecture and compiler independent and should be inherited
# from the upper level makefile if possible.
CFLAGS    = ${MPI_INCLUDE}
FFLAGS    =

# If you have C files in this directory, list them all on the line below.
SOURCEC	  =
# If you have F90 files in this directory, list them all on the line below.
SOURCEF	  = ESMF_Xform.F90
# If you have .h include files in this dir, list them all on the line below.
SOURCEH	  = 
# If you have public .h files in this dir, list them here
STOREH    = 

# Leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = Xformobj
LIBBASE	  = libesmf



# The following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Superstructure/Xform/src


include $(ESMF_DIR)/makefile

# leave the following line blank.
DIRS	  = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

