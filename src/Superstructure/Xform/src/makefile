# $Id: makefile,v 1.2 2003/10/23 20:23:05 jwolfe Exp $

# Leave alone.
ALL: lib 

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

