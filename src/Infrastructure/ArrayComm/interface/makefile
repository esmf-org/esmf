# $Id: makefile,v 1.1 2003/10/07 22:29:46 nscollins Exp $

# leave alone.
ALL: lib 

# don't set these unless absolutely necessary - they've got to be
#  architecture and compiler independent and should be inherited
#  from the upper level makefile if possible.
CFLAGS    = ${MPI_INCLUDE}
FFLAGS    =

# if you have C files in this directory, list them all on the line below.
SOURCEC	  = ESMC_ArrayComm_F.C
# if you have F90 files in this directory, list them all on the line below.
SOURCEF	  =  ESMF_ArrayComm.F90
# if you have .h include files in this dir, list them all on the line below.
SOURCEH	  =

# leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

# is MANSEC something about documentation?
MANSEC	  = Arrayobj
LIBBASE	  = libesmf



# the following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/ArrayComm/interface


include $(ESMF_DIR)/makefile

# leave the following line blank.
DIRS	  = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

