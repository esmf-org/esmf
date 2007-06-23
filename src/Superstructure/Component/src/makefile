# $Id: makefile,v 1.29 2007/06/23 07:00:51 cdeluca Exp $

# leave alone.
ALL: build_here 

# Don't set these unless absolutely necessary - they've got to be
# architecture and compiler independent and should be inherited
# from the upper level makefile if possible.
CFLAGS    += ${MPI_INCLUDE}
FFLAGS    =

# If you have C files in this directory, list them all on the line below.
SOURCEC	  = ESMC_FTable.C

# If you have F90 files in this directory, list them all on the line below.
SOURCEF	  = ESMF_Comp.F90 ESMF_GridComp.F90 ESMF_CplComp.F90 

# If you have .h include files in this dir, list them all on the line below.
SOURCEH	  = 
# If you have public .h files in this dir, list them here
STOREH    = ESMC_Comp.h ESMC_FTable.h ESMC_GridComp.h ESMC_CplComp.h

# Leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

# Is MANSEC something about documentation?
MANSEC	  = Componentobj
LIBBASE	  = libesmf



# The following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Superstructure/Component/src


include $(ESMF_DIR)/makefile

# Leave the following line blank.
DIRS	  = 

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =

ESMF_CplComp.o:  ESMF_Comp.o

ESMF_GridComp.o: ESMF_Comp.o

