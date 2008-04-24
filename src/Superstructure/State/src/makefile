# $Id: makefile,v 1.13.2.2 2008/04/24 05:50:12 theurich Exp $

# Leave alone.
ALL: build_here 

# Don't set these unless absolutely necessary - they've got to be
# architecture and compiler independent and should be inherited
# from the upper level makefile if possible.
CFLAGS    += ${MPI_INCLUDE}
FFLAGS    =

AUTOGEN   = 
# ESMF_StateGet.F90
#TODO:FIELDINTEGRATION Restore StateGet

SOURCEC	  = 
SOURCEF	  = ESMF_StateTypes.F90 ESMF_State.F90 ESMF_StateReconcile.F90 $(AUTOGEN)
SOURCEH	  = 
STOREH    = ESMC_State.h
TEXFILES  = $(addsuffix _fapi.tex, $(basename $(AUTOGEN)))

# Leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = Stateobj
LIBBASE	  = libesmf


# The following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Superstructure/State/src

include $(ESMF_DIR)/makefile

# leave the following line blank.
DIRS	  = 

CLEANDIRS   =
CLEANFILES  = $(AUTOGEN) $(addprefix ../doc/, $(TEXFILES) ) ESMF_LogFile
CLOBBERDIRS =

ESMF_StateGet.o: ESMF_StateGet.cppF90 ESMF_State.o

ESMF_State.o: ESMF_StateTypes.o

ESMF_StateReconcile.o: ESMF_StateTypes.o ESMF_State.o

