# $Id: makefile,v 1.6 2004/08/03 19:33:52 nscollins Exp $

# Leave alone.
ALL: build_here 

# Don't set these unless absolutely necessary - they've got to be
# architecture and compiler independent and should be inherited
# from the upper level makefile if possible.
CFLAGS    += ${MPI_INCLUDE}
FFLAGS    =

AUTOGEN   = ESMF_StateGet.F90

SOURCEC	  = 
SOURCEF	  = ESMF_State.F90  $(AUTOGEN)
SOURCEH	  = 
STOREH    = ESMC_State.h
CPPFILES  = $(AUTOGEN)
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
CLEANFILES  = $(AUTOGEN) $(addprefix ../doc/, $(TEXFILES) )
CLOBBERDIRS =

ESMF_StateGet.o: ESMF_StateGet.cpp ESMF_StateMacros.h ESMF_State.o

