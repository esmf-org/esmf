# $Id: makefile,v 1.19 2008/09/02 22:35:25 theurich Exp $

# Leave alone.
ALL: build_here 

AUTOGEN   = 
# ESMF_StateGet.F90
#TODO:FIELDINTEGRATION Restore StateGet

SOURCEC	  = 
SOURCEF	  = ESMF_StateTypes.F90 ESMF_StateVa.F90 ESMF_State.F90 ESMF_StateSet.F90 ESMF_StateReconcile.F90 $(AUTOGEN)
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

