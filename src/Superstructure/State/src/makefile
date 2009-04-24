# $Id: makefile,v 1.21 2009/04/24 05:20:17 theurich Exp $

ALL: build_here 

AUTOGEN   = 
# ESMF_StateGet.F90
#TODO:FIELDINTEGRATION Restore StateGet

SOURCEC	  = 
SOURCEF	  = ESMF_StateTypes.F90 ESMF_StateVa.F90 ESMF_State.F90 ESMF_StateSet.F90 ESMF_StateReconcile.F90 ESMF_AttachMethods.F90 $(AUTOGEN)
SOURCEH	  = 
STOREH    = ESMC_State.h
TEXFILES  = $(addsuffix _fapi.tex, $(basename $(AUTOGEN)))

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Superstructure/State/src

CLEANDIRS   =
CLEANFILES  = $(AUTOGEN) $(addprefix ../doc/, $(TEXFILES) ) ESMF_LogFile
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  = 

