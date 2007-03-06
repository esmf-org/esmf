# $Id: makefile,v 1.10 2007/03/06 00:10:58 theurich Exp $ 


ALL: build_here 

AUTOGEN   = ESMF_BundleGet.F90

SOURCEC	  = 
SOURCEF	  = ESMF_Bundle.F90  $(AUTOGEN)
SOURCEH	  = 


OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix _fapi.tex, $(basename $(AUTOGEN)))

MANSEC	  = Bundleobj
LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Bundle/src


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   =
CLEANFILES  = $(AUTOGEN) $(addprefix ../doc/, $(TEXFILES) )
CLOBBERDIRS =

ESMF_BundleGet.o:   ESMF_Bundle.o ESMF_BundleGet.cpp ESMF_BundleGetMacros.h

