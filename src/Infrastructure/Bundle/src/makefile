# $Id: makefile,v 1.9 2004/08/03 19:33:43 nscollins Exp $ 


ALL: build_here 

AUTOGEN   = ESMF_BundleGet.F90

SOURCEC	  = 
SOURCEF	  = ESMF_Bundle.F90  $(AUTOGEN)
SOURCEH	  = 
CPPFILES  = $(AUTOGEN)


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

