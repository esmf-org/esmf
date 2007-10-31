# $Id: makefile,v 1.14 2007/10/31 16:58:51 cdeluca Exp $ 


ALL: build_here 

AUTOGEN   = 
# TODO:FIELDINTEGRATION Restore BundleGet
# ESMF_BundleGet.F90 

SOURCEC	  = 
SOURCEF	  = ESMF_Bundle.F90 $(AUTOGEN)
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

ESMF_BundleGet.o:   ESMF_Bundle.o ESMF_BundleGet.cppF90 ESMF_BundleGetMacros.h

