# $Id: makefile,v 1.1.2.1 2008/04/02 20:27:39 cdeluca Exp $ 


ALL: build_here 

AUTOGEN   = 
# TODO:FIELDINTEGRATION Restore FieldBundleGet
# ESMF_FieldBundleGet.F90 

SOURCEC	  = 
SOURCEF	  = ESMF_FieldBundle.F90 $(AUTOGEN)
SOURCEH	  = 


OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix _fapi.tex, $(basename $(AUTOGEN)))

MANSEC	  = FieldBundleobj
LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/FieldBundle/src


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   =
CLEANFILES  = $(AUTOGEN) $(addprefix ../doc/, $(TEXFILES) )
CLOBBERDIRS =

ESMF_FieldBundleGet.o:   ESMF_FieldBundle.o ESMF_FieldBundleGet.cppF90 ESMF_FieldBundleGetMacros.h

