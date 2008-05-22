# $Id: makefile,v 1.4 2008/05/22 18:22:44 feiliu Exp $ 


ALL: build_here 

AUTOGEN   = 
# TODO:FIELDINTEGRATION Restore FieldBundleGet
# ESMF_FieldBundleGet.F90 

SOURCEC	  = 
SOURCEF	  = ESMF_FieldBundle.F90 ESMF_FieldBundleSMM.F90 ESMF_FieldBundleRedist.F90 $(AUTOGEN)
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

