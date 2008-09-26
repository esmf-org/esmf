# $Id: makefile,v 1.6 2008/09/26 05:53:54 cdeluca Exp $ 


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

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/FieldBundle/src

CLEANDIRS   =
CLEANFILES  = $(AUTOGEN) $(addprefix ../doc/, $(TEXFILES) )
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  = 

