# $Id: makefile,v 1.5 2008/08/06 17:35:35 tjcnrl Exp $ 


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

