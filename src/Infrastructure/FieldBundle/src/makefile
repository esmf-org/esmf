# $Id$ 


ALL: build_here 

AUTOGEN   = 

SOURCEC	  = 
#SOURCEF	  = ESMF_FieldBundle.F90 ESMF_FieldBundleSMM.F90 ESMF_FieldBundleRedist.F90 ESMF_FieldBundleHalo.F90 ESMF_FieldBundleRegrid.F90 $(AUTOGEN)
SOURCEF	  = ESMF_FieldBundle.F90 $(AUTOGEN)
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

