# $Id: makefile,v 1.9 2012/10/24 21:30:53 theurich Exp $

All: onedoc

LOCDIR    = src/addon/NUOPC/doc

DVIFILES  = NUOPC_refdoc.dvi
PDFFILES  = NUOPC_refdoc.pdf
HTMLFILES = NUOPC_refdoc

#
# Tex Files to make
#
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.F))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.F90))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.f))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.f90))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.cppF90))))

TEXFILES_TO_MAKE += $(addsuffix _ccapi.tex, $(basename $(notdir $(wildcard ../examples/*.C))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../examples/*.F))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../examples/*.F90))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../examples/*.f))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../examples/*.f90))))

#
# Files that a doc depends on.  Do not list the the ctex file !  
# The ctex file is listed in the rule found in the build/common file.
# These lists almost certainly will not be an exhaustive list of 
# all of the dependent files, but even a partial listing  will be helpful.
#
REFDOC_DEP_FILES = $(TEXFILES_TO_MAKE) NUOPC_desc.tex NUOPC_implnotes.tex NUOPC_GenericComponents.tex NUOPC_FieldDictionary.tex  NUOPC_ModelComponent_metadata.tex NUOPC_ConnectorComponent_metadata.tex NUOPC_Field_metadata.tex

REFDOC_DEP_FILES += NUOPC_Driver_manual_fapi.tex NUOPC_DriverAtmOcn_manual_fapi.tex NUOPC_ModelBase_manual_fapi.tex NUOPC_Model_manual_fapi.tex NUOPC_Mediator_manual_fapi.tex NUOPC_Connector_manual_fapi.tex

include $(ESMF_DIR)/makefile

DIRS        = 
CLEANDIRS   = NUOPC_refdoc $(HTMLFILES)
CLEANFILES  = $(TEXFILES_TO_MAKE) $(CLEAN_TEXFILES) \
              NUOPC_refdoc.tex $(PDFFILES)
CLOBBERDIRS =

