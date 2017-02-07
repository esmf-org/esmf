# $Id$

All: onedoc

LOCDIR    = src/addon/NUOPC/doc

DVIFILES  = NUOPC_refdoc.dvi NUOPC_howtodoc.dvi
PDFFILES  = NUOPC_refdoc.pdf NUOPC_howtodoc.pdf
HTMLFILES = NUOPC_refdoc NUOPC_howtodoc

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
REFDOC_DEP_FILES = $(TEXFILES_TO_MAKE) NUOPC_desc.tex NUOPC_implnotes.tex NUOPC_GenericComponents.tex NUOPC_FieldDictionary.tex NUOPC_PhaseMaps.tex NUOPC_IPD.tex NUOPC_FieldPairing.tex NUOPC_Namespaces.tex NUOPC_ConnectionOptions.tex NUOPC_DataDepInit.tex NUOPC_TransferGeom.tex NUOPC_Timekeeping.tex NUOPC_Metadata.tex NUOPC_DriverComponent_metadata.tex NUOPC_ModelComponent_metadata.tex NUOPC_MediatorComponent_metadata.tex NUOPC_ConnectorComponent_metadata.tex NUOPC_State_metadata.tex NUOPC_Field_metadata.tex NUOPC_StandardComponentDep.tex NUOPC_Compliance.tex NUOPC_ComplianceChecker.tex NUOPC_ComponentExplorer.tex NUOPC_FieldMirror.tex

REFDOC_DEP_FILES += NUOPC_Driver_manual_fapi.tex NUOPC_ModelBase_manual_fapi.tex NUOPC_Model_manual_fapi.tex NUOPC_Mediator_manual_fapi.tex NUOPC_Connector_manual_fapi.tex

HOWTO_DEP_FILES = $(REFDOC_DEP_FILES) NUOPC_howto_overview.tex NUOPC_howto_bigidea.tex NUOPC_howto_approach.tex NUOPC_howto_example.tex ESMF_NUOPCBasicModelEx_fapi.tex ESMF_NUOPCAtmModelEx_fapi.tex

include $(ESMF_DIR)/makefile

DIRS        = 
CLEANDIRS   = NUOPC_refdoc NUOPC_howtodoc $(HTMLFILES)
CLEANFILES  = $(TEXFILES_TO_MAKE) $(CLEAN_TEXFILES) \
              NUOPC_refdoc.tex NUOPC_howtodoc.tex $(PDFFILES)
CLOBBERDIRS =


## Rules for the howto doc, not covered by build/common.mk
NUOPC_howtodoc.dvi: NUOPC_howtodoc.ctex $(HOWTODOC_DEP_FILES)
	export TEXINPUTS=$(TEXINPUTS_VALUE) ;\
	${DO_LATEX} NUOPC howto

NUOPC_howtodoc.pdf: NUOPC_howtodoc.dvi
	dvipdf NUOPC_howtodoc.dvi

NUOPC_howtodoc: NUOPC_howtodoc.ctex
	-rm -rf NUOPC_howtodoc
	if [ $(TEXINPUTS_VALUE)foo != foo ] ; then \
	  echo '$$TEXINPUTS = $(TEXINPUTS_VALUE)' > .latex2html-init ;\
	fi;
	${DO_L2H} NUOPC howto
	rm -f .latex2html-init

