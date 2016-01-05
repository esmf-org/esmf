# $Id$ 

all: onedoc

ALL:  lib

LOCDIR	  = src/Superstructure/AppDriver/doc

DVIFILES  =     AppDriver_refdoc.dvi 
PDFFILES  =     $(ESMC_DOCDIR)/AppDriver_refdoc.pdf 
HTMLFILES =     $(ESMC_DOCDIR)/AppDriver_refdoc 

#
# Tex Files to make
#
TEXFILES_TO_MAKE = $(addsuffix SeqPrF_fapi.tex, $(basename $(notdir $(wildcard ../seq_pairwise_fdriver_spmd/*.F90))))

##TEXFILES_TO_MAKE += $(addsuffix SeqPrF_ccapi.tex, $(basename $(notdir $(wildcard ../seq_pairwise_cdriver_spmd/*.C))))
##TEXFILES_TO_MAKE += $(addsuffix SeqPrF_chapi.tex, $(basename $(notdir $(wildcard ../seq_pairwise_cdriver_spmd/*.h))))
##TEXFILES_TO_MAKE += $(addsuffix SeqPrF_fapi.tex, $(basename $(notdir $(wildcard ../seq_pairwise_fdriver_spmd/*.F))))
##TEXFILES_TO_MAKE += $(addsuffix SeqPrF_fapi.tex, $(basename $(notdir $(wildcard ../seq_pairwise_fdriver_spmd/*.f))))
##TEXFILES_TO_MAKE += $(addsuffix SeqPrF_fapi.tex, $(basename $(notdir $(wildcard ../seq_pairwise_fdriver_spmd/*.f90))))

#
# Files that a doc depends on.  Do not list the the ctex file !  
# The ctex file is listed in the rule found in the build/common file.
# These lists almost certainly will not be an exhastive list of 
# all of the dependent files, but even a partial listing  will be helpful.
#
REFDOC_DEP_FILES = $(TEXFILES_TO_MAKE) AppDriver_creqmethodsintro.tex AppDriver_desc.tex AppDriver_design.tex AppDriver_options.tex AppDriver_reqmethodsintro.tex AppDriver_reqmethods.tex AppDriver_rest.tex AppDriver_usage.tex
include $(ESMF_DIR)/makefile


DIRS	  = 
CLEANDIRS   = AppDriver_refdoc $(HTMLFILES)
CLEANFILES  = $(TEXFILES_TO_MAKE) $(CLEAN_TEXFILES) \
              AppDriver_refdoc.tex $(PDFFILES) 
CLOBBERDIRS =








