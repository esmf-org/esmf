# $Id: makefile,v 1.8 2005/03/01 21:05:49 nscollins Exp $ 

all: alldoc

ALL:  lib
CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  = 
SOURCEH	  = 
OBJSC     = 
OBJSF     = 
TEXFILES  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Superstructure/AppDriver/doc

DVIFILES  =     AppDriver_refdoc.dvi 
#               AppDriver_desdoc.dvi \
#		AppDriver_reqdoc.dvi \


PDFFILES  =     $(ESMC_DOCDIR)/AppDriver_refdoc.pdf 
#               $(ESMC_DOCDIR)/AppDriver_desdoc.pdf \
#		$(ESMC_DOCDIR)/AppDriver_reqdoc.pdf \

HTMLFILES =     $(ESMC_DOCDIR)/AppDriver_refdoc 
#               $(ESMC_DOCDIR)/AppDriver_desdoc \
#		$(ESMC_DOCDIR)/AppDriver_reqdoc \


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
REQDOC_DEP_FILES = 
DESDOC_DEP_FILES = $(TEXFILES_TO_MAKE) 
REFDOC_DEP_FILES = $(TEXFILES_TO_MAKE)

include $(ESMF_DIR)/makefile


DIRS	  = 

CLEANDIRS   = AppDriver_desdoc AppDriver_refdoc $(HTMLFILES)
CLEANFILES  = $(TEXFILES_TO_MAKE) $(CLEAN_TEXFILES) \
              AppDriver_desdoc.tex AppDriver_refdoc.tex $(PDFFILES) 
CLOBBERDIRS =








