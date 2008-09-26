# $Id: makefile,v 1.2 2008/09/26 21:11:39 cdeluca Exp $

ALL: onedoc 

LOCDIR	  = src/demos/coupled_flow/doc

DVIFILES  = 	CoupledFlow_usrdoc.dvi

PDFFILES  =     ${ESMC_DOCDIR}/CoupledFlow_usrdoc.pdf

HTMLFILES = 	${ESMC_DOCDIR}/CoupledFlow_usrdoc 

#
# Tex Files to make
#
TEXFILES_TO_MAKE  = $(addsuffix _ccapi.tex, $(basename $(notdir $(wildcard ../src/*.C))))
TEXFILES_TO_MAKE += $(addsuffix _chapi.tex, $(basename $(notdir $(wildcard ../include/*.h))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.F))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.F90))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.f))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.f90))))

TEXFILES_TO_MAKE += $(addsuffix _ccapi.tex, $(basename $(notdir $(wildcard ../examples/*.C))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../examples/*.F))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../examples/*.F90))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../examples/*.f))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../examples/*.f90))))


#
# Files that a doc depends on.  Do not list the the ctex file !  
# The ctex file is listed in the rule found in the build/common file.
# These lists almost certainly will not be an exhastive list of 
# all of the dependent files, but even a partial listing  will be helpful.
#
USRDOC_DEP_FILES = $(TEXFILES_TO_MAKE) flow_intro.tex

export TEXINPUTS := .

include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = CoupledFlow_usrdoc $(HTMLFILES)
CLEANFILES  = $(TEXFILES_TO_MAKE) $(CLEAN_TEXFILES) \
               CoupledFlow_usrdoc.tex $(PDFFILES)
CLOBBERDIRS =



%_usrdoc.dvi : %_usrdoc.ctex $(USRDOC_DEP_FILES)
	$(DO_LATEX) $* usr


$(ESMC_DOCDIR)/%_usrdoc: %_usrdoc.ctex $(USRDOC_DEP_FILES)
	${DO_L2H} $* usr
	mv -f $(@F) $(ESMC_DOCDIR)


