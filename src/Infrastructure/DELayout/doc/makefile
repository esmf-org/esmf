# $Id: makefile,v 1.4 2004/06/09 13:31:33 theurich Exp $

All: alldoc

LOCDIR = src/Infrastructure/DELayout/doc

DVIFILES = DELayout_refdoc.dvi

PDFFILES  = $(ESMC_DOCDIR)/DELayout_refdoc.pdf

HTMLFILES = $(ESMC_DOCDIR)/DELayout_refdoc

#
# Tex Files to make
#
TEXFILES_TO_MAKE  = $(addsuffix _ccapi.tex, $(basename $(notdir $(wildcard ../src/*.C))))
TEXFILES_TO_MAKE += $(addsuffix _chapi.tex, $(basename $(notdir $(wildcard ../include/*.h))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../interface/*.F))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../interface/*.F90))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../interface/*.f))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../interface/*.f90))))

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
REQDOC_DEP_FILES = 
DESDOC_DEP_FILES = $(TEXFILES_TO_MAKE) 
REFDOC_DEP_FILES = $(TEXFILES_TO_MAKE) DELayout_desc.tex DELayout_usage.tex DELayout_rest.tex DELayout_implnotes.tex


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = DELayout_refdoc $(HTMLFILES)
CLEANFILES  = $(TEXFILES_TO_MAKE) *.aux *.bbl *.blg *.log *.toc *.dvi *.ORIG \
              DELayout_refdoc.tex $(PDFFILES)
CLOBBERDIRS =
