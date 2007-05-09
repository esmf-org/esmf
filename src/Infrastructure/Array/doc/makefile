# $Id: makefile,v 1.10 2007/05/09 04:43:02 theurich Exp $

All: onedoc

LOCDIR = src/Infrastructure/Array/doc

DVIFILES = Array_refdoc.dvi
PDFFILES  = $(ESMC_DOCDIR)/Array_refdoc.pdf
HTMLFILES = $(ESMC_DOCDIR)/Array_refdoc

#
# Tex Files to make
#
TEXFILES_TO_MAKE  = $(addsuffix _ccapi.tex, $(basename $(notdir $(wildcard ../src/*.C))))
TEXFILES_TO_MAKE += $(addsuffix _chapi.tex, $(basename $(notdir $(wildcard ../include/*.h))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../interface/*.F))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../interface/*.F90))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../interface/*.f))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../interface/*.f90))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../interface/*.cppF90))))

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
REFDOC_DEP_FILES = $(TEXFILES_TO_MAKE) Array_desc.tex Array_usage.tex Array_rest.tex Array_implnotes.tex

include $(ESMF_DIR)/makefile

DIRS = 
CLEANDIRS   = Array_refdoc $(HTMLFILES)
CLEANFILES  = $(TEXFILES_TO_MAKE) $(CLEAN_TEXFILES) \
              Array_refdoc.tex $(PDFFILES)
CLOBBERDIRS =

