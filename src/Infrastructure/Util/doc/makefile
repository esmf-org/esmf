# $Id: makefile,v 1.8 2009/03/18 23:57:23 w6ws Exp $

ALL: onedoc

LOCDIR = src/Infrastructure/Util/doc

DVIFILES  = IOUtil_refdoc.dvi

PDFFILES  = $(ESMC_DOCDIR)/IOUtil_refdoc.pdf

HTMLFILES = $(ESMC_DOCDIR)/IOUtil_refdoc


#
# Tex Files to make
#
TEXFILES_TO_MAKE  = $(addsuffix _ccapi.tex, $(basename $(notdir $(wildcard ../src/*.C))))
TEXFILES_TO_MAKE += $(addsuffix _chapi.tex, $(basename $(notdir $(wildcard ../include/*.h))))
TEXFILES_TO_MAKE += $(addsuffix _chapi.tex, $(basename $(notdir $(wildcard ../include/*.inc))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.F))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.F90))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.f))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.f90))))

TEXFILES_TO_MAKE += $(addsuffix _ccapi.tex, $(basename $(notdir $(wildcard ../interface/*.C))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../interface/*.F))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../interface/*.F90))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../interface/*.f))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../interface/*.f90))))

TEXFILES_TO_MAKE += $(addsuffix _ccapi.tex, $(basename $(notdir $(wildcard ../examples/*.C))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../examples/*.F))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../examples/*.F90))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../examples/*.f))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../examples/*.f90))))
TEXFILES_TO_MAKE += ESMF_ErrReturnCodes.tex

# Files that a doc depends on.  Do not list the the ctex file !  
# The ctex file is listed in the rule found in the build/common file.
# These lists almost certainly will not be an exhaustive list of 
# all of the dependent files, but even a partial listing  will be helpfull.
#
REFDOC_DEP_FILES = $(TEXFILES_TO_MAKE) IOUtil_desc.tex

include $(ESMF_DIR)/makefile

# Filter out #if's, #endif's and #define's from error codes file,
#  which appears as an appendix in the ESMF reference manual.
ESMF_ErrReturnCodes.tex : ESMF_ErrReturnCodes_chapi.tex
	$(ESMF_RM) ESMF_ErrReturnCodes.tex
	$(ESMF_GREPV) 'if' ESMF_ErrReturnCodes_chapi.tex | $(ESMF_SED) 's/#define //' > ESMF_ErrReturnCodes.tex

DIRS = 

CLEANDIRS   = IOUtil_refdoc $(HTMLFILES)
CLEANFILES  = $(TEXFILES_TO_MAKE) $(CLEAN_TEXFILES) \
	      IOUtil_refdoc.tex $(PDFFILES)
CLOBBERDIRS =

