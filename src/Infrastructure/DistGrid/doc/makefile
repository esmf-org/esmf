# $Id: makefile,v 1.11 2005/03/01 21:00:50 nscollins Exp $ 
ALL: alldoc

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
LOCDIR	  = src/Infrastructure/DistGrid/doc

DVIFILES  = 	DistGrid_reqdoc.dvi \
		DistGrid_desdoc.dvi \
		DistGrid_refdoc.dvi

PDFFILES  =     ${ESMC_DOCDIR}/DistGrid_reqdoc.pdf \
		${ESMC_DOCDIR}/DistGrid_desdoc.pdf \
		${ESMC_DOCDIR}/DistGrid_refdoc.pdf

HTMLFILES = 	${ESMC_DOCDIR}/DistGrid_reqdoc \
		${ESMC_DOCDIR}/DistGrid_desdoc \
		${ESMC_DOCDIR}/DistGrid_refdoc

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
# These lists almost certainly will not be an exhaustive list of 
# all of the dependent files, but even a partial listing  will be helpful.
#
REQDOC_DEP_FILES = 
DESDOC_DEP_FILES = $(TEXFILES_TO_MAKE) 
REFDOC_DEP_FILES = $(TEXFILES_TO_MAKE)


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = DistGrid_reqdoc DistGrid_desdoc DistGrid_refdoc \
              $(HTMLFILES)
CLEANFILES  = $(TEXFILES_TO_MAKE) $(CLEAN_TEXFILES) \
              DistGrid_desdoc.tex DistGrid_reqdoc.tex DistGrid_refdoc.tex \
              $(PDFFILES)
CLOBBERDIRS =

