# $Id: makefile,v 1.6 2003/05/29 20:59:08 flanigan Exp $ 
ALL: alldoc 


CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  = 
SOURCEH	  = 
OBJSC     = 
OBJSF     = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Infrastructure/Field/doc

DVIFILES  = 	Field_reqdoc.dvi \
		Field_desdoc.dvi \
		Field_refdoc.dvi

PDFFILES  =     ${ESMC_DOCDIR}/Field_reqdoc.pdf \
		${ESMC_DOCDIR}/Field_desdoc.pdf \
		${ESMC_DOCDIR}/Field_refdoc.pdf

HTMLFILES = 	${ESMC_DOCDIR}/Field_reqdoc \
		${ESMC_DOCDIR}/Field_desdoc \
		${ESMC_DOCDIR}/Field_refdoc

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
REQDOC_DEP_FILES = Field_auth.tex Field_background.tex Field_terms.tex \
      Field_abbr.tex Field_req.tex Field_desdoc.ctex


DESDOC_DEP_FILES = $(TEXFILES_TO_MAKE) Bundle_ccex.tex Bundle_ccparam.tex \
      Bundle_def.tex Bundle_desc.tex Bundle_design.tex \
      Bundle_fex.tex Bundle_fparam.tex Bundle_rest.tex \
      Field_abbr.tex Field_alg.tex Field_background.tex \
      Field_ccex.tex Field_ccparam.tex Field_def.tex \
      Field_desc.tex Field_design.tex \
      Field_fex.tex Field_fparam.tex Field_glos.tex Field_prog.tex \
      Field_obj.tex Field_req.tex Field_reqdoc.ctex Field_rest.tex \
      Field_syn.tex Field_terms.tex fieldclass.eps bundleclass.eps 

REFDOC_DEP_FILES = $(TEXFILES_TO_MAKE)


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = Field_desdoc Field_reqdoc Field_refdoc $(HTMLFILES)
CLEANFILES  = $(TEXFILES_TO_MAKE) *.aux *.bbl *.blg *.log *.toc *.dvi \
              *.ORIG Field_desdoc.tex Field_reqdoc.tex Field_refdoc.tex \
              $(PDFFILES)
CLOBBERDIRS =


