# $Id: makefile,v 1.1 2003/09/17 21:26:29 cdeluca Exp $ 
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
LOCDIR	  = src/Infrastructure/Bundle/doc

DVIFILES  = 	Bundle_reqdoc.dvi \
		Bundle_desdoc.dvi \
		Bundle_refdoc.dvi

PDFFILES  =     ${ESMC_DOCDIR}/Bundle_reqdoc.pdf \
		${ESMC_DOCDIR}/Bundle_desdoc.pdf \
		${ESMC_DOCDIR}/Bundle_refdoc.pdf

HTMLFILES = 	${ESMC_DOCDIR}/Bundle_reqdoc \
		${ESMC_DOCDIR}/Bundle_desdoc \
		${ESMC_DOCDIR}/Bundle_refdoc

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
REQDOC_DEP_FILES = Bundle_auth.tex Bundle_background.tex Bundle_terms.tex \
      Bundle_abbr.tex Bundle_req.tex Bundle_desdoc.ctex


DESDOC_DEP_FILES = $(TEXFILES_TO_MAKE) Bundle_ccex.tex Bundle_ccparam.tex \
      Bundle_def.tex Bundle_desc.tex Bundle_design.tex \
      Bundle_fex.tex Bundle_fparam.tex Bundle_rest.tex \
      Bundle_abbr.tex Bundle_alg.tex Bundle_background.tex \
      Bundle_ccex.tex Bundle_ccparam.tex Bundle_def.tex \
      Bundle_desc.tex Bundle_design.tex \
      Bundle_fex.tex Bundle_fparam.tex Bundle_glos.tex Bundle_prog.tex \
      Bundle_obj.tex Bundle_req.tex Bundle_reqdoc.ctex Bundle_rest.tex \
      Bundle_syn.tex Bundle_terms.tex fieldclass.eps bundleclass.eps 

REFDOC_DEP_FILES = $(TEXFILES_TO_MAKE)


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = Bundle_desdoc Bundle_reqdoc Bundle_refdoc $(HTMLFILES)
CLEANFILES  = $(TEXFILES_TO_MAKE) *.aux *.bbl *.blg *.log *.toc *.dvi \
              *.ORIG Bundle_desdoc.tex Bundle_reqdoc.tex Bundle_refdoc.tex \
              $(PDFFILES)
CLOBBERDIRS =


