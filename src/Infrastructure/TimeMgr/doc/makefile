# $Id: makefile,v 1.12 2003/04/03 23:15:03 flanigan Exp $
ALL: lib 

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  = 
SOURCEH	  = 
OBJSC     = 
OBJSF     = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Infrastructure/TimeMgr/doc

DVIFILES  = 	${ESMC_DOCDIR}/TimeMgr_reqdoc.dvi \
		${ESMC_DOCDIR}/TimeMgr_desdoc.dvi \
		${ESMC_DOCDIR}/TimeMgr_refdoc.dvi

PDFFILES  = $(addsuffix .pdf, $(basename $(DVIFILES)))

HTMLFILES = 	${ESMC_DOCDIR}/TimeMgr_reqdoc \
		${ESMC_DOCDIR}/TimeMgr_desdoc \
		${ESMC_DOCDIR}/TimeMgr_refdoc

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


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = TimeMgr_reqdoc TimeMgr_desdoc TimeMgr_refdoc 
CLEANFILES  = $(TEXFILES_TO_MAKE) *.aux *.bbl *.blg *.log *.toc *.dvi TimeMgr_reqdoc.tex TimeMgr_desdoc.tex TimeMgr_refdoc.tex 
CLOBBERDIRS =


