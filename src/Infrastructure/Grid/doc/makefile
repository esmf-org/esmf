# $Id: makefile,v 1.6 2003/03/21 17:57:57 flanigan Exp $ 
ALL: lib 

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
LOCDIR	  = src/Infrastructure/Grid/doc

DVIFILES  = $(ESMC_DOCDIR)/Grid_desdoc.dvi \
#		${ESMC_DOCDIR}/Grid_reqdoc.dvi \
#		${ESMC_DOCDIR}/Grid_desdoc.dvi \
#		${ESMC_DOCDIR}/Grid_refdoc.dvi

PDFFILES  = $(addsuffix .pdf, $(basename $(DVIFILES)))

HTMLFILES = $(ESMC_DOCDIR)/Grid_desdoc
#		${ESMC_DOCDIR}/Grid_reqdoc 
#		${ESMC_DOCDIR}/Grid_desdoc \
#		${ESMC_DOCDIR}/Grid_refdoc

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

DIRS	  = 


# Dvi files
${ESMC_DOCDIR}/Grid_desdoc.dvi:
	${DO_LATEX} Grid des
	mv -f Grid_desdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/Grid_reqdoc.dvi:
	${DO_LATEX} Grid req
	mv -f Grid_reqdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/Grid_refdoc.dvi:
	${DO_LATEX} Grid ref
	mv -f Grid_refdoc.dvi ${ESMC_DOCDIR}


# Pdf files
${ESMC_DOCDIR}/Grid_desdoc.pdf: ${ESMC_DOCDIR}/Grid_desdoc.dvi
	dvipdf ${ESMC_DOCDIR}/Grid_desdoc.dvi ${ESMC_DOCDIR}/Grid_desdoc.pdf

${ESMC_DOCDIR}/Grid_reqdoc.pdf: ${ESMC_DOCDIR}/Grid_reqdoc.dvi
	dvipdf ${ESMC_DOCDIR}/Grid_reqdoc.dvi ${ESMC_DOCDIR}/Grid_reqdoc.pdf

${ESMC_DOCDIR}/Grid_refdoc.pdf: ${ESMC_DOCDIR}/Grid_refdoc.dvi
	dvipdf ${ESMC_DOCDIR}/Grid_refdoc.dvi ${ESMC_DOCDIR}/Grid_refdoc.pdf

# Html Files
${ESMC_DOCDIR}/Grid_desdoc: 
	${DO_L2H} Grid des
	mv -f Grid_desdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/Grid_reqdoc: 
	${DO_L2H} Grid req
	mv -f Grid_reqdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/Grid_refdoc: 
	${DO_L2H} Grid ref
	mv -f Grid_refdoc ${ESMC_DOCDIR}
