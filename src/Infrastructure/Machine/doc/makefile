# $Id: makefile,v 1.5 2003/03/21 17:57:58 flanigan Exp $ 
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
LOCDIR	  = src/Infrastructure/Machine/doc

DVIFILES  = \
		${ESMC_DOCDIR}/Machine_desdoc.dvi \
		${ESMC_DOCDIR}/Machine_refdoc.dvi

#		${ESMC_DOCDIR}/Machine_reqdoc.dvi 

PDFFILES  = $(addsuffix .pdf, $(basename $(DVIFILES)))

HTMLFILES = \
		${ESMC_DOCDIR}/Machine_desdoc \
		${ESMC_DOCDIR}/Machine_refdoc

#		${ESMC_DOCDIR}/Machine_reqdoc 

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
${ESMC_DOCDIR}/Machine_desdoc.dvi:
	${DO_LATEX} Machine des
	mv -f Machine_desdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/Machine_reqdoc.dvi:
	${DO_LATEX} Machine req
	mv -f Machine_reqdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/Machine_refdoc.dvi:
	${DO_LATEX} Machine ref
	mv -f Machine_refdoc.dvi ${ESMC_DOCDIR}


# Pdf files
${ESMC_DOCDIR}/Machine_desdoc.pdf: ${ESMC_DOCDIR}/Machine_desdoc.dvi
	dvipdf ${ESMC_DOCDIR}/Machine_desdoc.dvi ${ESMC_DOCDIR}/Machine_desdoc.pdf

${ESMC_DOCDIR}/Machine_reqdoc.pdf: ${ESMC_DOCDIR}/Machine_reqdoc.dvi
	dvipdf ${ESMC_DOCDIR}/Machine_reqdoc.dvi ${ESMC_DOCDIR}/Machine_reqdoc.pdf

${ESMC_DOCDIR}/Machine_refdoc.pdf: ${ESMC_DOCDIR}/Machine_refdoc.dvi
	dvipdf ${ESMC_DOCDIR}/Machine_refdoc.dvi ${ESMC_DOCDIR}/Machine_refdoc.pdf

# Html Files
${ESMC_DOCDIR}/Machine_desdoc: 
	${DO_L2H} Machine des
	mv -f Machine_desdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/Machine_reqdoc: 
	${DO_L2H} Machine req
	mv -f Machine_reqdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/Machine_refdoc: 
	${DO_L2H} Machine ref
	mv -f Machine_refdoc ${ESMC_DOCDIR}
