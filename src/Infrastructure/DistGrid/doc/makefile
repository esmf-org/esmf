# $Id: makefile,v 1.5 2003/03/21 17:57:56 flanigan Exp $ 
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
LOCDIR	  = src/Infrastructure/DistGrid/doc

DVIFILES  = \
		${ESMC_DOCDIR}/DistGrid_reqdoc.dvi 
#		${ESMC_DOCDIR}/DistGrid_desdoc.dvi \
#		${ESMC_DOCDIR}/DistGrid_refdoc.dvi

PDFFILES  = $(addsuffix .pdf, $(basename $(DVIFILES)))

HTMLFILES = \
		${ESMC_DOCDIR}/DistGrid_reqdoc 
#		${ESMC_DOCDIR}/DistGrid_desdoc \
#		${ESMC_DOCDIR}/DistGrid_refdoc

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
${ESMC_DOCDIR}/DistGrid_desdoc.dvi:
	${DO_LATEX} DistGrid des
	mv -f DistGrid_desdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/DistGrid_reqdoc.dvi:
	${DO_LATEX} DistGrid req
	mv -f DistGrid_reqdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/DistGrid_refdoc.dvi:
	${DO_LATEX} DistGrid ref
	mv -f DistGrid_refdoc.dvi ${ESMC_DOCDIR}


# Pdf files
${ESMC_DOCDIR}/DistGrid_desdoc.pdf: ${ESMC_DOCDIR}/DistGrid_desdoc.dvi
	dvipdf ${ESMC_DOCDIR}/DistGrid_desdoc.dvi ${ESMC_DOCDIR}/DistGrid_desdoc.pdf

${ESMC_DOCDIR}/DistGrid_reqdoc.pdf: ${ESMC_DOCDIR}/DistGrid_reqdoc.dvi
	dvipdf ${ESMC_DOCDIR}/DistGrid_reqdoc.dvi ${ESMC_DOCDIR}/DistGrid_reqdoc.pdf

${ESMC_DOCDIR}/DistGrid_refdoc.pdf: ${ESMC_DOCDIR}/DistGrid_refdoc.dvi
	dvipdf ${ESMC_DOCDIR}/DistGrid_refdoc.dvi ${ESMC_DOCDIR}/DistGrid_refdoc.pdf

# Html Files
${ESMC_DOCDIR}/DistGrid_desdoc: 
	${DO_L2H} DistGrid des
	mv -f DistGrid_desdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/DistGrid_reqdoc: 
	${DO_L2H} DistGrid req
	mv -f DistGrid_reqdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/DistGrid_refdoc: 
	${DO_L2H} DistGrid ref
	mv -f DistGrid_refdoc ${ESMC_DOCDIR}
