# $Id: makefile,v 1.2 2003/02/27 17:54:23 flanigan Exp $ 
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
	${DOL2H} DistGrid des
	mv -f DistGrid_desdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/DistGrid_reqdoc: 
	${DOL2H} DistGrid req
	mv -f DistGrid_reqdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/DistGrid_refdoc: 
	${DOL2H} DistGrid ref
	mv -f DistGrid_refdoc ${ESMC_DOCDIR}
