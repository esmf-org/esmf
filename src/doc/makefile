# $Id: makefile,v 1.3 2002/09/09 21:56:37 cdeluca Exp $ 
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
DIRS	  = 
LOCDIR	  = src/doc

DVIFILES  = \
		${ESMC_DOCDIR}/ESMF_reqdoc.dvi \
		${ESMC_DOCDIR}/ESMF_usrdoc.dvi \

PDFFILES  = $(addsuffix .pdf, $(basename $(DVIFILES)))

HTMLFILES = \
		${ESMC_DOCDIR}/ESMF_reqdoc \
		${ESMC_DOCDIR}/ESMF_usrdoc 

include ${ESMF_DIR}/build/${ESMF_ARCH}/base

# Dvi files
${ESMC_DOCDIR}/ESMF_reqdoc.dvi:
	${DO_LATEX} ESMF req
	mv -f ESMF_reqdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/ESMF_usrdoc.dvi:
	${DO_LATEX} ESMF usr
	mv -f ESMF_usrdoc.dvi ${ESMC_DOCDIR}


# Pdf files
${ESMC_DOCDIR}/ESMF_reqdoc.pdf: ${ESMC_DOCDIR}/ESMF_reqdoc.dvi
	dvipdf ${ESMC_DOCDIR}/ESMF_reqdoc.dvi ${ESMC_DOCDIR}/ESMF_reqdoc.pdf

${ESMC_DOCDIR}/ESMF_usrdoc.pdf: ${ESMC_DOCDIR}/ESMF_usrdoc.dvi
	dvipdf ${ESMC_DOCDIR}/ESMF_usrdoc.dvi ${ESMC_DOCDIR}/ESMF_usrdoc.pdf


# Html Files
${ESMC_DOCDIR}/ESMF_reqdoc: 
	${DOL2H} ESMF req
	mv -f ESMF_reqdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/ESMF_usrdoc: 
	${DOL2H} ESMF usr
	mv -f ESMF_usrdoc ${ESMC_DOCDIR}

