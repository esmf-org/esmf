# $Id: makefile,v 1.4 2003/01/21 18:17:03 flanigan Exp $ 
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
		${ESMC_DOCDIR}/ESMF_buildreqdoc.dvi \

PDFFILES  = $(addsuffix .pdf, $(basename $(DVIFILES)))

HTMLFILES = \
		${ESMC_DOCDIR}/ESMF_reqdoc \
		${ESMC_DOCDIR}/ESMF_usrdoc 

include ${ESMF_DIR}/build/${ESMF_ARCH}/base

# Dvi files
${ESMC_DOCDIR}/ESMF_reqdoc.dvi: ESMF_req.tex build_req.tex
	${DO_LATEX} ESMF req
	mv -f ESMF_reqdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/ESMF_buildreqdoc.dvi: ESMF_buildreqdoc.tex build_req.tex
	${DO_LATEX} ESMF buildreq
	mv -f ESMF_buildreqdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/ESMF_usrdoc.dvi:
	${DO_LATEX} ESMF usr
	mv -f ESMF_usrdoc.dvi ${ESMC_DOCDIR}


# Pdf files
${ESMC_DOCDIR}/ESMF_reqdoc.pdf: ${ESMC_DOCDIR}/ESMF_reqdoc.dvi
	dvipdf ${ESMC_DOCDIR}/ESMF_reqdoc.dvi ${ESMC_DOCDIR}/ESMF_reqdoc.pdf

${ESMC_DOCDIR}/ESMF_buildreqdoc.pdf: ${ESMC_DOCDIR}/ESMF_buildreqdoc.dvi
	dvipdf ${ESMC_DOCDIR}/ESMF_buildreqdoc.dvi ${ESMC_DOCDIR}/ESMF_buildreqdoc.pdf

${ESMC_DOCDIR}/ESMF_usrdoc.pdf: ${ESMC_DOCDIR}/ESMF_usrdoc.dvi
	dvipdf ${ESMC_DOCDIR}/ESMF_usrdoc.dvi ${ESMC_DOCDIR}/ESMF_usrdoc.pdf


# Html Files
${ESMC_DOCDIR}/ESMF_reqdoc: 
	${DOL2H} ESMF req
	mv -f ESMF_reqdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/ESMF_usrdoc: 
	${DOL2H} ESMF usr
	mv -f ESMF_usrdoc ${ESMC_DOCDIR}

