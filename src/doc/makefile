# $Id: makefile,v 1.9 2003/02/04 17:29:16 flanigan Exp $ 
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
                $(ESMC_DOCDIR)/ESMF_archdoc.dvi \


PDFFILES  = $(addsuffix .pdf, $(basename $(DVIFILES)))

HTMLFILES = \
		${ESMC_DOCDIR}/ESMF_reqdoc \
		${ESMC_DOCDIR}/ESMF_usrdoc \
                $(ESMC_DOCDIR)/ESMF_archdoc

USERDOC_FILES = ESMF_usrdoc.tex ESMF_desc.tex ESMF_install.tex 

include ${ESMF_DIR}/build/${ESMF_ARCH}/base

# Dvi files
${ESMC_DOCDIR}/ESMF_reqdoc.dvi: ESMF_req.tex ESMF_buildreq.tex
	${DO_LATEX} ESMF req
	mv -f ESMF_reqdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/ESMF_usrdoc.dvi: $(USERDOC_FILES)
	${DO_LATEX} ESMF usr
	mv -f ESMF_usrdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/ESMF_archdoc.dvi: ESMF_archdoc.ctex
	${DO_LATEX} ESMF arch
	mv -f ESMF_archdoc.dvi ${ESMC_DOCDIR}


# Pdf files
${ESMC_DOCDIR}/ESMF_reqdoc.pdf: ${ESMC_DOCDIR}/ESMF_reqdoc.dvi
	dvipdf ${ESMC_DOCDIR}/ESMF_reqdoc.dvi ${ESMC_DOCDIR}/ESMF_reqdoc.pdf

${ESMC_DOCDIR}/ESMF_usrdoc.pdf: ${ESMC_DOCDIR}/ESMF_usrdoc.dvi
	dvipdf ${ESMC_DOCDIR}/ESMF_usrdoc.dvi ${ESMC_DOCDIR}/ESMF_usrdoc.pdf

${ESMC_DOCDIR}/ESMF_archdoc.pdf: ${ESMC_DOCDIR}/ESMF_archdoc.dvi
	dvipdf ${ESMC_DOCDIR}/ESMF_archdoc.dvi ${ESMC_DOCDIR}/ESMF_archdoc.pdf



# Html Files
${ESMC_DOCDIR}/ESMF_reqdoc: ESMF_req.tex ESMF_buildreq.tex
	${DOL2H} ESMF req
	mv -f ESMF_reqdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/ESMF_usrdoc: $(USERDOC_FILES)
	${DOL2H} ESMF usr
	mv -f ESMF_usrdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/ESMF_archdoc: 
	${DOL2H} ESMF arch
	mv -f ESMF_archdoc ${ESMC_DOCDIR}





