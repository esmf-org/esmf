# $Id: makefile,v 1.5 2003/02/11 18:58:56 eschwab Exp $
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
LOCDIR	  = src/Infrastructure/TimeMgr/doc

DVIFILES  = \
		${ESMC_DOCDIR}/TimeMgr_reqdoc.dvi \
		${ESMC_DOCDIR}/TimeMgr_desdoc.dvi \
		${ESMC_DOCDIR}/TimeMgr_refdoc.dvi

PDFFILES  = $(addsuffix .pdf, $(basename $(DVIFILES)))

HTMLFILES = \
		${ESMC_DOCDIR}/TimeMgr_reqdoc \
		${ESMC_DOCDIR}/TimeMgr_desdoc \
		${ESMC_DOCDIR}/TimeMgr_refdoc

include ${ESMF_DIR}/build/${ESMF_ARCH}/base

# Dvi files
${ESMC_DOCDIR}/TimeMgr_desdoc.dvi:
	${DO_LATEX} TimeMgr des
	mv -f TimeMgr_desdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/TimeMgr_reqdoc.dvi:
	${DO_LATEX} TimeMgr req
	mv -f TimeMgr_reqdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/TimeMgr_refdoc.dvi:
	${DO_LATEX} TimeMgr ref
	mv -f TimeMgr_refdoc.dvi ${ESMC_DOCDIR}


# Pdf files
${ESMC_DOCDIR}/TimeMgr_desdoc.pdf: ${ESMC_DOCDIR}/TimeMgr_desdoc.dvi
	dvipdf ${ESMC_DOCDIR}/TimeMgr_desdoc.dvi ${ESMC_DOCDIR}/TimeMgr_desdoc.pdf

${ESMC_DOCDIR}/TimeMgr_reqdoc.pdf: ${ESMC_DOCDIR}/TimeMgr_reqdoc.dvi
	dvipdf ${ESMC_DOCDIR}/TimeMgr_reqdoc.dvi ${ESMC_DOCDIR}/TimeMgr_reqdoc.pdf

${ESMC_DOCDIR}/TimeMgr_refdoc.pdf: ${ESMC_DOCDIR}/TimeMgr_refdoc.dvi
	dvipdf ${ESMC_DOCDIR}/TimeMgr_refdoc.dvi ${ESMC_DOCDIR}/TimeMgr_refdoc.pdf

# Html Files
${ESMC_DOCDIR}/TimeMgr_desdoc: 
	${DOL2H} TimeMgr des
	mv -f TimeMgr_desdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/TimeMgr_reqdoc: 
	${DOL2H} TimeMgr req
	mv -f TimeMgr_reqdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/TimeMgr_refdoc: 
	${DOL2H} TimeMgr ref
	mv -f TimeMgr_refdoc ${ESMC_DOCDIR}
