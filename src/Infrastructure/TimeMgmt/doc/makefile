# $Id: makefile,v 1.4 2003/04/03 23:15:03 flanigan Exp $ 
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
LOCDIR	  = src/Infrastructure/TimeMgmt/doc

DVIFILES  = \
		${ESMC_DOCDIR}/TimeMgmt_refdoc.dvi \
		${ESMC_DOCDIR}/TimeMgmt_reqdoc.dvi \
		${ESMC_DOCDIR}/TimeMgmt_desdoc.dvi 

PDFFILES  = $(addsuffix .pdf, $(basename $(DVIFILES)))

HTMLFILES = \
		${ESMC_DOCDIR}/TimeMgmt_refdoc \
		${ESMC_DOCDIR}/TimeMgmt_reqdoc \
		${ESMC_DOCDIR}/TimeMgmt_desdoc 

include $(ESMF_DIR)/makefile

DIRS = 


# Dvi files
${ESMC_DOCDIR}/TimeMgmt_desdoc.dvi:
	${DO_LATEX} TimeMgmt des
	mv -f TimeMgmt_desdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/TimeMgmt_reqdoc.dvi:
	${DO_LATEX} TimeMgmt req
	mv -f TimeMgmt_reqdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/TimeMgmt_refdoc.dvi:
	${DO_LATEX} TimeMgmt ref
	mv -f TimeMgmt_refdoc.dvi ${ESMC_DOCDIR}


# Pdf files
${ESMC_DOCDIR}/TimeMgmt_desdoc.pdf: ${ESMC_DOCDIR}/TimeMgmt_desdoc.dvi
	dvipdf ${ESMC_DOCDIR}/TimeMgmt_desdoc.dvi ${ESMC_DOCDIR}/TimeMgmt_desdoc.pdf

${ESMC_DOCDIR}/TimeMgmt_reqdoc.pdf: ${ESMC_DOCDIR}/TimeMgmt_reqdoc.dvi
	dvipdf ${ESMC_DOCDIR}/TimeMgmt_reqdoc.dvi ${ESMC_DOCDIR}/TimeMgmt_reqdoc.pdf

${ESMC_DOCDIR}/TimeMgmt_refdoc.pdf: ${ESMC_DOCDIR}/TimeMgmt_refdoc.dvi
	dvipdf ${ESMC_DOCDIR}/TimeMgmt_refdoc.dvi ${ESMC_DOCDIR}/TimeMgmt_refdoc.pdf

# Html Files
${ESMC_DOCDIR}/TimeMgmt_desdoc: 
	${DO_L2H} TimeMgmt des
	mv -f TimeMgmt_desdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/TimeMgmt_reqdoc: 
	${DO_L2H} TimeMgmt req
	mv -f TimeMgmt_reqdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/TimeMgmt_refdoc: 
	${DO_L2H} TimeMgmt ref
	mv -f TimeMgmt_refdoc ${ESMC_DOCDIR}
