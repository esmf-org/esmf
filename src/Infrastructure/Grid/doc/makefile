# $Id: makefile,v 1.3 2003/02/27 17:54:35 flanigan Exp $ 
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
	${DOL2H} Grid des
	mv -f Grid_desdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/Grid_reqdoc: 
	${DOL2H} Grid req
	mv -f Grid_reqdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/Grid_refdoc: 
	${DOL2H} Grid ref
	mv -f Grid_refdoc ${ESMC_DOCDIR}
