# $Id: makefile,v 1.3 2003/03/10 03:03:06 cdeluca Exp $ 
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
