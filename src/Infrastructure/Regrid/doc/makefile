# $Id: makefile,v 1.3 2003/03/10 03:03:08 cdeluca Exp $ 
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
LOCDIR	  = src/Infrastructure/Regrid/doc

DVIFILES  = \
		${ESMC_DOCDIR}/Regrid_reqdoc.dvi 
#		${ESMC_DOCDIR}/Regrid_desdoc.dvi \
#		${ESMC_DOCDIR}/Regrid_refdoc.dvi

PDFFILES  = $(addsuffix .pdf, $(basename $(DVIFILES)))

HTMLFILES = \
		${ESMC_DOCDIR}/Regrid_reqdoc 
#		${ESMC_DOCDIR}/Regrid_desdoc \
#		${ESMC_DOCDIR}/Regrid_refdoc

include $(ESMF_DIR)/makefile

DIRS	  = 

# Dvi files
${ESMC_DOCDIR}/Regrid_desdoc.dvi:
	${DO_LATEX} Regrid des
	mv -f Regrid_desdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/Regrid_reqdoc.dvi:
	${DO_LATEX} Regrid req
	mv -f Regrid_reqdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/Regrid_refdoc.dvi:
	${DO_LATEX} Regrid ref
	mv -f Regrid_refdoc.dvi ${ESMC_DOCDIR}


# Pdf files
${ESMC_DOCDIR}/Regrid_desdoc.pdf: ${ESMC_DOCDIR}/Regrid_desdoc.dvi
	dvipdf ${ESMC_DOCDIR}/Regrid_desdoc.dvi ${ESMC_DOCDIR}/Regrid_desdoc.pdf

${ESMC_DOCDIR}/Regrid_reqdoc.pdf: ${ESMC_DOCDIR}/Regrid_reqdoc.dvi
	dvipdf ${ESMC_DOCDIR}/Regrid_reqdoc.dvi ${ESMC_DOCDIR}/Regrid_reqdoc.pdf

${ESMC_DOCDIR}/Regrid_refdoc.pdf: ${ESMC_DOCDIR}/Regrid_refdoc.dvi
	dvipdf ${ESMC_DOCDIR}/Regrid_refdoc.dvi ${ESMC_DOCDIR}/Regrid_refdoc.pdf

# Html Files
${ESMC_DOCDIR}/Regrid_desdoc: 
	${DO_L2H} Regrid des
	mv -f Regrid_desdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/Regrid_reqdoc: 
	${DO_L2H} Regrid req
	mv -f Regrid_reqdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/Regrid_refdoc: 
	${DO_L2H} Regrid ref
	mv -f Regrid_refdoc ${ESMC_DOCDIR}
