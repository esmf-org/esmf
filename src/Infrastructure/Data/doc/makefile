# $Id: makefile,v 1.4 2003/03/10 03:03:01 cdeluca Exp $ 
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
LOCDIR	  = src/Infrastructure/Data/doc

DVIFILES  = \
#		${ESMC_DOCDIR}/Data_reqdoc.dvi 
#		${ESMC_DOCDIR}/Data_desdoc.dvi \
#		${ESMC_DOCDIR}/Data_refdoc.dvi

PDFFILES  = $(addsuffix .pdf, $(basename $(DVIFILES)))

HTMLFILES = \
#		${ESMC_DOCDIR}/Data_reqdoc 
#		${ESMC_DOCDIR}/Data_desdoc \
#		${ESMC_DOCDIR}/Data_refdoc

include $(ESMF_DIR)/makefile

DIRS	  = 

# Dvi files
${ESMC_DOCDIR}/Data_desdoc.dvi:
	${DO_LATEX} Data des
	mv -f Data_desdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/Data_reqdoc.dvi:
	${DO_LATEX} Data req
	mv -f Data_reqdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/Data_refdoc.dvi:
	${DO_LATEX} Data ref
	mv -f Data_refdoc.dvi ${ESMC_DOCDIR}


# Pdf files
${ESMC_DOCDIR}/Data_desdoc.pdf: ${ESMC_DOCDIR}/Data_desdoc.dvi
	dvipdf ${ESMC_DOCDIR}/Data_desdoc.dvi ${ESMC_DOCDIR}/Data_desdoc.pdf

${ESMC_DOCDIR}/Data_reqdoc.pdf: ${ESMC_DOCDIR}/Data_reqdoc.dvi
	dvipdf ${ESMC_DOCDIR}/Data_reqdoc.dvi ${ESMC_DOCDIR}/Data_reqdoc.pdf

${ESMC_DOCDIR}/Data_refdoc.pdf: ${ESMC_DOCDIR}/Data_refdoc.dvi
	dvipdf ${ESMC_DOCDIR}/Data_refdoc.dvi ${ESMC_DOCDIR}/Data_refdoc.pdf

# Html Files
${ESMC_DOCDIR}/Data_desdoc: 
	${DO_L2H} Data des
	mv -f Data_desdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/Data_reqdoc: 
	${DO_L2H} Data req
	mv -f Data_reqdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/Data_refdoc: 
	${DO_L2H} Data ref
	mv -f Data_refdoc ${ESMC_DOCDIR}
