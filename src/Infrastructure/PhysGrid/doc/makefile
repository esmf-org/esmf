# $Id: makefile,v 1.1 2002/11/01 19:51:08 jwolfe Exp $ 
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
LOCDIR	  = src/Infrastructure/PhysGrid/doc

DVIFILES  = \
		${ESMC_DOCDIR}/PhysGrid_reqdoc.dvi 
#		${ESMC_DOCDIR}/PhysGrid_desdoc.dvi \
#		${ESMC_DOCDIR}/PhysGrid_refdoc.dvi

PDFFILES  = $(addsuffix .pdf, $(basename $(DVIFILES)))

HTMLFILES = \
		${ESMC_DOCDIR}/PhysGrid_reqdoc 
#		${ESMC_DOCDIR}/PhysGrid_desdoc \
#		${ESMC_DOCDIR}/PhysGrid_refdoc

include ${ESMF_DIR}/build/${ESMF_ARCH}/base

# Dvi files
${ESMC_DOCDIR}/PhysGrid_desdoc.dvi:
	${DO_LATEX} PhysGrid des
	mv -f PhysGrid_desdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/PhysGrid_reqdoc.dvi:
	${DO_LATEX} PhysGrid req
	mv -f PhysGrid_reqdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/PhysGrid_refdoc.dvi:
	${DO_LATEX} PhysGrid ref
	mv -f PhysGrid_refdoc.dvi ${ESMC_DOCDIR}


# Pdf files
${ESMC_DOCDIR}/PhysGrid_desdoc.pdf: ${ESMC_DOCDIR}/PhysGrid_desdoc.dvi
	dvipdf ${ESMC_DOCDIR}/PhysGrid_desdoc.dvi ${ESMC_DOCDIR}/PhysGrid_desdoc.pdf

${ESMC_DOCDIR}/PhysGrid_reqdoc.pdf: ${ESMC_DOCDIR}/PhysGrid_reqdoc.dvi
	dvipdf ${ESMC_DOCDIR}/PhysGrid_reqdoc.dvi ${ESMC_DOCDIR}/PhysGrid_reqdoc.pdf

${ESMC_DOCDIR}/PhysGrid_refdoc.pdf: ${ESMC_DOCDIR}/PhysGrid_refdoc.dvi
	dvipdf ${ESMC_DOCDIR}/PhysGrid_refdoc.dvi ${ESMC_DOCDIR}/PhysGrid_refdoc.pdf

# Html Files
${ESMC_DOCDIR}/PhysGrid_desdoc: 
	${DOL2H} PhysGrid des
	mv -f PhysGrid_desdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/PhysGrid_reqdoc: 
	${DOL2H} PhysGrid req
	mv -f PhysGrid_reqdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/PhysGrid_refdoc: 
	${DOL2H} PhysGrid ref
	mv -f PhysGrid_refdoc ${ESMC_DOCDIR}
