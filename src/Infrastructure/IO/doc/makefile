# $Id: makefile,v 1.1 2002/05/08 18:30:10 dneckels Exp $ 
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
LOCDIR	  = src/Infrastructure/IO/doc

DVIFILES  = \
		${ESMC_DOCDIR}/IO_reqdoc.dvi 

PDFFILES  = $(addsuffix .pdf, $(basename $(DVIFILES)))
HTMLFILES = \
		${ESMC_DOCDIR}/IO_reqdoc 


include ${ESMF_DIR}/build/${ESMF_ARCH}/base


${ESMC_DOCDIR}/IO_reqdoc.dvi:
	${DO_LATEX} IO req
	mv -f IO_reqdoc.dvi ${ESMC_DOCDIR}


# Pdf files

${ESMC_DOCDIR}/IO_reqdoc.pdf: ${ESMC_DOCDIR}/IO_reqdoc.dvi
	dvipdf ${ESMC_DOCDIR}/IO_reqdoc.dvi ${ESMC_DOCDIR}/IO_reqdoc.pdf


# Html Files

${ESMC_DOCDIR}/IO_reqdoc: 
	${DOL2H} IO req
	mv -f IO_reqdoc ${ESMC_DOCDIR}

