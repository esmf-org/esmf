# $Id: makefile,v 1.2 2003/02/27 17:54:37 flanigan Exp $ 
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
LOCDIR	  = src/Infrastructure/IO/doc

DVIFILES  = \
		${ESMC_DOCDIR}/IO_reqdoc.dvi 

PDFFILES  = $(addsuffix .pdf, $(basename $(DVIFILES)))
HTMLFILES = \
		${ESMC_DOCDIR}/IO_reqdoc 


include $(ESMF_DIR)/makefile

DIRS	  = 

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

