# $Id: makefile,v 1.3 2003/03/10 03:03:05 cdeluca Exp $ 
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
	${DO_L2H} IO req
	mv -f IO_reqdoc ${ESMC_DOCDIR}

