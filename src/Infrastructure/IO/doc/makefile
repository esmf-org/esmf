# $Id: makefile,v 1.4 2003/03/20 20:33:03 flanigan Exp $ 
ALL: lib 

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  = 
SOURCEH	  = 
OBJSC     = 
OBJSF     = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Infrastructure/IO/doc

DVIFILES  = \
		${ESMC_DOCDIR}/IO_reqdoc.dvi 

PDFFILES  = $(addsuffix .pdf, $(basename $(DVIFILES)))
HTMLFILES = \
		${ESMC_DOCDIR}/IO_reqdoc 


#
# Tex Files to make
#
TEXFILES_TO_MAKE  = $(addsuffix _ccapi.tex, $(basename $(notdir $(wildcard ../src/*.C))))
TEXFILES_TO_MAKE += $(addsuffix _chapi.tex, $(basename $(notdir $(wildcard ../include/*.h))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.F))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.F90))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.f))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.f90))))


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

