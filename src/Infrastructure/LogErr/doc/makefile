# $Id: makefile,v 1.1 2003/03/28 23:04:17 nscollins Exp $
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
LOCDIR	  = src/Infrastructure/LogErr/doc

DVIFILES  = \
		${ESMC_DOCDIR}/LogErr_desdoc.dvi 
#		${ESMC_DOCDIR}/LogErr_reqdoc.dvi \
#		${ESMC_DOCDIR}/LogErr_refdoc.dvi

PDFFILES  = $(addsuffix .pdf, $(basename $(DVIFILES)))

HTMLFILES = \
		${ESMC_DOCDIR}/LogErr_desdoc 
#		${ESMC_DOCDIR}/LogErr_reqdoc \
#		${ESMC_DOCDIR}/LogErr_refdoc


#
# Tex Files to make
#
TEXFILES_TO_MAKE  = $(addsuffix _ccapi.tex, $(basename $(notdir $(wildcard ../src/*.C))))
TEXFILES_TO_MAKE += $(addsuffix _chapi.tex, $(basename $(notdir $(wildcard ../include/*.h))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.F))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.F90))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.f))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.f90))))

TEXFILES_TO_MAKE += $(addsuffix _ccapi.tex, $(basename $(notdir $(wildcard ../examples/*.C))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../examples/*.F))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../examples/*.F90))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../examples/*.f))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../examples/*.f90))))


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(TEXFILES_TO_MAKE) *.aux *.bbl *.blg *.log *.toc *.dvi LogErr_reqdoc.tex LogErr_desdoc.tex
CLOBBERDIRS =


# Dvi files
${ESMC_DOCDIR}/LogErr_desdoc.dvi:
	${DO_LATEX} LogErr des
	mv -f LogErr_desdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/LogErr_reqdoc.dvi:
	${DO_LATEX} LogErr req
	mv -f LogErr_reqdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/LogErr_refdoc.dvi:
	${DO_LATEX} LogErr ref
	mv -f LogErr_refdoc.dvi ${ESMC_DOCDIR}


# Pdf files
${ESMC_DOCDIR}/LogErr_desdoc.pdf: ${ESMC_DOCDIR}/LogErr_desdoc.dvi
	dvipdf ${ESMC_DOCDIR}/LogErr_desdoc.dvi ${ESMC_DOCDIR}/LogErr_desdoc.pdf

${ESMC_DOCDIR}/LogErr_reqdoc.pdf: ${ESMC_DOCDIR}/LogErr_reqdoc.dvi
	dvipdf ${ESMC_DOCDIR}/LogErr_reqdoc.dvi ${ESMC_DOCDIR}/LogErr_reqdoc.pdf

${ESMC_DOCDIR}/LogErr_refdoc.pdf: ${ESMC_DOCDIR}/LogErr_refdoc.dvi
	dvipdf ${ESMC_DOCDIR}/LogErr_refdoc.dvi ${ESMC_DOCDIR}/LogErr_refdoc.pdf

# Html Files
${ESMC_DOCDIR}/LogErr_desdoc: 
	${DO_L2H} LogErr des
	mv -f LogErr_desdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/LogErr_reqdoc: 
	${DO_L2H} LogErr req
	mv -f LogErr_reqdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/LogErr_refdoc: 
	${DO_L2H} LogErr ref
	mv -f LogErr_refdoc ${ESMC_DOCDIR}
