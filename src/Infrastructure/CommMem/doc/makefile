# $Id: makefile,v 1.8 2003/04/02 19:51:43 flanigan Exp $ 
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
LOCDIR	  = src/Infrastructure/CommMem/doc

DVIFILES  = \
		${ESMC_DOCDIR}/CommMem_reqdoc.dvi \
		${ESMC_DOCDIR}/CommMem_desdoc.dvi \
		${ESMC_DOCDIR}/CommMem_refdoc.dvi

PDFFILES  = $(addsuffix .pdf, $(basename $(DVIFILES)))

HTMLFILES = \
		${ESMC_DOCDIR}/CommMem_reqdoc \
		${ESMC_DOCDIR}/CommMem_desdoc \
		${ESMC_DOCDIR}/CommMem_refdoc

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
CLEANFILES  = $(TEXFILES_TO_MAKE) *.aux *.bbl *.blg *.log *.toc *.dvi CommMem_desdoc.tex CommMem_reqdoc.tex CommMem_refdoc.tex
CLOBBERDIRS =

# Dvi files
${ESMC_DOCDIR}/CommMem_desdoc.dvi:
	${DO_LATEX} CommMem des
	mv -f CommMem_desdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/CommMem_reqdoc.dvi:
	${DO_LATEX} CommMem req
	mv -f CommMem_reqdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/CommMem_refdoc.dvi:
	${DO_LATEX} CommMem ref
	mv -f CommMem_refdoc.dvi ${ESMC_DOCDIR}


# Pdf files
${ESMC_DOCDIR}/CommMem_desdoc.pdf: ${ESMC_DOCDIR}/CommMem_desdoc.dvi
	dvipdf ${ESMC_DOCDIR}/CommMem_desdoc.dvi ${ESMC_DOCDIR}/CommMem_desdoc.pdf

${ESMC_DOCDIR}/CommMem_reqdoc.pdf: ${ESMC_DOCDIR}/CommMem_reqdoc.dvi
	dvipdf ${ESMC_DOCDIR}/CommMem_reqdoc.dvi ${ESMC_DOCDIR}/CommMem_reqdoc.pdf

${ESMC_DOCDIR}/CommMem_refdoc.pdf: ${ESMC_DOCDIR}/CommMem_refdoc.dvi
	dvipdf ${ESMC_DOCDIR}/CommMem_refdoc.dvi ${ESMC_DOCDIR}/CommMem_refdoc.pdf

# Html Files
${ESMC_DOCDIR}/CommMem_desdoc: 
	${DO_L2H} CommMem des
	mv -f CommMem_desdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/CommMem_reqdoc: 
	${DO_L2H} CommMem req
	mv -f CommMem_reqdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/CommMem_refdoc: 
	${DO_L2H} CommMem ref
	mv -f CommMem_refdoc ${ESMC_DOCDIR}
