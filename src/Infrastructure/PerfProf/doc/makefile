# $Id: makefile,v 1.8 2003/03/25 00:18:18 flanigan Exp $ 
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
LOCDIR	  = src/Infrastructure/PerfProf/doc

DVIFILES  = \
		${ESMC_DOCDIR}/PerfProf_reqdoc.dvi \
		${ESMC_DOCDIR}/PerfProf_desdoc.dvi 
#		${ESMC_DOCDIR}/PerfProf_refdoc.dvi

PDFFILES  = $(addsuffix .pdf, $(basename $(DVIFILES)))

HTMLFILES = \
		${ESMC_DOCDIR}/PerfProf_reqdoc \
		${ESMC_DOCDIR}/PerfProf_desdoc 
#		${ESMC_DOCDIR}/PerfProf_refdoc

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

CLEANDIRS   = PerfProf_desdoc PerfProd_reqdoc
CLEANFILES  = $(TEXFILES_TO_MAKE) *.aux *.bbl *.blg *.log *.toc *.dvi PerfProf_reqdoc.tex PerfProf_desdoc.tex
CLOBBERDIRS =


# Dvi files
${ESMC_DOCDIR}/PerfProf_desdoc.dvi:
	${DO_LATEX} PerfProf des
	mv -f PerfProf_desdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/PerfProf_reqdoc.dvi:
	${DO_LATEX} PerfProf req
	mv -f PerfProf_reqdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/PerfProf_refdoc.dvi:
	${DO_LATEX} PerfProf ref
	mv -f PerfProf_refdoc.dvi ${ESMC_DOCDIR}


# Pdf files
${ESMC_DOCDIR}/PerfProf_desdoc.pdf: ${ESMC_DOCDIR}/PerfProf_desdoc.dvi
	dvipdf ${ESMC_DOCDIR}/PerfProf_desdoc.dvi ${ESMC_DOCDIR}/PerfProf_desdoc.pdf

${ESMC_DOCDIR}/PerfProf_reqdoc.pdf: ${ESMC_DOCDIR}/PerfProf_reqdoc.dvi
	dvipdf ${ESMC_DOCDIR}/PerfProf_reqdoc.dvi ${ESMC_DOCDIR}/PerfProf_reqdoc.pdf

${ESMC_DOCDIR}/PerfProf_refdoc.pdf: ${ESMC_DOCDIR}/PerfProf_refdoc.dvi
	dvipdf ${ESMC_DOCDIR}/PerfProf_refdoc.dvi ${ESMC_DOCDIR}/PerfProf_refdoc.pdf

# Html Files
${ESMC_DOCDIR}/PerfProf_desdoc: 
	${DO_L2H} PerfProf des
	mv -f PerfProf_desdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/PerfProf_reqdoc: 
	${DO_L2H} PerfProf req
	mv -f PerfProf_reqdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/PerfProf_refdoc: 
	${DO_L2H} PerfProf ref
	mv -f PerfProf_refdoc ${ESMC_DOCDIR}
