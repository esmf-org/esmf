# $Id: makefile,v 1.5 2003/03/21 17:57:58 flanigan Exp $ 
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

DIRS	  = 


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
	${DO_L2H} PhysGrid des
	mv -f PhysGrid_desdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/PhysGrid_reqdoc: 
	${DO_L2H} PhysGrid req
	mv -f PhysGrid_reqdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/PhysGrid_refdoc: 
	${DO_L2H} PhysGrid ref
	mv -f PhysGrid_refdoc ${ESMC_DOCDIR}
