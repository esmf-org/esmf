# $Id: makefile,v 1.8 2003/03/20 20:33:17 flanigan Exp $
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
LOCDIR	  = src/Infrastructure/TimeMgr/doc

DVIFILES  = \
		${ESMC_DOCDIR}/TimeMgr_reqdoc.dvi \
		${ESMC_DOCDIR}/TimeMgr_desdoc.dvi \
		#${ESMC_DOCDIR}/TimeMgr_refdoc.dvi

PDFFILES  = $(addsuffix .pdf, $(basename $(DVIFILES)))

HTMLFILES = \
		${ESMC_DOCDIR}/TimeMgr_reqdoc \
		${ESMC_DOCDIR}/TimeMgr_desdoc \
		#${ESMC_DOCDIR}/TimeMgr_refdoc


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

DIRS = 

# Dvi files
${ESMC_DOCDIR}/TimeMgr_desdoc.dvi:
	${DO_LATEX} TimeMgr des
	mv -f TimeMgr_desdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/TimeMgr_reqdoc.dvi:
	${DO_LATEX} TimeMgr req
	mv -f TimeMgr_reqdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/TimeMgr_refdoc.dvi:
	${DO_LATEX} TimeMgr ref
	mv -f TimeMgr_refdoc.dvi ${ESMC_DOCDIR}


# Pdf files
${ESMC_DOCDIR}/TimeMgr_desdoc.pdf: ${ESMC_DOCDIR}/TimeMgr_desdoc.dvi
	dvipdf ${ESMC_DOCDIR}/TimeMgr_desdoc.dvi ${ESMC_DOCDIR}/TimeMgr_desdoc.pdf

${ESMC_DOCDIR}/TimeMgr_reqdoc.pdf: ${ESMC_DOCDIR}/TimeMgr_reqdoc.dvi
	dvipdf ${ESMC_DOCDIR}/TimeMgr_reqdoc.dvi ${ESMC_DOCDIR}/TimeMgr_reqdoc.pdf

${ESMC_DOCDIR}/TimeMgr_refdoc.pdf: ${ESMC_DOCDIR}/TimeMgr_refdoc.dvi
	dvipdf ${ESMC_DOCDIR}/TimeMgr_refdoc.dvi ${ESMC_DOCDIR}/TimeMgr_refdoc.pdf

# Html Files
${ESMC_DOCDIR}/TimeMgr_desdoc: 
	${DO_L2H} TimeMgr des
	mv -f TimeMgr_desdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/TimeMgr_reqdoc: 
	${DO_L2H} TimeMgr req
	mv -f TimeMgr_reqdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/TimeMgr_refdoc: 
	${DO_L2H} TimeMgr ref
	mv -f TimeMgr_refdoc ${ESMC_DOCDIR}
