# $Id: makefile,v 1.4 2003/03/10 03:03:07 cdeluca Exp $ 
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
LOCDIR	  = src/Infrastructure/PerfProf/doc

DVIFILES  = \
		${ESMC_DOCDIR}/PerfProf_reqdoc.dvi 
#		${ESMC_DOCDIR}/PerfProf_desdoc.dvi \
#		${ESMC_DOCDIR}/PerfProf_refdoc.dvi

PDFFILES  = $(addsuffix .pdf, $(basename $(DVIFILES)))

HTMLFILES = \
		${ESMC_DOCDIR}/PerfProf_reqdoc 
#		${ESMC_DOCDIR}/PerfProf_desdoc \
#		${ESMC_DOCDIR}/PerfProf_refdoc

include $(ESMF_DIR)/makefile

DIRS = 

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
