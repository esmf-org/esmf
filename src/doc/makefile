# $Id: makefile,v 1.12 2003/02/07 20:13:17 flanigan Exp $ 
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
LOCDIR	  = src/doc

DVIFILES  = \
		${ESMC_DOCDIR}/ESMF_reqdoc.dvi \
		${ESMC_DOCDIR}/ESMF_usrdoc.dvi \
                $(ESMC_DOCDIR)/ESMF_archdoc.dvi \


PDFFILES  = $(addsuffix .pdf, $(basename $(DVIFILES)))

HTMLFILES = \
		${ESMC_DOCDIR}/ESMF_reqdoc \
		${ESMC_DOCDIR}/ESMF_usrdoc \
                $(ESMC_DOCDIR)/ESMF_archdoc

USERDOC_FILES = ESMF_usrdoc.tex ESMF_desc.tex ESMF_install.tex 

# Not a complete list of the requirement \input docs.
REQDOC_FILES  = ESMF_reqdoc.ctex ESMF_syn.tex ESMF_auth.tex ESMF_buildreq.tex

ARCHDOC_FILES = ESMF_archdoc.ctex ESMF_archintro.tex ESMF_shortscope.tex ESMF_strategies.tex ESMF_conventions.tex \
                ESMF_superclasses.tex ESMF_fieldclasses.tex ESMF_utilclasses.tex ESMF_implications.tex ESMF_terms.tex \
                ESMF_archauth.tex

include ${ESMF_DIR}/build/${ESMF_ARCH}/base

# Dvi files
${ESMC_DOCDIR}/ESMF_reqdoc.dvi: $(REQDOC_FILES)
	${DO_LATEX} ESMF req
	mv -f ESMF_reqdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/ESMF_usrdoc.dvi: $(USERDOC_FILES)
	${DO_LATEX} ESMF usr
	mv -f ESMF_usrdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/ESMF_archdoc.dvi: $(ARCHDOC_FILES)
	${DO_LATEX} ESMF arch
	mv -f ESMF_archdoc.dvi ${ESMC_DOCDIR}


# Pdf files
${ESMC_DOCDIR}/ESMF_reqdoc.pdf: ${ESMC_DOCDIR}/ESMF_reqdoc.dvi
	dvipdf ${ESMC_DOCDIR}/ESMF_reqdoc.dvi ${ESMC_DOCDIR}/ESMF_reqdoc.pdf

${ESMC_DOCDIR}/ESMF_usrdoc.pdf: ${ESMC_DOCDIR}/ESMF_usrdoc.dvi
	dvipdf ${ESMC_DOCDIR}/ESMF_usrdoc.dvi ${ESMC_DOCDIR}/ESMF_usrdoc.pdf

${ESMC_DOCDIR}/ESMF_archdoc.pdf: ${ESMC_DOCDIR}/ESMF_archdoc.dvi
	dvipdf ${ESMC_DOCDIR}/ESMF_archdoc.dvi ${ESMC_DOCDIR}/ESMF_archdoc.pdf



# Html Files
${ESMC_DOCDIR}/ESMF_reqdoc: $(REQDOC_FILES)
	-rm -r $(ESMC_DOCDIR)/ESMF_reqdoc
	${DOL2H} ESMF req
	mv -f ESMF_reqdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/ESMF_usrdoc: $(USERDOC_FILES)
	-rm -r $(ESMC_DOCDIR)/ESMF_usrdoc
	${DOL2H} ESMF usr
	mv -f ESMF_usrdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/ESMF_archdoc: $(ARCHDOC_FILES)
	-rm -r $(ESMC_DOCDIR)/ESMF_archdoc
	${DOL2H} ESMF arch
	mv -f ESMF_archdoc ${ESMC_DOCDIR}





