# $Id: makefile,v 1.21 2003/04/03 17:41:06 flanigan Exp $ 
ALL: alldoc 



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
                ${ESMC_DOCDIR}/ESMF_refdoc.dvi


PDFFILES  = $(addsuffix .pdf, $(basename $(DVIFILES)))

HTMLFILES = \
		${ESMC_DOCDIR}/ESMF_reqdoc \
		${ESMC_DOCDIR}/ESMF_usrdoc \
                ${ESMC_DOCDIR}/ESMF_refdoc

USRDOC_FILES = ESMF_usrdoc.tex ESMF_desc.tex ESMF_install.tex 

# Not a complete list of the requirement \input docs.
REQDOC_FILES  = ESMF_reqdoc.ctex ESMF_syn.tex ESMF_auth.tex ESMF_buildreq.tex

include $(ESMF_DIR)/makefile

refdoc : $(ESMC_DOCDIR)/ESMF_refdoc.pdf

cleanref : 
	rm -f $(ESMC_DOCDIR)/ESMF_refdoc.*


DIRS = 

CLEANDIRS   = ESMF_reqdoc ESMF_usrdoc ESMF_refdoc
CLEANFILES  = *.aux *.bbl *.blg *.log *.toc *.dvi  ESMF_reqdoc.tex ESMF_refdoc.tex
CLOBBERDIRS = 

refdoc : $(ESMC_DOCDIR)/ESMF_refdoc.pdf


# Dvi files
${ESMC_DOCDIR}/ESMF_reqdoc.dvi: $(REQDOC_FILES)
	${DO_LATEX} ESMF req
	mv -f ESMF_reqdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/ESMF_usrdoc.dvi: $(USRDOC_FILES)
	export TEXINPUTS=".:$(ESMF_BUILD_DOCDIR):" ;\
	${DO_LATEX} ESMF usr
	mv -f ESMF_usrdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/ESMF_refdoc.dvi: $(REFDOC_FILES)
	${DO_LATEX} ESMF ref
	mv -f ESMF_refdoc.dvi ${ESMC_DOCDIR}


# Pdf files
${ESMC_DOCDIR}/ESMF_reqdoc.pdf: ${ESMC_DOCDIR}/ESMF_reqdoc.dvi
	dvipdf ${ESMC_DOCDIR}/ESMF_reqdoc.dvi ${ESMC_DOCDIR}/ESMF_reqdoc.pdf

${ESMC_DOCDIR}/ESMF_usrdoc.pdf: ${ESMC_DOCDIR}/ESMF_usrdoc.dvi
	dvipdf ${ESMC_DOCDIR}/ESMF_usrdoc.dvi ${ESMC_DOCDIR}/ESMF_usrdoc.pdf

${ESMC_DOCDIR}/ESMF_refdoc.pdf: ${ESMC_DOCDIR}/ESMF_refdoc.dvi
	dvipdf ${ESMC_DOCDIR}/ESMF_refdoc.dvi ${ESMC_DOCDIR}/ESMF_refdoc.pdf



# Html Files
${ESMC_DOCDIR}/ESMF_reqdoc: $(REQDOC_FILES)
	-rm -r $(ESMC_DOCDIR)/ESMF_reqdoc
	${DO_L2H} ESMF req
	mv -f ESMF_reqdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/ESMF_usrdoc: $(USRDOC_FILES)
	-rm -r $(ESMC_DOCDIR)/ESMF_usrdoc
	${DO_L2H} ESMF usr
	mv -f ESMF_usrdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/ESMF_refdoc: $(REFDOC_FILES)
	-rm -r $(ESMC_DOCDIR)/ESMF_refdoc
	${DO_L2H} ESMF ref
	mv -f ESMF_refdoc ${ESMC_DOCDIR}







