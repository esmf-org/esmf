# $Id: makefile,v 1.15 2003/03/10 02:22:39 cdeluca Exp $ 
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
		${ESMC_DOCDIR}/ESMF_usrdoc.dvi 


PDFFILES  = $(addsuffix .pdf, $(basename $(DVIFILES)))

HTMLFILES = \
		${ESMC_DOCDIR}/ESMF_reqdoc \
		${ESMC_DOCDIR}/ESMF_usrdoc 

USERDOC_FILES = ESMF_usrdoc.tex ESMF_desc.tex ESMF_install.tex 

# Not a complete list of the requirement \input docs.
REQDOC_FILES  = ESMF_reqdoc.ctex ESMF_syn.tex ESMF_auth.tex ESMF_buildreq.tex

include $(ESMF_DIR)/makefile

DIRS = 



# Dvi files
${ESMC_DOCDIR}/ESMF_reqdoc.dvi: $(REQDOC_FILES)
	${DO_LATEX} ESMF req
	mv -f ESMF_reqdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/ESMF_usrdoc.dvi: $(USERDOC_FILES)
	export TEXINPUTS=".:$(ESMF_BUILD_DOCDIR):" ;\
	${DO_LATEX} ESMF usr
	mv -f ESMF_usrdoc.dvi ${ESMC_DOCDIR}


# Pdf files
${ESMC_DOCDIR}/ESMF_reqdoc.pdf: ${ESMC_DOCDIR}/ESMF_reqdoc.dvi
	dvipdf ${ESMC_DOCDIR}/ESMF_reqdoc.dvi ${ESMC_DOCDIR}/ESMF_reqdoc.pdf

${ESMC_DOCDIR}/ESMF_usrdoc.pdf: ${ESMC_DOCDIR}/ESMF_usrdoc.dvi
	dvipdf ${ESMC_DOCDIR}/ESMF_usrdoc.dvi ${ESMC_DOCDIR}/ESMF_usrdoc.pdf



# Html Files
${ESMC_DOCDIR}/ESMF_reqdoc: $(REQDOC_FILES)
	-rm -r $(ESMC_DOCDIR)/ESMF_reqdoc
	${DOL2H} ESMF req
	mv -f ESMF_reqdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/ESMF_usrdoc: $(USERDOC_FILES)
	-rm -r $(ESMC_DOCDIR)/ESMF_usrdoc
	${DOL2H} ESMF usr
	mv -f ESMF_usrdoc ${ESMC_DOCDIR}







