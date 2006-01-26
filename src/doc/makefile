# $Id: makefile,v 1.45 2006/01/26 21:40:01 nscollins Exp $ 

ALL: doc

#
#  Note that the ref doc build rules are redefined here.
#  The build rules for the ref file in this file override
#  the ref build rules in build/common.
#


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

DVIFILES  =	ESMF_refdoc.dvi \
                ESMF_usrdoc.dvi \
                ESMF_reqdoc.dvi

PDFFILES  =	${ESMC_DOCDIR}/ESMF_refdoc.pdf \
		${ESMC_DOCDIR}/ESMF_usrdoc.pdf \
		${ESMC_DOCDIR}/ESMF_reqdoc.ps \
		${ESMC_DOCDIR}/ESMF_reqdoc.pdf 


HTMLFILES =	${ESMC_DOCDIR}/ESMF_refdoc \
		${ESMC_DOCDIR}/ESMF_usrdoc \
                ${ESMC_DOCDIR}/ESMF_reqdoc


TEXINPUTS_VALUE = ".:$(ESMF_DIR)/src/doc:$(ESMF_BUILD_DOCDIR):$(ESMF_DIR)/src/demo/coupled_flow:" 

# Files that a doc depends on.  Do not list the the ctex file !  
# The ctex file is listed in the rule found in the build/common file.
# These lists almost certainly will not be an exhaustive list of 
# all of the dependent files, but even a partial listing  will be helpful.

REQDOC_DEP_FILES = ESMF_syn.tex ESMF_auth.tex ESMF_buildreq.tex 
DESDOC_DEP_FILES = 
REFDOC_DEP_FILES = 
USRDOC_DEP_FILES = ESMF_desc.tex ESMF_install.tex  ESMF_techoverview.tex

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = ESMF_reqdoc ESMF_usrdoc ESMF_refdoc $(HTMLFILES)
CLEANFILES  = $(CLEAN_TEXFILES) ESMF.bib \
	      ESMF_reqdoc.tex ESMF_refdoc.tex ESMF_usrdoc.tex \
	      ESMF_reqdoc.pdf ESMF_refdoc.pdf ESMF_usrdoc.pdf ESMF_reqdoc.ps \
	      ESMF_reqdoc.bib ESMF_refdoc.bib ESMF_usrdoc.bib \
	      $(PDFFILES) $(DVIFILES)
CLOBBERDIRS = 

#-------------------------------------------------------------------------------
# dvi files
#-------------------------------------------------------------------------------
dvi:  tex $(DVIFILES)

ESMF_usrdoc.dvi: ESMF_usrdoc.ctex $(USRDOC_DEP_FILES)
	${DO_LATEX} ESMF usr

ESMF_reqdoc.dvi: ESMF_reqdoc.ctex $(REQDOC_DEP_FILES)
	${DO_LATEX} ESMF req

ESMF_refdoc.dvi: ESMF_refdoc.ctex $(REFDOC_DEP_FILES)
	${DO_LATEX} ESMF ref


#-------------------------------------------------------------------------------
# pdf files postprocessed from the dvi files
#-------------------------------------------------------------------------------
pdf:  $(DVIFILES) $(PDFFILES)

${ESMC_DOCDIR}/ESMF_usrdoc.pdf: ESMF_usrdoc.dvi
	dvipdf ESMF_usrdoc.dvi
	mv -f ESMF_usrdoc.pdf ${ESMC_DOCDIR}

${ESMC_DOCDIR}/ESMF_refdoc.pdf: ESMF_refdoc.dvi
	dvipdf ESMF_refdoc.dvi
	mv -f ESMF_refdoc.pdf ${ESMC_DOCDIR}

${ESMC_DOCDIR}/ESMF_reqdoc.ps: ESMF_reqdoc.dvi
	dvips -o ESMF_reqdoc.ps ESMF_reqdoc.dvi
	mv -f ESMF_reqdoc.ps ${ESMC_DOCDIR}

${ESMC_DOCDIR}/ESMF_reqdoc.pdf: ESMF_reqdoc.dvi
	dvipdf ESMF_reqdoc.dvi
	mv -f ESMF_reqdoc.pdf ${ESMC_DOCDIR}


#-------------------------------------------------------------------------------
# Html Files directly from the latex source
#-------------------------------------------------------------------------------
html:  tex $(HTMLFILES)

${ESMC_DOCDIR}/ESMF_usrdoc: ESMF_usrdoc.ctex
	-rm -r $(ESMC_DOCDIR)/ESMF_usrdoc
	if [ $(TEXINPUTS_VALUE)foo != foo ] ; then \
	  echo '$$TEXINPUTS = $(TEXINPUTS_VALUE)' > .latex2html-init ;\
	fi;
	${DO_L2H} ESMF usr
	rm -f .latex2html-init
	mv -f ESMF_usrdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/ESMF_reqdoc: ESMF_reqdoc.ctex
	-rm -rf $(ESMC_DOCDIR)/ESMF_reqdoc
	if [ $(TEXINPUTS_VALUE)foo != foo ] ; then \
	  echo '$$TEXINPUTS = $(TEXINPUTS_VALUE)' > .latex2html-init ;\
	fi;
	${DO_L2H} ESMF req
	rm -f .latex2html-init
	mv -f ESMF_reqdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/ESMF_refdoc: ESMF_refdoc.ctex
	-rm -rf $(ESMC_DOCDIR)/ESMF_refdoc
	if [ $(TEXINPUTS_VALUE)foo != foo ] ; then \
	  echo '$$TEXINPUTS = $(TEXINPUTS_VALUE)' > .latex2html-init ;\
	fi;
	${DO_L2H} ESMF ref
	rm -f .latex2html-init
	mv -f ESMF_refdoc ${ESMC_DOCDIR}


