# $Id$ 

ALL: doc

#
#  Note that the ref doc build rules are redefined here.
#  The build rules for the ref file in this file override
#  the ref build rules in build/common.
#

LOCDIR	  = src/doc

DVIFILES  =	ESMF_refdoc.dvi \
		ESMC_crefdoc.dvi \
                ESMF_usrdoc.dvi

PDFFILES  =	${ESMC_DOCDIR}/ESMF_refdoc.pdf \
		${ESMC_DOCDIR}/ESMC_crefdoc.pdf \
		${ESMC_DOCDIR}/ESMF_usrdoc.pdf

HTMLFILES =	${ESMC_DOCDIR}/ESMF_refdoc \
		${ESMC_DOCDIR}/ESMC_crefdoc \
		${ESMC_DOCDIR}/ESMF_usrdoc

TEXINPUTS_VALUE = ".:$(ESMF_DIR)/src/doc:$(ESMF_BUILD_DOCDIR):"

# Files that a doc depends on.  Do not list the the ctex file !  
# The ctex file is listed in the rule found in the build/common file.
# These lists almost certainly will not be an exhaustive list of 
# all of the dependent files, but even a partial listing  will be helpful.

REFDOC_DEP_FILES = ESMF_options.tex
USRDOC_DEP_FILES = ESMF_acknowledge.tex ESMF_whatisesmf.tex ESMF_usrintro.tex ESMF_usrsupport.tex ESMF_quickstart.tex ESMF_techoverview.tex ESMF_archoverview.tex ESMF_adoption.tex ESMF_use.tex ESMF_apps.tex ESMF_install.tex ESMF_builddetail.tex ESMF_testing.tex ESMF_examples.tex ESMF_esmfmkfiletesting.tex $(ESMF_DIR)/build/doc/user_arch.tex

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = ESMF_refdoc $(HTMLFILES) \
	      ESMC_crefdoc $(HTMLFILES)
CLEANFILES  = $(CLEAN_TEXFILES) ESMF.bib ESMC.bib\
	      ESMF_refdoc.tex ESMC_crefdoc.tex ESMF_usrdoc.tex \
	      ESMF_refdoc.pdf ESMC_crefdoc.pdf ESMF_usrdoc.pdf \
	      ESMF_refdoc.bib ESMC_crefdoc.bib ESMF_usrdoc.bib \
	      $(PDFFILES) $(DVIFILES)
CLOBBERDIRS = 

#-------------------------------------------------------------------------------
# dvi files
#-------------------------------------------------------------------------------
dvi:  tex $(DVIFILES)

ESMF_usrdoc.dvi: ESMF_usrdoc.ctex $(USRDOC_DEP_FILES)
	${DO_LATEX} ESMF usr

ESMF_refdoc.dvi: ESMF_refdoc.ctex $(REFDOC_DEP_FILES)
	${DO_LATEX} ESMF ref

ESMC_crefdoc.dvi: ESMC_crefdoc.ctex $(REFDOC_DEP_FILES)
	${DO_LATEX} ESMC cref


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

${ESMC_DOCDIR}/ESMC_crefdoc.pdf: ESMC_crefdoc.dvi
	dvipdf ESMC_crefdoc.dvi
	mv -f ESMC_crefdoc.pdf ${ESMC_DOCDIR}

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

${ESMC_DOCDIR}/ESMF_refdoc: ESMF_refdoc.ctex
	-rm -rf $(ESMC_DOCDIR)/ESMF_refdoc
	if [ $(TEXINPUTS_VALUE)foo != foo ] ; then \
	  echo '$$TEXINPUTS = $(TEXINPUTS_VALUE)' > .latex2html-init ;\
	fi;
	${DO_L2H} ESMF ref
	rm -f .latex2html-init
	mv -f ESMF_refdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/ESMC_crefdoc: ESMC_crefdoc.ctex
	-rm -rf $(ESMC_DOCDIR)/ESMC_crefdoc
	if [ $(TEXINPUTS_VALUE)foo != foo ] ; then \
	  echo '$$TEXINPUTS = $(TEXINPUTS_VALUE)' > .latex2html-init ;\
	fi;
	${DO_L2H} ESMC cref
	rm -f .latex2html-init
	mv -f ESMC_crefdoc ${ESMC_DOCDIR}



