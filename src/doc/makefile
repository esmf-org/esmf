# $Id: makefile,v 1.37 2003/09/22 17:25:41 flanigan Exp $ 
ALL: alldoc 


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

DVIFILES  =	ESMF_reqdoc.dvi \
		ESMF_usrdoc.dvi \
                ESMF_refdoc.dvi

PDFFILES  =     ${ESMC_DOCDIR}/ESMF_reqdoc.pdf \
		${ESMC_DOCDIR}/ESMF_usrdoc.pdf \
                ${ESMC_DOCDIR}/ESMF_refdoc.pdf

HTMLFILES =	${ESMC_DOCDIR}/ESMF_reqdoc \
		${ESMC_DOCDIR}/ESMF_usrdoc \
                ${ESMC_DOCDIR}/ESMF_refdoc


#
# Files that a doc depends on.  Do not list the the ctex file !  
# The ctex file is listed in the rule found in the build/common file.
# These lists almost certainly will not be an exhastive list of 
# all of the dependent files, but even a partial listing  will be helpful.
#
REQDOC_DEP_FILES = ESMF_syn.tex ESMF_auth.tex ESMF_buildreq.tex 
DESDOC_DEP_FILES = 
REFDOC_DEP_FILES = 
USRDOC_DEP_FILES = ESMF_desc.tex ESMF_install.tex  ESMF_techoverview.tex

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = ESMF_reqdoc ESMF_usrdoc ESMF_refdoc $(HTMLFILES)
CLEANFILES  = *.aux *.bbl *.blg *.log *.toc *.dvi *.ORIG ESMF_reqdoc.tex ESMF_refdoc.tex ESMF_usrdoc.tex $(PDFFILES) $(DVIFILES)
CLOBBERDIRS = 

# TEXINPUTS_VALUE lists the paths to search for files specified by the
# \input and \includegraphics LateX commands.  Paths are seperated by
# colons.  The first path should be a "." for the local directory.
# The last path listed should have a trailing colon.

TEXINPUTS_VALUE = ".:graphics:blank_graphics:$(ESMF_BUILD_DOCDIR):$(ESMF_DIR)/src/Demo/coupled_flow:" 

#
# The build rules for the req are in 
# build/common.
#

# Dvi files
ESMF_usrdoc.dvi: $(USRDOC_DEP_FILES)
	cd $(ESMF_TOP_DIR) ; $(MAKE) tex
	export TEXINPUTS=$(TEXINPUTS_VALUE) ;\
	${DO_LATEX} ESMF usr

ESMF_refdoc.dvi: ESMF_refdoc.ctex $(REFDOC_DEP_FILES)
	export TEXINPUTS=$(TEXINPUTS_VALUE) ;\
	cd $(ESMF_TOP_DIR) ; $(MAKE) tex
	${DO_LATEX} ESMF ref


# Html Files
${ESMC_DOCDIR}/ESMF_usrdoc: $(USRDOC_DEP_FILES)
	cd $(ESMF_TOP_DIR) ; $(MAKE) tex
	-rm -r $(ESMC_DOCDIR)/ESMF_usrdoc
	if [ $(TEXINPUTS_VALUE)foo != foo ] ; then \
	  echo '$$TEXINPUTS = $(TEXINPUTS_VALUE)' > .latex2html-init ;\
	fi;
	${DO_L2H} ESMF usr
	rm -f .latex2html-init
	mv -f ESMF_usrdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/ESMF_refdoc: ESMF_refdoc.ctex $(REFDOC_DEP_FILES)
	cd $(ESMF_TOP_DIR) ; $(MAKE) tex
	-rm -rf $(ESMC_DOCDIR)/ESMF_refdoc
	if [ $(TEXINPUTS_VALUE)foo != foo ] ; then \
	  echo '$$TEXINPUTS = $(TEXINPUTS_VALUE)' > .latex2html-init ;\
	fi;
	${DO_L2H} ESMF ref
	rm -f .latex2html-init
	mv -f ESMF_refdoc ${ESMC_DOCDIR}











