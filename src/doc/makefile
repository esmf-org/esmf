# $Id: makefile,v 1.24 2003/04/08 16:06:09 cdeluca Exp $ 
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

DVIFILES  = 	${ESMC_DOCDIR}/ESMF_reqdoc.dvi \
		${ESMC_DOCDIR}/ESMF_usrdoc.dvi \
                ${ESMC_DOCDIR}/ESMF_refdoc.dvi


PDFFILES  = $(addsuffix .pdf, $(basename $(DVIFILES)))

HTMLFILES = 	${ESMC_DOCDIR}/ESMF_reqdoc \
		${ESMC_DOCDIR}/ESMF_usrdoc \
                ${ESMC_DOCDIR}/ESMF_refdoc


#
# Files that a doc depends on.  Do not list the the ctex file !  
# The ctex file is listed in the rule found in the build/common file.
# These lists almost certainly will not be an exhastive list of 
# all of the dependent files, but even a partial listing  will be helpful.
#
REQDOC_DEP_FILES = ESMF_syn.tex ESMF_auth.tex ESMF_buildreq.tex
DESDOC_DEP_FILES = $(TEXFILES_TO_MAKE) 
REFDOC_DEP_FILES = $(TEXFILES_TO_MAKE)

#  Note that ESMF_usrdoc.tex is not a .ctex file.
USRDOC_DEP_FILES = ESMF_usrdoc.tex ESMF_desc.tex ESMF_install.tex 

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = ESMF_reqdoc ESMF_usrdoc ESMF_refdoc
CLEANFILES  = *.aux *.bbl *.blg *.log *.toc *.dvi *.ORIG ESMF_reqdoc.tex ESMF_refdoc.tex
CLOBBERDIRS = 

#
# The build rules for the req are in 
# build/common.
#

# Dvi files
${ESMC_DOCDIR}/ESMF_usrdoc.dvi: $(USRDOC_DEP_FILES)
	export TEXINPUTS=".:$(ESMF_BUILD_DOCDIR):" ;\
	${DO_LATEX} ESMF usr
	mv -f ESMF_usrdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/ESMF_refdoc.dvi: ESMF_refdoc.ctex $(REFDOC_DEP_FILES)
	cd $(ESMF_TOP_DIR) ; $(MAKE) tex
	${DO_LATEX} ESMF ref
	mv -f ESMF_refdoc.dvi ${ESMC_DOCDIR}


# Pdf files
${ESMC_DOCDIR}/ESMF_usrdoc.pdf: ${ESMC_DOCDIR}/ESMF_usrdoc.dvi
	dvipdf ${ESMC_DOCDIR}/ESMF_usrdoc.dvi ${ESMC_DOCDIR}/ESMF_usrdoc.pdf


# Html Files
${ESMC_DOCDIR}/ESMF_usrdoc: $(USRDOC_DEP_FILES)
	-rm -r $(ESMC_DOCDIR)/ESMF_usrdoc
	export TEXINPUTS=".:$(ESMF_BUILD_DOCDIR):" ;\
	${DO_L2H} ESMF usr
	mv -f ESMF_usrdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/ESMF_refdoc: ESMF_refdoc.ctex $(REFDOC_DEP_FILES)
	cd $(ESMF_TOP_DIR) ; $(MAKE) tex
	-rm -rf $(ESMC_DOCDIR)/ESMF_refdoc
	${DO_L2H} ESMF ref
	mv -f ESMF_refdoc ${ESMC_DOCDIR}









