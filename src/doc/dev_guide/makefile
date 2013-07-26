# $Id$ 

#

doall:  ESMF_devdoc.dvi

LOCDIR	  = src/doc/dev_guide

DVIFILES  =	ESMF_devdoc.dvi 

PDFFILES  =	${ESMC_DOCDIR}/ESMF_devdoc.pdf

HTMLFILES =	${ESMC_DOCDIR}/ESMF_devdoc


TEXINPUTS_VALUE =


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = ESMF_devdoc
CLEANFILES  = $(CLEAN_TEXFILES) $(PDFFILES) $(DVIFILES)
CLOBBERDIRS = 

#-------------------------------------------------------------------------------
# dvi files
#-------------------------------------------------------------------------------
dvi:  $(DVIFILES)

ESMF_devdoc.dvi: 
	sh bld


#-------------------------------------------------------------------------------
# pdf files postprocessed from the dvi files
#-------------------------------------------------------------------------------
pdf:  $(DVIFILES) $(PDFFILES)

${ESMC_DOCDIR}/ESMF_devdoc.pdf: ESMF_devdoc.dvi
	dvipdf ESMF_devdoc.dvi
	mv -f ESMF_devdoc.pdf ${ESMC_DOCDIR}


#-------------------------------------------------------------------------------
# Html Files directly from the latex source
#-------------------------------------------------------------------------------
html:  $(HTMLFILES)

${ESMC_DOCDIR}/ESMF_devdoc: ESMF_devdoc.ctex
	-rm -r $(ESMC_DOCDIR)/ESMF_devdoc
	if [ $(TEXINPUTS_VALUE)foo != foo ] ; then \
	  echo '$$TEXINPUTS = $(TEXINPUTS_VALUE)' > .latex2html-init ;\
	fi;
	${DO_L2H} ESMF dev
	rm -f .latex2html-init
	mv -f ESMF_devdoc ${ESMC_DOCDIR}


