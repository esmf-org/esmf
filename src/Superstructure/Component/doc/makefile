# $Id: makefile,v 1.6 2003/01/30 23:42:35 nscollins Exp $ 

all: alldocs

ALL:  lib
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
LOCDIR	  = src/Superstructure/Component/doc

DVIFILES  = \
		Component_desdoc.dvi 
#		Component_reqdoc.dvi \
#		Component_refdoc.dvi

PDFFILES  = $(addsuffix .pdf, $(basename $(DVIFILES)))
PSFILES  = $(addsuffix .ps, $(basename $(DVIFILES)))

HTMLFILES = \
		Component_desdoc 
#		Component_reqdoc \
#		Component_refdoc

alldocs: ${DVIFILES} ${PDFFILES} ${HTMLFILES} ${PSFILES}

# TODO: this should be a very long list of all files which are
#  part of the design doc.
DESTEXFILES = Component_desdoc.ctex  

# these are the requirements files, which currently aren't in this
# directory.  they seem to be in the Coupler directory.
REQTEXFILES = Component_reqdoc.ctex

# programmers's reference document
REFREXFILES = Component_refdoc.ctex

## TODO: need to generate <class>_fapi.tex and <class>_ccapi.tex
## files from the real source code here using protex:
#
# e.g. comp_fapi.tex: ../interface/ESMF_Component.F90
#                     do_fprotex comp ../interface/ESMF_Component.F90
#
#      comp_ccapi.tex: ../src/ESMC_Component.C
#                     do_ccprotex comp ../src/ESMC_Component.C
#

include ${ESMF_DIR}/build/${ESMF_ARCH}/base


# Dvi files
Component_desdoc.dvi: Component_desdoc.ctex fsrc ccsrc
	${DO_LATEX} Component des

Component_reqdoc.dvi:
	${DO_LATEX} Component req

Component_refdoc.dvi:
	${DO_LATEX} Component ref


# Pdf files
Component_desdoc.pdf: Component_desdoc.dvi
	dvipdf Component_desdoc.dvi Component_desdoc.pdf
	mv -f Component_desdoc.pdf ${ESMC_DOCDIR}

Component_reqdoc.pdf: Component_reqdoc.dvi
	dvipdf Component_reqdoc.dvi Component_reqdoc.pdf
	mv -f Component_reqdoc.pdf ${ESMC_DOCDIR}

Component_refdoc.pdf: Component_refdoc.dvi
	dvipdf Component_refdoc.dvi Component_refdoc.pdf
	mv -f Component_refdoc.pdf ${ESMC_DOCDIR}

# PS files
Component_desdoc.ps: Component_desdoc.dvi
	dvips -o Component_desdoc.ps Component_desdoc.dvi
	mv -f Component_desdoc.ps ${ESMC_DOCDIR}

Component_reqdoc.ps: Component_reqdoc.dvi
	dvips -o Component_reqdoc.ps Component_reqdoc.dvi
	mv -f Component_reqdoc.ps ${ESMC_DOCDIR}

Component_refdoc.ps: Component_refdoc.dvi
	dvips -o Component_refdoc.ps Component_refdoc.dvi
	mv -f Component_refdoc.ps ${ESMC_DOCDIR}

# Html Files
# the original rules have a problem - if the dir already exists the mv -f
# will fail.  (also, i didn't have a doc dir and no rule created it the
# first time - but that's another problem.)  i put in the following
# hack which tries to remove the old dir first and then move the new dir.
Component_desdoc: 
	${DO_L2H} Component des
	rm -fr ${ESMC_DOCDIR}/Component_desdoc
	mv -f Component_desdoc ${ESMC_DOCDIR}

Component_reqdoc: 
	${DO_L2H} Component req
	mv -f Component_reqdoc ${ESMC_DOCDIR}

Component_refdoc: 
	${DO_L2H} Component ref
	mv -f Component_refdoc ${ESMC_DOCDIR}



## stuff i added locally
fsrc:
	do_fprotex state ../src/ESMF_State.F90
	do_fprotex comp ../src/ESMF_Comp.F90
	do_fprotex app ../src/ESMF_App.F90
	do_fprotex xform ../src/ESMF_Xform.F90
	do_fprotex cpl ../src/ESMF_Cpl.F90
	touch gcomp_fapi.tex
	do_fprotex stateex ../examples/ESMF_StateCreateEx.F90
	do_fprotex compex ../examples/ESMF_CompCreateEx.F90

ccsrc:
	do_ccprotex state ../interface/ESMC_State.C
	do_ccprotex comp ../interface/ESMC_Comp.C
	do_ccprotex app ../interface/ESMC_App.C
	do_ccprotex xform ../interface/ESMC_Xform.C
	do_ccprotex cpl ../interface/ESMC_Cpl.C
	touch gcomp_ccapi.tex
	#do_ccprotex stateex ../examples/ESMC_StateCreateEx.C
	#do_ccprotex compex ../examples/ESMC_CompCreateEx.C


