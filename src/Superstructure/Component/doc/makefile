# $Id: makefile,v 1.1 2003/01/07 21:36:12 nscollins Exp $ 

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

## sorry, i cannot make this work with the larger build env.
## TODO: fix this so you can build the docs as part of the
## larger system.  in the meantime, this just work locally
## and not with the nested make system.  nsc  01/07/2003
#
include ${ESMF_DIR}/build/${ESMF_ARCH}/base
#
DO_LATEX = do_latex
DO_L2H = do_l2h

# Dvi files
Component_desdoc.dvi: Component_desdoc.ctex
	${DO_LATEX} Component des

Component_reqdoc.dvi:
	${DO_LATEX} Component req

Component_refdoc.dvi:
	${DO_LATEX} Component ref


# Pdf files
Component_desdoc.pdf: Component_desdoc.dvi
	dvipdf Component_desdoc.dvi Component_desdoc.pdf

Component_reqdoc.pdf: Component_reqdoc.dvi
	dvipdf Component_reqdoc.dvi Component_reqdoc.pdf

Component_refdoc.pdf: Component_refdoc.dvi
	dvipdf Component_refdoc.dvi Component_refdoc.pdf

# PS files
Component_desdoc.ps: Component_desdoc.dvi
	dvips -o Component_desdoc.ps Component_desdoc.dvi

Component_reqdoc.ps: Component_reqdoc.dvi
	dvips -o Component_reqdoc.ps Component_reqdoc.dvi

Component_refdoc.ps: Component_refdoc.dvi
	dvips -o Component_refdoc.ps Component_refdoc.dvi

# Html Files
Component_desdoc: 
	${DO_L2H} Component des

Component_reqdoc: 
	${DO_L2H} Component req

Component_refdoc: 
	${DO_L2H} Component ref

