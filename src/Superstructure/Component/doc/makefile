# $Id: makefile,v 1.13 2003/03/25 23:13:41 flanigan Exp $ 

all: alldoc

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
LOCDIR	  = src/Superstructure/Component/doc

DVIFILES  =     $(ESMC_DOCDIR)/Component_desdoc.dvi \
                $(ESMC_DOCDIR)/Component_refdoc.dvi
#		$(ESMC_DOCDIR)/Component_reqdoc.dvi \


PDFFILES  = $(addsuffix .pdf, $(basename $(DVIFILES)))
PSFILES  = $(addsuffix .ps, $(basename $(DVIFILES)))

HTMLFILES =     $(ESMC_DOCDIR)/Component_desdoc \
                $(ESMC_DOCDIR)/Component_refdoc
#		$(ESMC_DOCDIR)/Component_reqdoc \



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

#
# Tex Files to make
#
TEXFILES_TO_MAKE  = $(addsuffix _ccapi.tex, $(basename $(notdir $(wildcard ../src/*.C))))
TEXFILES_TO_MAKE += $(addsuffix _chapi.tex, $(basename $(notdir $(wildcard ../include/*.h))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.F))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.F90))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.f))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.f90))))

TEXFILES_TO_MAKE += $(addsuffix _ccapi.tex, $(basename $(notdir $(wildcard ../examples/*.C))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../examples/*.F))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../examples/*.F90))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../examples/*.f))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../examples/*.f90))))

include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = Component_desdoc Component_refdoc
CLEANFILES  = $(TEXFILES_TO_MAKE) *.aux *.bbl *.blg *.log *.toc *.dvi Component_desdoc.tex Component_refdoc.tex
CLOBBERDIRS =


# Dvi files
$(ESMC_DOCDIR)/Component_desdoc.dvi: Component_desdoc.ctex $(TEXFILES_TO_MAKE)
	${DO_LATEX} Component des 
	mv -f Component_desdoc.dvi $(ESMC_DOCDIR)

$(ESMC_DOCDIR)/Component_reqdoc.dvi:
	${DO_LATEX} Component req 
	mv -f Component_reqdoc.dvi $(ESMC_DOCDIR)

$(ESMC_DOCDIR)/Component_refdoc.dvi: $(TEXFILES_TO_MAKE)
	${DO_LATEX} Component ref 
	mv -f Component_refdoc.dvi $(ESMC_DOCDIR)


# Pdf files
$(ESMC_DOCDIR)/Component_desdoc.pdf: $(ESMC_DOCDIR)/Component_desdoc.dvi
	dvipdf $(ESMC_DOCDIR)/Component_desdoc.dvi $(ESMC_DOCDIR)/Component_desdoc.pdf


$(ESMC_DOCDIR)/Component_reqdoc.pdf: $(ESMC_DOCDIR)/Component_reqdoc.dvi
	dvipdf $(ESMC_DOCDIR)/Component_reqdoc.dvi $(ESMC_DOCDIR)/Component_reqdoc.pdf


$(ESMC_DOCDIR)/Component_refdoc.pdf: $(ESMC_DOCDIR)/Component_refdoc.dvi
	dvipdf $(ESMC_DOCDIR)/Component_refdoc.dvi $(ESMC_DOCDIR)/Component_refdoc.pdf


# PS files
# Component_desdoc.ps: Component_desdoc.dvi
# 	dvips -o Component_desdoc.ps Component_desdoc.dvi
# 	mv -f Component_desdoc.ps ${ESMC_DOCDIR}

# Component_reqdoc.ps: Component_reqdoc.dvi
# 	dvips -o Component_reqdoc.ps Component_reqdoc.dvi
# 	mv -f Component_reqdoc.ps ${ESMC_DOCDIR}

# Component_refdoc.ps: Component_refdoc.dvi
# 	dvips -o Component_refdoc.ps Component_refdoc.dvi
# 	mv -f Component_refdoc.ps ${ESMC_DOCDIR}

# Html Files
$(ESMC_DOCDIR)/Component_desdoc: 
	${DO_L2H} Component des
	rm -fr ${ESMC_DOCDIR}/Component_desdoc
	mv -f Component_desdoc ${ESMC_DOCDIR}

$(ESMC_DOCDIR)/Component_reqdoc: 
	${DO_L2H} Component req
	rm -fr ${ESMC_DOCDIR}/Component_reqdoc
	mv -f Component_reqdoc ${ESMC_DOCDIR}

$(ESMC_DOCDIR)/Component_refdoc: 
	${DO_L2H} Component ref
	rm -fr ${ESMC_DOCDIR}/Component_refdoc
	mv -f Component_refdoc ${ESMC_DOCDIR}



## stuff i added locally
#fsrc:
# 	do_fprotex compex ../examples/ESMF_CompEx.F90
# 	do_fprotex comp ../src/ESMF_Comp.F90
# 	do_fprotex stateex ../examples/ESMF_StateEx.F90
# 	do_fprotex state ../src/ESMF_State.F90
# 	do_fprotex xformex ../examples/ESMF_XformEx.F90
# 	do_fprotex xform ../src/ESMF_Xform.F90
# 	do_fprotex appcompex ../examples/ESMF_AppCompEx.F90
# 	do_fprotex appmainex ../examples/ESMF_AppMainEx.F90
# 	do_fprotex cplex ../examples/ESMF_CplEx.F90
# 	do_fprotex gcompex ../examples/ESMF_GCompEx.F90

#ccsrc:
# 	do_ccprotex compex ../examples/ESMC_CompEx.C
# 	do_ccprotex comp ../interface/ESMC_Comp.C
# 	do_ccprotex stateex ../examples/ESMC_StateEx.C
# 	do_ccprotex state ../interface/ESMC_State.C
# 	do_ccprotex xformex ../examples/ESMC_XformEx.C
# 	do_ccprotex xform ../interface/ESMC_Xform.C
# 	do_ccprotex appcompex ../examples/ESMC_AppCompEx.C
# 	do_ccprotex appmainex ../examples/ESMC_AppMainEx.C
# 	do_ccprotex cplex ../examples/ESMC_CplEx.C
# 	do_ccprotex gcompex ../examples/ESMC_GCompEx.C


