# $Id: makefile,v 1.2 2003/03/20 20:32:58 flanigan Exp $ 
ALL: lib 

DESTEX = Bundle_ccex.tex Bundle_ccparam.tex \
      Bundle_def.tex Bundle_desc.tex Bundle_design.tex \
      Bundle_fex.tex Bundle_fparam.tex Bundle_rest.tex \
      Field_abbr.tex Field_alg.tex Field_background.tex \
      Field_ccex.tex Field_ccparam.tex Field_def.tex \
      Field_desc.tex Field_design.tex \
      Field_fex.tex Field_fparam.tex Field_glos.tex Field_prog.tex \
      Field_obj.tex Field_req.tex Field_reqdoc.ctex Field_rest.tex \
      Field_syn.tex Field_terms.tex Field_desdoc.ctex

DESEPS = fieldclass.eps bundleclass.eps 

REQTEX = Field_auth.tex Field_background.tex Field_terms.tex \
         Field_abbr.tex Field_req.tex Field_desdoc.ctex

FSRC = ../src/ESMF_Field.F90 ../src/ESMF_Bundle.F90 \
       ../src/ESMF_DataMap.F90 ../../Data/interface/ESMF_Array.F90 

# are these .C or .h (or both)?
CSRC = ../include/ESMC_Field.h ../include/ESMC_Bundle.h \
       ../include/ESMC_DataMap.h ../../Data/include/ESMC_Array.h 


CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  = 
SOURCEH	  = 
OBJSC     = 
OBJSF     = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Infrastructure/Field/doc

DVIFILES  = \
		${ESMC_DOCDIR}/Field_reqdoc.dvi \
		${ESMC_DOCDIR}/Field_desdoc.dvi \
		${ESMC_DOCDIR}/Field_refdoc.dvi

PDFFILES  = $(addsuffix .pdf, $(basename $(DVIFILES)))

HTMLFILES = \
		${ESMC_DOCDIR}/Field_reqdoc \
		${ESMC_DOCDIR}/Field_desdoc \
		${ESMC_DOCDIR}/Field_refdoc

#
# Tex Files to make
#
TEXFILES_TO_MAKE  = $(addsuffix _ccapi.tex, $(basename $(notdir $(wildcard ../src/*.C))))
TEXFILES_TO_MAKE += $(addsuffix _chapi.tex, $(basename $(notdir $(wildcard ../include/*.h))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.F))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.F90))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.f))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.f90))))


include $(ESMF_DIR)/makefile

DIRS	  = 


doc: ${ESMC_DOCDIR}/Field_desdoc.pdf ${ESMC_DOCDIR}/Field_desdoc.html \
     ${ESMC_DOCDIR}/Field_reqdoc.pdf ${ESMC_DOCDIR}/Field_reqdoc.html 
#    figs.ps figs.pdf

# Dvi files
${ESMC_DOCDIR}/Field_desdoc.dvi: Field_desdoc.ctex fsrc ccsrc
	${DO_LATEX} Field des
	mv -f Field_desdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/Field_reqdoc.dvi:
	${DO_LATEX} Field req
	mv -f Field_reqdoc.dvi ${ESMC_DOCDIR}

${ESMC_DOCDIR}/Field_refdoc.dvi:
	${DO_LATEX} Field ref
	mv -f Field_refdoc.dvi ${ESMC_DOCDIR}


# Pdf files
${ESMC_DOCDIR}/Field_desdoc.pdf: ${ESMC_DOCDIR}/Field_desdoc.dvi
	dvipdf ${ESMC_DOCDIR}/Field_desdoc.dvi ${ESMC_DOCDIR}/Field_desdoc.pdf

${ESMC_DOCDIR}/Field_reqdoc.pdf: ${ESMC_DOCDIR}/Field_reqdoc.dvi
	dvipdf ${ESMC_DOCDIR}/Field_reqdoc.dvi ${ESMC_DOCDIR}/Field_reqdoc.pdf

${ESMC_DOCDIR}/Field_refdoc.pdf: ${ESMC_DOCDIR}/Field_refdoc.dvi
	dvipdf ${ESMC_DOCDIR}/Field_refdoc.dvi ${ESMC_DOCDIR}/Field_refdoc.pdf

# Html Files
${ESMC_DOCDIR}/Field_desdoc.html: 
	rm -fr ${ESMC_DOCDIR}/Field_desdoc
	${DO_L2H} Field des
	mv -f Field_desdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/Field_reqdoc.html: 
	${DO_L2H} Field req
	mv -f Field_reqdoc ${ESMC_DOCDIR}

${ESMC_DOCDIR}/Field_refdoc.html: 
	${DO_L2H} Field ref
	mv -f Field_refdoc ${ESMC_DOCDIR}

# postscript files
Field_desdoc.ps:  Field_desdoc.dvi
	dvips -o Field_desdoc.ps Field_desdoc.dvi
	mv -f Field_desdoc.ps ${ESMF_DOCDIR}

Field_reqdoc.ps:  Field_reqdoc.dvi
	dvips -o Field_reqdoc.ps Field_reqdoc.dvi
	mv -f Field_reqdoc.ps ${ESMF_DOCDIR}

Field_refdoc.ps:  Field_refdoc.dvi
	dvips -o Field_refdoc.ps Field_refdoc.dvi
	mv -f Field_refdoc.ps ${ESMF_DOCDIR}


# figures
figs.ps:  figs.dvi
	dvips -o figs.ps figs.dvi

figs.pdf:  figs.dvi
	dvipdf figs.dvi figs.pdf

figs.dvi: figs.tex
	latex figs.tex
	latex figs.tex


# source files which gen input for design doc
ccsrc: ${CSRC}
# 	do_ccprotex field ../include/ESMC_Field.h
# 	do_ccprotex bundle ../include/ESMC_Bundle.h
# 	do_ccprotex array ../../Data/include/ESMC_Array.h
# 	do_ccprotex datamap ../include/ESMC_DataMap.h
# 	do_ccprotex base ../../Base/include/ESMC_Base.h 
# 	touch ccsrc

fsrc:  ${FSRC}
# 	do_fprotex field ../src/ESMF_Field.F90
# 	do_fprotex bundle ../src/ESMF_Bundle.F90
# 	do_fprotex array ../../Data/interface/ESMF_Array.F90
# 	do_fprotex datamap ../src/ESMF_DataMap.F90
# 	do_fprotex base ../../Base/interface/ESMF_Base.F90
# 	do_fprotex fieldex ../examples/ESMF_FieldCreateEx.F90
# 	do_fprotex bundleex ../examples/ESMF_BundleCreateEx.F90
# 	do_fprotex arrayex ../../Data/examples/ESMF_ArrayCreateEx.F90
# 	touch fsrc

clean:
	rm -f *.dvi *.ps Field_reqdoc/* Field_desdoc/* fsrc ccsrc






