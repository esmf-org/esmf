

All: tex

LOCDIR = src/Infrastructure/Base/doc

DVIFILES  = ${ESMC_DOCDIR}/Base_refdoc.dvi

PDFFILES  = $(addsuffix .pdf, $(basename $(DVIFILES)))

HTMLFILES = ${ESMC_DOCDIR}/Base_refdoc


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

DIRS = 

CLEANDIRS   = Base_reqdoc Base_desdoc Base_refdoc 
CLEANFILES  = $(TEXFILES_TO_MAKE) *.aux *.bbl *.blg *.log *.toc *.dvi Base_reqdoc.tex Base_desdoc.tex Base_refdoc.tex 
CLOBBERDIRS =

