# $Id$ 

ALL: onedoc

LOCDIR	  = src/Superstructure/Component/doc

DVIFILES  = Component_refdoc.dvi Component_crefdoc.dvi
PDFFILES  = Component_refdoc.pdf Component_crefdoc.pdf
HTMLFILES = Component_refdoc Component_crefdoc

#
# Tex Files to make
#
TEXFILES_TO_MAKE  = $(addsuffix _ccapi.tex, $(basename $(notdir $(wildcard ../src/*.C))))
TEXFILES_TO_MAKE += $(addsuffix _chapi.tex, $(basename $(notdir $(wildcard ../include/*.h))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.F))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.F90))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.f))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.f90))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../src/*.cppF90))))

TEXFILES_TO_MAKE += $(addsuffix _ccapi.tex, $(basename $(notdir $(wildcard ../interface/*.C))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../interface/*.F90))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../interface/*.cppF90))))

TEXFILES_TO_MAKE += $(addsuffix _ccapi.tex, $(basename $(notdir $(wildcard ../examples/*.C))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../examples/*.F))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../examples/*.F90))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../examples/*.f))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../examples/*.f90))))

#
# Files that a doc depends on.  Do not list the the ctex file !  
# The ctex file is listed in the rule found in the build/common file.
# These lists almost certainly will not be an exhastive list of 
# all of the dependent files, but even a partial listing  will be helpfull.
#
REFDOC_DEP_FILES = $(TEXFILES_TO_MAKE) Component_crefdoc.ctex Component_refdoc.ctex GridComp_cdesc.tex GridComp_desc.tex GridComp_rest.tex GridComp_usage.tex CplComp_cdesc.tex CplComp_desc.tex CplComp_rest.tex CplComp_usage.tex SciComp_cdesc.tex SciComp_desc.tex SciComp_rest.tex SciComp_usage.tex CompTunnel_desc.tex CompTunnel_rest.tex CompTunnel_usage.tex

include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = Component_refdoc Component_crefdoc $(HTMLFILES)
CLEANFILES  = $(TEXFILES_TO_MAKE) $(CLEAN_TEXFILES) \
              Component_refdoc.tex Component_crefdoc.tex $(PDFFILES) 
CLOBBERDIRS =








