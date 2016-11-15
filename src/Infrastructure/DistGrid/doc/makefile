# $Id$

All: onedoc

LOCDIR    = src/Infrastructure/DistGrid/doc

DVIFILES  = DistGrid_refdoc.dvi DistGrid_crefdoc.dvi
PDFFILES  = DistGrid_refdoc.pdf DistGrid_crefdoc.pdf
HTMLFILES = DistGrid_refdoc DistGrid_crefdoc
GRAPHFILES= dgconnect_2tiles_not_connected.eps \
            dgconnect_2tiles_connected.eps \
            dgconnect_1tile_not_connected.eps \
            dgconnect_1tile_periodic1_connected.eps \
            dgconnect_1tile_periodic2_connected.eps \
            dgconnect_1tile_peripole_connected.eps \
            dgconnect_1tile_peribipole_connected.eps \
            dgconnect_cusph_not_connected.eps \
            dgconnect_cusph_5connected.eps \
            dgconnect_cusph_6connected.eps \
            dgconnect_cusph_2rotconnected.eps \
            dgconnect_cusph_12connected.eps

#
# Tex Files to make
#
TEXFILES_TO_MAKE  = $(addsuffix _ccapi.tex, $(basename $(notdir $(wildcard ../src/*.C))))
TEXFILES_TO_MAKE += $(addsuffix _chapi.tex, $(basename $(notdir $(wildcard ../include/*.h))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../interface/*.F))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../interface/*.F90))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../interface/*.f))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../interface/*.f90))))

TEXFILES_TO_MAKE += $(addsuffix _ccapi.tex, $(basename $(notdir $(wildcard ../examples/*.C))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../examples/*.F))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../examples/*.F90))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../examples/*.f))))
TEXFILES_TO_MAKE += $(addsuffix _fapi.tex, $(basename $(notdir $(wildcard ../examples/*.f90))))

#
# Files that a doc depends on.  Do not list the the ctex file !  
# The ctex file is listed in the rule found in the build/common file.
# These lists almost certainly will not be an exhaustive list of 
# all of the dependent files, but even a partial listing  will be helpful.
#
REFDOC_DEP_FILES = $(TEXFILES_TO_MAKE) DistGrid_desc.tex DistGrid_options.tex DistGrid_usage.tex DistGrid_rest.tex DistGrid_implnotes.tex


include $(ESMF_DIR)/makefile

DIRS        = 
CLEANDIRS   = DistGrid_refdoc DistGrid_crefdoc $(HTMLFILES)
CLEANFILES  = $(TEXFILES_TO_MAKE) $(CLEAN_TEXFILES) \
              DistGrid_refdoc.tex DistGrid_crefdoc.tex $(PDFFILES) $(GRAPHFILES)
CLOBBERDIRS =

