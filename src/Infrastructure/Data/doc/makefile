# $Id: makefile,v 1.13 2003/05/29 20:59:07 flanigan Exp $ 
ALL: alldoc

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
LOCDIR	  = src/Infrastructure/Data/doc

DVIFILES  = 	Data_refdoc.dvi \
#		Data_reqdoc.dvi \
#		Data_desdoc.dvi 


PDFFILES  =     ${ESMC_DOCDIR}/Data_refdoc.pdf \
#		${ESMC_DOCDIR}/Data_reqdoc.pdf \
#		${ESMC_DOCDIR}/Data_desdoc.pdf \

HTMLFILES = 	${ESMC_DOCDIR}/Data_refdoc \
#		${ESMC_DOCDIR}/Data_reqdoc \
#		${ESMC_DOCDIR}/Data_desdoc \


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

#
# Files that a doc depends on.  Do not list the the ctex file !  
# The ctex file is listed in the rule found in the build/common file.
# These lists almost certainly will not be an exhaustive list of 
# all of the dependent files, but even a partial listing  will be helpful.
#
REQDOC_DEP_FILES = 
DESDOC_DEP_FILES = $(TEXFILES_TO_MAKE) 
REFDOC_DEP_FILES = $(TEXFILES_TO_MAKE)


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = Data_refdoc $(HTMLFILES)
CLEANFILES  = $(TEXFILES_TO_MAKE) *.aux *.bbl *.blg *.log *.toc *.dvi \
              *.ORIG Data_refdoc.tex $(PDFFILES)
CLOBBERDIRS =


