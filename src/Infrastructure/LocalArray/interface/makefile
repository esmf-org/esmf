# $Id: makefile,v 1.19 2009/06/05 23:46:37 theurich Exp $

ALL: build_here 

AUTOGEN   = ESMF_LocalArrayWrapperType.F90 ESMF_LocalArrayCreate.F90 ESMF_LocalArrayGet.F90
SOURCEC	  = ESMCI_LocalArray_F.C 
SOURCEF	  = $(AUTOGEN) ESMF_LocalArray.F90 ESMF_LocalArray_C.F90
SOURCEH	  = 

# List all .h files which should be copied to a common include dir
STOREH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/LocalArray/interface

CLEANDIRS   =
CLEANFILES  = $(AUTOGEN)
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

# on the absoft compiler on the mac, files in this dir have to have
# > 16 bit internal jump tables.  see the absoft faq's for more info.
ifeq ($(ESMF_COMPILER),absoft)
ESMF_F90COMPILEOPTS += -N11
endif

# leave the following line blank.
DIRS	  = 

