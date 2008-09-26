# $Id: makefile,v 1.25 2008/09/26 05:18:02 cdeluca Exp $

ALL: build_here

AUTOGEN   = ESMF_ArrayCreate.F90 ESMF_ArrayGet.F90 ESMF_ArrayGather.F90 ESMF_ArrayScatter.F90
SOURCEC	  = ESMCI_Array_F.C ESMC_Array.C
SOURCEF	  = $(AUTOGEN) ESMF_ArrayPr.F90 ESMF_Array.F90
SOURCEH	  = 

# List all .h files which should be copied to common include dir
STOREH	  = ESMC_Array.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Array/interface

CLEANDIRS   =
CLEANFILES  = $(AUTOGEN)
CLOBBERDIRS =

include ${ESMF_DIR}/makefile

DIRS	  = 

