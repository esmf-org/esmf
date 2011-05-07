# $Id: makefile,v 1.28 2011/05/07 00:21:59 samsoncheung Exp $

ALL: build_here

AUTOGEN   = ESMF_ArrayCreate.F90 ESMF_ArrayGet.F90 ESMF_ArrayGather.F90 ESMF_ArrayScatter.F90 ESMF_ArrayIO.F90
SOURCEC	  = ESMCI_Array_F.C ESMC_Array.C
SOURCEF	  = $(AUTOGEN) ESMF_ArrayHa.F90 ESMF_Array.F90 ESMF_Array_C.F90
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

