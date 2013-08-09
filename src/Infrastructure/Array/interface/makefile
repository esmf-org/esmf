# $Id$

ALL: build_here

AUTOGEN   = ESMF_ArrayCreate.F90 ESMF_ArrayGet.F90 ESMF_ArrayGather.F90 ESMF_ArrayScatter.F90
SOURCEC	  = ESMCI_Array_F.C ESMC_Array.C
SOURCEF	  = $(AUTOGEN) ESMF_ArrayHa.F90 ESMF_Array.F90
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

