# $Id$

ALL: build_here 

SOURCEC	  = ESMCI_VM.C ESMCI_VMKernel.C
SOURCEF	  = 
SOURCEH	  = 

ifeq ($(ESMF_ACC_FRAMEWORK),opencl)
SOURCEC += ESMC_OpenCLInfo.C
endif

# List all .h files which should be copied to common include dir
STOREH	  = ESMCI_VM.h ESMCI_VMKernel.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/VM/src

include ${ESMF_DIR}/makefile

CLEANDIRS  =
CLEANFILES =
CLOBBERDIS =

DIRS	  = 

