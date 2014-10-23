# $Id$

ALL: build_here 

SOURCEC	  = ESMCI_VM.C ESMCI_VMKernel.C
SOURCEF	  = 
SOURCEH	  = 

ifeq ($(ESMF_ACC_FRAMEWORK),opencl)
SOURCEC += ESMC_OpenCLInfo.C
endif
ifeq ($(ESMF_ACC_FRAMEWORK),openacc)
SOURCEF += ESMF_OpenACCInfo.F90
endif

# List all .h files which should be copied to common include dir
STOREH	  = ESMCI_VM.h ESMCI_VMKernel.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/VM/src

include ${ESMF_DIR}/makefile

ifeq ($(ESMF_ACC_FRAMEWORK),openacc)
ESMF_F90COMPILEOPTS += $(ESMF_OPENACC_F90COMPILEOPTS)
ESMF_F90LINKOPTS    += $(ESMF_OPENACC_F90LINKOPTS)
ESMF_CXXCOMPILECPPFLAGS += -DESMF_ACC_FW_HAS_FTN_INTERFACE
#ESMF_CXXCOMPILEOPTS += $(ESMF_OPENACC_CXXCOMPILEOPTS)
#ESMF_CXXLINKOPTS    += $(ESMF_OPENACC_CXXLINKOPTS)
endif

CLEANDIRS  =
CLEANFILES =
CLOBBERDIS =

DIRS	  = 

