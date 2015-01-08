# $Id$

ALL: build_here 

SOURCEC	  = ESMCI_VM.C ESMCI_VMKernel.C
SOURCEF	  = 
SOURCEH	  = 

ifeq ($(ESMF_ACC_SOFTWARE_STACK),opencl)
SOURCEC += ESMCI_OpenCLInfo.C
endif
ifeq ($(ESMF_ACC_SOFTWARE_STACK),openacc)
SOURCEC += ESMCI_OpenACCInfo.C
endif
ifeq ($(ESMF_ACC_SOFTWARE_STACK),intelmic)
SOURCEC += ESMCI_IntelMICInfo.C
endif
ifeq ($(ESMF_ACC_SOFTWARE_STACK),openmp4)
SOURCEC += ESMCI_OpenMP4Info.C
endif

# List all .h files which should be copied to common include dir
STOREH	  = ESMCI_VM.h ESMCI_VMKernel.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/VM/src

include ${ESMF_DIR}/makefile

# FIXME: Since we are modifying the compile/link flags the
# acc framework code needs to move to a separate directory
ifeq ($(ESMF_ACC_SOFTWARE_STACK),openacc)
#ESMF_F90COMPILEOPTS += $(ESMF_OPENACC_F90COMPILEOPTS)
#ESMF_F90LINKOPTS    += $(ESMF_OPENACC_F90LINKOPTS)
#ESMF_CXXCOMPILECPPFLAGS += -DESMF_ACC_FW_HAS_FTN_INTERFACE
#ESMF_CXXCOMPILEOPTS += $(ESMF_OPENACC_CXXCOMPILEOPTS)
#ESMF_CXXLINKOPTS    += $(ESMF_OPENACC_CXXLINKOPTS)
endif

ifeq ($(ESMF_ACC_SOFTWARE_STACK),intelmic)
#ESMF_CXXCOMPILECPPFLAGS += -DESMF_ACC_FW_HAS_FTN_INTERFACE
endif

ifeq ($(ESMF_ACC_SOFTWARE_STACK),openmp4)
ESMF_CXXCOMPILEOPTS += $(ESMF_OPENMP_CXXCOMPILEOPTS)
ESMF_CXXLINKOPTS    += $(ESMF_OPENMP_CXXLINKOPTS)
endif

CLEANDIRS  =
CLEANFILES =
CLOBBERDIS =

DIRS	  = 

