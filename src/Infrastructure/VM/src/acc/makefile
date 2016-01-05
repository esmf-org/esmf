# $Id$

ALL: build_here 

SOURCEC	  = 
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
STOREH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/VM/src/acc

include ${ESMF_DIR}/makefile

ifeq ($(ESMF_ACC_SOFTWARE_STACK),openmp4)
ESMF_CXXCOMPILEOPTS += $(ESMF_OPENMP_CXXCOMPILEOPTS)
ESMF_CXXLINKOPTS    += $(ESMF_OPENMP_CXXLINKOPTS)
endif

CLEANDIRS  =
CLEANFILES =
CLOBBERDIS =

DIRS	  = 

