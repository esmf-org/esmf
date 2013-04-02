# $Id$

ALL: build_here 

SOURCEC	  = ESMCI_Test.C ESMC_Test.C
SOURCEF	  = ESMF_Test.F90 ESMF_ComplianceIC.F90

# list all .h files which should be copied to common include dir
STOREH	  = ESMCI_Test.h ESMC_Test.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/epilogue/src

CLEANDIRS  =
CLEANFILES =
CLOBBERDIS =

include ${ESMF_DIR}/makefile

DIRS	  = 

