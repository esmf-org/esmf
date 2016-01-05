# $Id$ 

ALL: build_here 

SOURCEC	  = ESMCI_LocStream_F.C ESMCI_LocStream.C ESMC_LocStream.C
SOURCEF	  = ESMF_LocStream_C.F90

# List all .h files which should be copied to common include dir
STOREH	  = ESMC_LocStream.h ESMCI_LocStream.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/LocStream/interface

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  = 




