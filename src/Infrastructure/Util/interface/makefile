# $Id$ 

ALL: build_here 

SOURCEC	  = ESMCI_Util_F.C ESMC_Util.C ESMCI_F90Interface_F.C ESMC_Interface.C \
            ESMCI_Fraction_F.C
SOURCEF	  = ESMF_Util.F90 ESMF_F90Interface.F90 ESMF_Fraction.F90
SOURCEH	  = 

# List all .h files which should be copied to common include dir
STOREH    = ESMC_Arg.h ESMC_Interface.h ESMC_Macros.h ESMC_Util.h ESMC_ReturnCodes.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Util/interface

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS = 






