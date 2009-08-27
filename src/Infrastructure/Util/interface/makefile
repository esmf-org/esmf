# $Id: makefile,v 1.8 2009/08/27 05:34:24 theurich Exp $ 

ALL: build_here 

SOURCEC	  = ESMC_Util_F.C ESMC_F90Interface_F.C ESMC_Interface.C \
            ESMCI_Fraction_F.C
SOURCEF	  = ESMF_Util.F90 ESMF_F90Interface.F90 ESMF_Fraction.F90
SOURCEH	  = 

# List all .h files which should be copied to common include dir
STOREH    = ESMC_Interface.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Util/interface

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS = 






