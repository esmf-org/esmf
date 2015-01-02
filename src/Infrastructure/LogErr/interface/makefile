# $Id$ 

ALL: build_here 

SOURCEC	  = ESMCI_LogErr_F.C ESMC_LogErr.C
SOURCEF	  = ESMF_LogErr_C.F90 ESMF_LogPublic.F90
SOURCEH	  = ESMC_LogErr.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/LogErr/interface

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  = 
