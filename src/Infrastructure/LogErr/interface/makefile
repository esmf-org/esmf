# $Id: makefile,v 1.12 2008/09/26 05:18:08 cdeluca Exp $ 

ALL: build_here 

SOURCEC	  = ESMC_LogErr_F.C ESMC_LogErr.C
SOURCEF	  = ESMF_LogErr_C.F90
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
