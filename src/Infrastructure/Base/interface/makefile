# $Id: makefile,v 1.11 2004/06/08 07:20:08 nscollins Exp $ 

ALL: build_here 

CFLAGS    += -I$(ESMF_DIR)/src/Infrastructure/LogErr/include
FFLAGS    =
SOURCEC	  = ESMC_Base_F.C
SOURCEF	  = ESMF_Base.F90 ESMF_Base_C.F90
SOURCEH	  = 
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
MANSEC	  = Baseobj
LIBBASE	  = libesmf
DIRS	  = 
LOCDIR	  = src/Infrastructure/Base/interface

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =






