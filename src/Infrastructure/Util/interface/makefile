# $Id: makefile,v 1.4 2008/09/02 22:35:22 theurich Exp $ 

ALL: build_here 

SOURCEC	  = ESMC_Util_F.C ESMC_F90Interface_F.C ESMC_Interface.C
SOURCEF	  = ESMF_Util.F90 ESMF_Util_C.F90 ESMF_F90Interface.F90
STOREH    = ESMC_Interface.h
SOURCEH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = Utilobj
LIBBASE	  = libesmf
DIRS	  = 

LOCDIR	  = src/Infrastructure/Util/interface

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =






