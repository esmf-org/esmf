# $Id: makefile,v 1.8 2003/04/08 22:47:54 nscollins Exp $ 
ALL: lib  install

CFLAGS    =
FFLAGS    =
STOREH    = ESMC_Base.h ESMC_Test.h
SOURCEC	  = ESMC_Base.C ESMC_Test.C
SOURCEF	  = ESMF_Test.F90
SOURCEH	  = 
STOREH    = ESMC.h ESMF_Macros.inc ESMF_Version.inc ESMF.h ESMC_Macros.h
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
MANSEC	  = Baseobj
LIBBASE	  = libesmf
DIRS	  = 
LOCDIR	  = src/Infrastructure/Base/src

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =



