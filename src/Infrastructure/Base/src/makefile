# $Id: makefile,v 1.10 2003/09/19 22:13:43 cdeluca Exp $ 
ALL: lib  install

CFLAGS    =
FFLAGS    =
SOURCEC	  = ESMC_Base.C ESMC_Test.C
SOURCEF	  = ESMF_Test.F90
SOURCEH	  = 
STOREH    = ESMC.h ESMF_Macros.inc ESMF_Version.inc ESMF.h ESMC_Macros.h \
            ESMC_Base.h ESMC_Test.h

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



