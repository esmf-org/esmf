# $Id: makefile,v 1.11 2004/02/05 21:48:19 nscollins Exp $ 

# default rule if no explicit target is given to make
ALL: build_here

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



