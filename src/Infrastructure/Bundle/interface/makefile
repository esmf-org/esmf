# $Id: makefile,v 1.4 2007/06/28 22:37:43 cdeluca Exp $ 

ALL: build_here 

CFLAGS    =
FFLAGS    =
SOURCEC	  = ESMC_Bundle_F.C
SOURCEF	  = ESMF_Bundle_C.F90
STOREH	  = ESMC_Bundle.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
MANSEC	  = Bundleobj
LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Bundle/interface


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =




