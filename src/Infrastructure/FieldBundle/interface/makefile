# $Id: makefile,v 1.2 2008/04/02 20:42:54 cdeluca Exp $ 

ALL: build_here 

CFLAGS    =
FFLAGS    =
SOURCEC	  = ESMC_FieldBundle_F.C
SOURCEF	  = ESMF_FieldBundle_C.F90
STOREH	  = ESMC_FieldBundle.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
MANSEC	  = FieldBundleobj
LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/FieldBundle/interface


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =




