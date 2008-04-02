# $Id: makefile,v 1.1.2.1 2008/04/02 20:07:22 cdeluca Exp $ 

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




