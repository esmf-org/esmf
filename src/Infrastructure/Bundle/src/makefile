# $Id: makefile,v 1.5 2004/03/23 18:30:30 nscollins Exp $ 


ALL: build_here 

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  = ESMF_Bundle.F90  ESMF_BundleGet.F90
SOURCEH	  = 
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = Bundleobj
LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Bundle/src


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   =
CLEANFILES  = ESMF_BundleGet.F90
CLOBBERDIRS =

ESMF_Bundle.o:	        ESMF_Field.o

ESMF_BundleGet.o:   ESMF_BundleGet.cpp ESMF_BundleMacros.h ESMF_Bundle.o

