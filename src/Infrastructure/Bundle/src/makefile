# $Id: makefile,v 1.7 2004/04/19 22:18:00 nscollins Exp $ 


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

ESMF_BundleGet.o:   ESMF_Bundle.o ESMF_BundleGet.F90

ESMF_BundleGet.F90: ESMF_BundleGet.cpp ESMF_BundleGetMacros.h ESMF_Bundle.o
