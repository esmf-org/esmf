# $Id: makefile,v 1.5 2008/09/26 05:53:54 cdeluca Exp $

ALL: build_here 

SOURCEC	  = ESMC_InternArray.C 
SOURCEF	  = ESMF_InternArrayDataMap.F90

# If you have .h include files in this dir, list them all on the line below.
STOREH	  = ESMC_InternArray.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/InternArray/src

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  = 

