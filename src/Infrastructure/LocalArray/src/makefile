# $Id: makefile,v 1.6 2008/09/26 05:53:54 cdeluca Exp $

ALL: build_here 

SOURCEC	  =  ESMC_LocalArray.C 
SOURCEF	  =  

# if you have .h include files in this dir, list them all on the line below.
STOREH	  = ESMC_LocalArray.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/LocalArray/src

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

# leave the following line blank.
DIRS	  = 

