# $Id: makefile,v 1.4 2010/06/23 09:12:13 theurich Exp $ 

ALL: build_here 

SOURCEC	  = ESMCI_IOSpec.C
SOURCEF	  = 
SOURCEH	  =

# List all .h files which should be copied to common include dir
STOREH    = ESMCI_IOSpec.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/IOSpec/interface

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS = 




