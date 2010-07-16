# $Id: makefile,v 1.1 2010/07/16 14:15:08 feiliu Exp $ 

ALL: build_here 

SOURCEC	  = ESMCI_XGrid_F.C
SOURCEF	  = 

# List all .h files which should be copied to a common include dir
STOREH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/XGrid/interface

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  = 
