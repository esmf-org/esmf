# $Id: makefile,v 1.32 2008/09/26 05:53:55 cdeluca Exp $

ALL: build_here 

SOURCEC	  = ESMCI_FTable.C
SOURCEF	  = ESMF_Comp.F90 ESMF_GridComp.F90 ESMF_CplComp.F90 
SOURCEH	  = 

# If you have public .h files in this dir, list them here
STOREH    = ESMCI_FTable.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Superstructure/Component/src

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  = 
