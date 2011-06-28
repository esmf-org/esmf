# $Id: makefile,v 1.35 2011/06/28 04:55:39 theurich Exp $

ALL: build_here 

SOURCEC	  = ESMCI_FTable.C
SOURCEF	  = ESMF_Comp.F90 ESMF_GridComp.F90 ESMF_CplComp.F90 
SOURCEH	  = 

# If you have public .h files in this dir, list them here
STOREH    = ESMCI_FTable.h

# etc files
STOREETC  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Superstructure/Component/src

CLEANDIRS   =
CLEANFILES  = 
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  = 
