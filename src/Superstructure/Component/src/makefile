# $Id$

ALL: build_here 

SOURCEC	  = ESMCI_FTable.C ESMCI_MethodTable.C ESMCI_CompTunnel.C
SOURCEF	  = ESMF_Comp.F90 ESMF_GridComp.F90 ESMF_CplComp.F90 ESMF_SciComp.F90
SOURCEH	  = 

# If you have public .h files in this dir, list them here
STOREH    = ESMCI_FTable.h ESMCI_MethodTable.h ESMCI_CompTunnel.h

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
