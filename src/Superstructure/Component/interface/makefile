# $Id$

ALL: build_here 

SOURCEC	  = ESMCI_Comp.C ESMC_Comp.C
SOURCEF	  = ESMF_Comp_C.F90 
SOURCEH	  = 

# List .h files to be copied to higher level common include directory
STOREH	  =  ESMCI_Comp.h ESMC_GridComp.h ESMC_CplComp.h ESMC_SciComp.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Superstructure/Component/interface

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  = 
