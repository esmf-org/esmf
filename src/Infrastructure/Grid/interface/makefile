# $Id$ 

ALL: build_here 

SOURCEC	  = ESMCI_Grid_F.C ESMC_Grid.C
SOURCEF	  = ESMF_StaggerLoc.F90 ESMF_Grid.F90 ESMF_Grid_C.F90
SOURCEH	  = 

# List all .h files which should be copied to common include dir 
STOREH    = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Grid/interface

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS = 


