# $Id: makefile,v 1.3 2007/06/25 16:13:30 oehmke Exp $ 

ALL: build_here 

CFLAGS    = 
FFLAGS    = -free
SOURCEC	  = ESMCI_Grid_F.C

SOURCEF	  = ESMF_newGrid.F90 ESMF_StaggerLoc.F90

SOURCEH	  = 
STOREH    = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
MANSEC	  = Gridobj
LIBBASE	  = libesmf
DIRS	  = 
LOCDIR	  = src/Infrastructure/newGrid/interface

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =


