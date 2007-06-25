# $Id: makefile,v 1.11 2007/06/25 20:15:53 cdeluca Exp $ 

ALL: build_here 

CFLAGS    = 
FFLAGS    = -free
SOURCEC	  = ESMCI_Grid_F.C

SOURCEF	  = ESMF_Grid.F90 ESMF_StaggerLoc.F90

SOURCEH	  = 
STOREH    = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
MANSEC	  = Gridobj
LIBBASE	  = libesmf
DIRS	  = 
LOCDIR	  = src/Infrastructure/Grid/interface

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =


