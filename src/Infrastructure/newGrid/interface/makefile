# $Id: makefile,v 1.2 2007/06/12 21:47:30 cdeluca Exp $ 

ALL: build_here 

CFLAGS    = 
FFLAGS    = -free
SOURCEC	  = 

SOURCEF	  = ESMF_Grid.F90 ESMF_StaggerLoc.F90

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


