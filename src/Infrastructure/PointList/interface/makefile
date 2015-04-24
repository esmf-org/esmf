# $Id$

ALL: build_here 

SOURCEC	  = ESMCI_PointList_F.C
SOURCEF	  = ESMF_PointList.F90
SOURCEH	  = 

# list all .h files which should be copied to common include dir
STOREH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/PointList/interface

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include ${ESMF_DIR}/makefile

DIRS =

