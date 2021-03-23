# $Id$

ALL: build_here 

SOURCEC	  = ESMCI_GridUtil_F.C
SOURCEF	  = ESMF_GridUtil.F90
SOURCEH	  = 

# list .h files to be copied to higher level common include directory
STOREH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/GridUtil/interface

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

ifeq ($(ESMF_MOAB),internal)
ESMF_CXXCOMPILECPPFLAGS += -I$(ESMF_DIR)/src/Infrastructure/Mesh/src/Moab
endif

DIRS	  = 
