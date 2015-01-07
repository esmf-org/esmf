# $Id$

ALL: build_here 

SOURCEC	  = ESMCI_Regrid_F.C
SOURCEF	  =  
SOURCEH	  = 

# list .h files to be copied to higher level common include directory
STOREH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Regrid/interface

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  = 
