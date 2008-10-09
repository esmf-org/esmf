# $Id: makefile,v 1.2 2008/10/09 17:01:02 rokuingh Exp $ 

ALL: build_here 

AUTOGEN	  = ESMF_Attribute.F90
SOURCEC	  = 
SOURCEF	  = $(AUTOGEN)
SOURCEH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Superstructure/AttributeAPI/interface

CLEANDIRS   =
CLEANFILES  = $(AUTOGEN)
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS = 






