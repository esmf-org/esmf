# $Id: makefile,v 1.2 2008/08/21 19:03:35 w6ws Exp $ 

ALL: build_here 

AUTOGEN	  = ESMF_Attribute.F90
SOURCEC	  = 
SOURCEF	  = $(AUTOGEN)
SOURCEH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = Attrobj
LIBBASE	  = libesmf
DIRS	  = 
LOCDIR	  = src/Superstructure/Attribute_API/interface

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(AUTOGEN)
CLOBBERDIRS =






