# $Id: makefile,v 1.1.2.2 2010/03/10 06:33:08 oehmke Exp $

ALL: build_here 

SOURCEC	  = ESMCI_GeomBase_F.C
SOURCEF	  = 
SOURCEH	  = 

# list .h files to be copied to higher level common include directory
STOREH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/GeomBase/interface

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  = 
