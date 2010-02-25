# $Id: makefile,v 1.1 2010/02/25 19:59:50 oehmke Exp $

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
