# $Id: makefile,v 1.4 2008/09/26 05:18:06 cdeluca Exp $

ALL: build_here 

SOURCEC	  = ESMCI_GridUtil_F.C ESMCI_GeomBase_F.C
SOURCEF	  =  ESMF_GridUtil.F90
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

DIRS	  = 
