# $Id: makefile,v 1.6 2010/06/28 17:59:24 theurich Exp $

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

DIRS	  = 
