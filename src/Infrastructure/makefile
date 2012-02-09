# $Id: makefile,v 1.119 2012/02/09 18:40:07 theurich Exp $ 

ALL: build_here

LOCDIR   = src/Infrastructure

include $(ESMF_DIR)/makefile

#TODO: remove the IO/PIO entry below once ESMCI::IO satisfies Array IO needs
DIRS        = stubs Util Base LogErr VM 
ifeq ($(ESMF_PIO),internal)
DIRS       += IO/PIO
endif
DIRS       += TimeMgr DELayout \
              Config ArraySpec LocalArray Route DistGrid Array ArrayBundle \
              IO Grid Mesh GridUtil LocStream XGrid GeomBase Regrid Field \
              Container FieldBundle Attribute 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =



