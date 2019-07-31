# $Id$ 

ALL: build_here

LOCDIR   = src/Superstructure

include $(ESMF_DIR)/makefile

ifeq ($(ESMF_MAPPER_BUILD),ON)
DIRS     =  State Component Mapper AttachMethods AttributeAPI TraceAPI WebServices IOAPI PreESMFMod ESMFMod AppDriver
else
DIRS     =  State Component AttachMethods AttributeAPI TraceAPI WebServices IOAPI PreESMFMod ESMFMod AppDriver
endif

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =










