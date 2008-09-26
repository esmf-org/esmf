# $Id: makefile,v 1.5 2008/09/26 05:18:04 cdeluca Exp $ 

ALL: build_here 

SOURCEC	  = ESMC_Config.C
SOURCEF	  = ESMF_Config_C.F90 
SOURCEH	  =

# List .h files to be copied to higher level common include directory
STOREH	  = ESMC_Config.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Config/interface

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  = 
