# $Id$

ALL: build_here

SOURCEC	  = ESMCI_Trace_F.C ESMC_Trace.C
SOURCEF	  = ESMF_Trace.F90
SOURCEH	  = 

# List all .h files which should be copied to common include dir
STOREH	  = ESMC_Trace.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Trace/interface

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  = 
