# $Id: makefile,v 1.5 2008/09/26 05:53:54 cdeluca Exp $

ALL: build_here

SOURCEC	  =  ESMC_InternArrayComm.C 
SOURCEF	  =  

# If you have .h include files in this dir, list them all on the line below.
STOREH	  = ESMC_InternArrayComm.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/InternArrayComm/src

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  = 

