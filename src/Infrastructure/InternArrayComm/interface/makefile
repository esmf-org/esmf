# $Id: makefile,v 1.3 2008/09/26 05:18:07 cdeluca Exp $

ALL: build_here 

SOURCEC	  = ESMC_InternArrayComm_F.C
SOURCEF	  =  ESMF_InternArrayComm.F90
SOURCEH	  =

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/InternArrayComm/interface

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  = 

