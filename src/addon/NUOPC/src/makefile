# $Id: makefile,v 1.1 2011/04/13 23:12:16 theurich Exp $ 

ALL: build_here 

SOURCEC	  = 
SOURCEF	  = NUOPC.F90 \
            NUOPC_ModelExplicit.F90 \
            NUOPC_DriverExplicit.F90 NUOPC_DriverExplicitAtmOcn.F90 \
            NUOPC_Connector.F90
SOURCEH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix _fapi.tex, $(basename $(AUTOGEN)))

LIBBASE	  = libesmf
LOCDIR	  = src/addon/NUOPC/src

CLEANDIRS   =
CLEANFILES  = $(addprefix ../doc/, $(TEXFILES) )
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  = 

