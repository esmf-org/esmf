# $Id: makefile,v 1.2 2011/04/25 19:32:33 theurich Exp $ 

ALL: build_here 

SOURCEC	  = 
SOURCEF	  = NUOPC.F90 \
            NUOPC_ModelExplicitBase.F90 NUOPC_ModelExplicit.F90 \
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

