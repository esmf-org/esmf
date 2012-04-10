# $Id: makefile,v 1.5 2012/04/10 17:35:16 theurich Exp $ 

ALL: build_here 

SOURCEC	  = 
SOURCEF	  = NUOPC_FieldDictionaryDef.F90 NUOPC_Base.F90 \
            NUOPC_RunSequenceDef.F90 NUOPC.F90 \
            NUOPC_ModelExplicitBase.F90 NUOPC_ModelExplicit.F90 \
            NUOPC_MediatorExplicit.F90 \
            NUOPC_Driver.F90 \
            NUOPC_DriverExplicit.F90 NUOPC_DriverExplicitAtmOcn.F90 \
            NUOPC_DriverExplicitAtmOcnMed.F90 NUOPC_Connector.F90
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

