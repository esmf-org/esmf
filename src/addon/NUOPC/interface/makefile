# $Id$ 

ALL: build_here 

SOURCEC	  = NUOPC.c
SOURCEF	  = NUOPC_c.F90
SOURCEH	  = 

# List all .h files which should be copied to common include dir
STOREH	  = NUOPC.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix _fapi.tex, $(basename $(AUTOGEN)))

LIBBASE	  = libesmf
LOCDIR	  = src/addon/NUOPC/interface

CLEANDIRS   =
CLEANFILES  = $(addprefix ../doc/, $(TEXFILES) )
CLOBBERDIRS =

include $(ESMF_DIR)/makefile

DIRS	  = 

