# $Id: makefile,v 1.5 2003/02/28 01:08:05 eschwab Exp $ 
ALL: lib  install

CFLAGS    =
FFLAGS    =
STOREH    = ESMC_Base.h ESMC_Test.h
SOURCEC	  = ESMC_Base.C ESMC_Test.C
SOURCEF	  = ESMF_Test.F90
SOURCEH	  = 
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix .tex, $(basename $(SOURCEF)))
MANSEC	  = Baseobj
LIBBASE	  = libesmf
DIRS	  = 
LOCDIR	  = src/Infrastructure/Base/src
CLEANFILES = $(LIBNAME) $(addprefix doc/, $(addsuffix .tex, $(basename $(SOURCEF))))

include $(ESMF_DIR)/makefile

DIRS = 


