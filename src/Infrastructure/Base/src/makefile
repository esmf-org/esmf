# $Id: makefile,v 1.4 2003/02/27 17:54:11 flanigan Exp $ 
ALL: lib  install

CFLAGS    =
FFLAGS    =
STOREH    = ESMC_Base.h
SOURCEC	  = ESMC_Base.C
SOURCEF	  = 
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


