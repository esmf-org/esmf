# $Id: makefile,v 1.2 2003/02/27 17:54:41 flanigan Exp $ 
ALL: lib 

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  = 
SOURCEH	  = 
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix .tex, $(basename $(SOURCEF)))
MANSEC	  = Timeobj
LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/PerfProf/
CLEANFILES = $(LIBNAME) $(addprefix doc/, $(addsuffix .tex, $(basename $(SOURCEF))))

include $(ESMF_DIR)/makefile

DIRS = doc







