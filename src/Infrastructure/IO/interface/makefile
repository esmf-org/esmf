# $Id: makefile,v 1.3 2003/02/27 17:54:38 flanigan Exp $ 
ALL: lib 

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  = ESMF_IO.F90
SOURCEH	  = 
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
#TEXFILES  = $(addsuffix .tex, $(basename $(SOURCEF)))
MANSEC	  = IOobj
LIBBASE	  = libesmf
DIRS	  = 
LOCDIR	  = src/Infrastructure/IO/interface
CLEANFILES = $(LIBNAME) $(addprefix doc/, $(addsuffix .tex, $(basename $(SOURCEF))))

include $(ESMF_DIR)/makefile

DIRS = 





