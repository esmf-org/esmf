# $Id: makefile,v 1.4 2003/03/20 20:33:04 flanigan Exp $ 
ALL: lib 

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  = ESMF_IO.F90
SOURCEH	  = 
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
MANSEC	  = IOobj
LIBBASE	  = libesmf
DIRS	  = 
LOCDIR	  = src/Infrastructure/IO/interface
CLEANFILES = $(LIBNAME) $(addprefix doc/, $(addsuffix .tex, $(basename $(SOURCEF))))

include $(ESMF_DIR)/makefile

DIRS = 





