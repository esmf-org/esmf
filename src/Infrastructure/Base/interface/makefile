# $Id: makefile,v 1.4 2003/02/27 17:54:10 flanigan Exp $ 
ALL: lib 

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  = ESMF_Base.F90
SOURCEH	  = 
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
#TEXFILES  = $(addsuffix .tex, $(basename $(SOURCEF)))
MANSEC	  = Baseobj
LIBBASE	  = libesmf
DIRS	  = 
LOCDIR	  = src/Infrastructure/Base/interface
CLEANFILES = $(LIBNAME) $(addprefix doc/, $(addsuffix .tex, $(basename $(SOURCEF))))

include $(ESMF_DIR)/makefile

DIRS = 






