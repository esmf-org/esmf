# $Id: makefile,v 1.2 2003/01/21 18:17:03 flanigan Exp $ 
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

include ${ESMF_DIR}/build/${ESMF_ARCH}/base






