# $Id: makefile,v 1.2 2002/10/22 19:28:26 nscollins Exp $ 
ALL: lib 

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  = ESMF_PhysGrid.F90
SOURCEH	  = 
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix .tex, $(basename $(SOURCEF)))
MANSEC	  = PhysGridobj
LIBBASE	  = libesmf
DIRS	  = 
LOCDIR	  = src/Infrastructure/PhysGrid/src
CLEANFILES = $(LIBNAME) $(addprefix doc/, $(addsuffix .tex, $(basename $(SOURCEF))))

include ${ESMF_DIR}/build/${ESMF_ARCH}/base






