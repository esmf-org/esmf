# $Id: makefile,v 1.3 2003/01/21 18:17:02 flanigan Exp $ 
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

include ${ESMF_DIR}/build/${ESMF_ARCH}/base






