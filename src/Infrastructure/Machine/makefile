# $Id: makefile,v 1.1 2001/11/13 18:35:47 dneckels Exp $ 
ALL: lib 

CFLAGS    =
FFLAGS    =
SOURCEC	  = ESMC_Machine.c ESMC_MachineF.c
SOURCEF	  = ESMF_MachineMod.F
SOURCEH	  = ESMC_Machine.h
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix .tex, $(basename $(SOURCEF)))
MANSEC	  = Timeobj
LIBBASE	  = libesmf
DIRS	  = doc
LOCDIR	  = src/Infrastructure/Machine/

include ${ESMF_DIR}/build/${ESMF_ARCH}/base






