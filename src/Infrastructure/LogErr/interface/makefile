# $Id: makefile,v 1.8 2004/04/28 20:14:23 cpboulder Exp $ 

ALL: build_here 

CFLAGS    += ${MPI_INCLUDE}
FFLAGS    += ${MPI_INCLUDE}

SOURCEC	  = ESMC_LogErr_F.C
SOURCEF	  =
SOURCEH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = LogErrobj
LIBBASE	  = libesmf

LOCDIR	  = src/Infrastructure/LogErr/interface


include $(ESMF_DIR)/makefile


DIRS	  = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =
