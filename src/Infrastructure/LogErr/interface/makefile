# $Id: makefile,v 1.9 2005/06/28 19:54:47 nscollins Exp $ 

ALL: build_here 

CFLAGS    += ${MPI_INCLUDE}
FFLAGS    += ${MPI_INCLUDE}

SOURCEC	  = ESMC_LogErr_F.C
SOURCEF	  = ESMF_LogErr_C.F90
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
