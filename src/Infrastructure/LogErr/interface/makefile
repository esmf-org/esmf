# $Id: makefile,v 1.6 2004/03/24 17:44:47 cpboulder Exp $ 

ALL: build_here 

CFLAGS    += ${MPI_INCLUDE}
FFLAGS    += ${MPI_INCLUDE}

SOURCEC	  = 
SOURCEF	  = ESMF_LogErr.F90
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

ESMF_LogErr.o:	makefile
