# $Id: makefile,v 1.5 2004/03/24 17:36:30 cpboulder Exp $ 

ALL: build_here 

CFLAGS    += ${MPI_INCLUDE}
FFLAGS    += ${MPI_INCLUDE}

SOURCEC	  = 
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

ESMF_LogErr.o:	makefile
