# $Id: makefile,v 1.2 2003/04/15 18:07:32 nscollins Exp $ 
ALL: lib 

CFLAGS    = ${MPI_INCLUDE}
FFLAGS    = ${MPI_INCLUDE}
SOURCEC	  = ESMC_LogErrInterface.C
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


