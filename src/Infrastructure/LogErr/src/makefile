# $Id: makefile,v 1.4 2004/02/13 15:37:08 nscollins Exp $ 

ALL: build_here 

CFLAGS    += ${MPI_INCLUDE}
FFLAGS    =

SOURCEC	  = ESMC_LogErr.C
SOURCEF	  = ESMF_LogErr_C.F90
SOURCEH	  = 
STOREH    = ESMC_LogErr.h ESMC_UtilityFunctions.h ESMF_ErrConstants.inc \
            ESMF_LogConstants.inc ESMF_LogErr.inc ESMF_LogMacros.inc

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = LogErrobj
LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/LogErr/src


include $(ESMF_DIR)/makefile

ESMC_LogErr.o: ESMF_LogErr_C.o

DIRS	  = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =


