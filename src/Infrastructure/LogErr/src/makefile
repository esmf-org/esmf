# $Id: makefile,v 1.9 2004/05/19 11:32:04 nscollins Exp $ 

ALL: build_here 

CFLAGS    += ${MPI_INCLUDE}
FFLAGS    =

SOURCEC	  = ESMC_LogErr.C
SOURCEF	  = ESMF_LogErr.F90
SOURCEH	  = 
STOREH    = ESMF_LogConstants.inc ESMF_LogErr.inc \
            ESMF_LogMacros.inc ESMC_LogErr.h ESMC_LogErr.inc

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = LogErrobj
LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/LogErr/src


include $(ESMF_DIR)/makefile


DIRS	  = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =


