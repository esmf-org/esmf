# $Id: makefile,v 1.7 2004/04/21 04:15:45 cpboulder Exp $ 

ALL: build_here 

CFLAGS    += ${MPI_INCLUDE}
FFLAGS    =

SOURCEC	  = ESMC_LogErr.C
SOURCEF	  = ESMF_LogErr.F90
SOURCEH	  = 
STOREH    = ESMF_ErrConstants.inc ESMF_LogConstants.inc\
            ESMF_LogErr.inc ESMF_LogMacros.inc ESMC_LogErr.h\
	    ESMC_LogErr.inc

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


