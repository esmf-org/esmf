# $Id: makefile,v 1.2 2003/04/03 20:05:16 nscollins Exp $ 
ALL: lib 

CFLAGS    = ${MPI_INCLUDE}
FFLAGS    =

SOURCEC	  = ESMC_LogErr.C
SOURCEF	  = 
SOURCEH	  = 
STOREH    = ESMC_LogErr.h ESMC_UtilityFunctions.h ESMF_ErrConstants.inc \
            ESMF_LogConstants.inc ESMF_LogErr.inc ESMF_LogMacros.inc

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


