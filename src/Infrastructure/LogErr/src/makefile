# $Id: makefile,v 1.6 2004/03/24 17:49:54 nscollins Exp $ 

ALL: build_here 

CFLAGS    += ${MPI_INCLUDE}
FFLAGS    =

SOURCEC	  = 
SOURCEF	  = ESMF_LogErr.F90
SOURCEH	  = 
STOREH    = ESMF_ErrConstants.inc ESMF_LogConstants.inc\
            ESMF_LogErr.inc ESMF_LogMacros.inc

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


