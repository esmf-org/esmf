# $Id: makefile,v 1.1 2003/03/28 23:04:23 nscollins Exp $ 
ALL: lib 

CFLAGS    = ${MPI_INCLUDE}
FFLAGS    =
SOURCEC	  = ESMC_LogErr.C
SOURCEF	  = 
SOURCEH	  = 
STOREH    = ESMC_LogErr.h
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


