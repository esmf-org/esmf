# $Id: makefile,v 1.13 2008/07/30 11:29:51 rosalind Exp $ 

ALL: build_here 

CFLAGS    += ${MPI_INCLUDE}
FFLAGS    =

SOURCEC	  = ESMCI_LogErr.C
SOURCEF	  = 
SOURCEH	  = 
STOREH    = ESMC_LogErr.h ESMCI_LogErr.h ESMC_LogErr.inc

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


