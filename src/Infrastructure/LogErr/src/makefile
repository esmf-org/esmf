# $Id: makefile,v 1.14 2008/09/02 22:35:21 theurich Exp $ 

ALL: build_here 

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


