# $Id: makefile,v 1.11 2008/09/02 22:35:20 theurich Exp $ 

ALL: build_here 

SOURCEC	  = ESMC_LogErr_F.C ESMC_LogErr.C
SOURCEF	  = ESMF_LogErr_C.F90
SOURCEH	  = ESMC_LogErr.h

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
