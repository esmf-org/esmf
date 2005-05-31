# $Id: makefile,v 1.1 2005/05/31 17:27:20 nscollins Exp $ 

ALL: build_here 

# add the logerr include dir, since the logerr code has to be compiled 
# before the Base class so base routines can use it.
CFLAGS    += -I$(ESMF_DIR)/src/Infrastructure/LogErr/include ${MPI_INCLUDE}
FFLAGS    =

SOURCEC	  = ESMC_Util_F.C
SOURCEF	  = ESMF_Util.F90 ESMF_Util_C.F90
SOURCEH	  = 

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = Utilobj
LIBBASE	  = libesmf
DIRS	  = 

LOCDIR	  = src/Infrastructure/Util/interface

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =






