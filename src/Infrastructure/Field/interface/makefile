# $Id: makefile,v 1.5 2003/03/25 17:39:43 nscollins Exp $ 
ALL: lib 

CFLAGS    =
FFLAGS    =
SOURCEC	  = ESMC_Field.C ESMC_Bundle.C
SOURCEF	  = ESMF_Field_C.F90 ESMF_Bundle_C.F90
STOREH	  = ESMC_Field.h ESMC_Bundle.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
MANSEC	  = Fieldobj
LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Field/interface


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =




