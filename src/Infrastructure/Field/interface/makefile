# $Id: makefile,v 1.10 2007/10/31 16:58:52 cdeluca Exp $ 

ALL: build_here 

CFLAGS    =
FFLAGS    =
SOURCEC	  = ESMC_Field_F.C
SOURCEF	  = 
! TODO:FIELDINTEGRATION Restore ESMF_Field_C.F90
# ESMF_Field_C.F90 
STOREH	  = ESMC_Field.h 

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




