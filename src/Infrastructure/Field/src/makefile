# $Id: makefile,v 1.5 2003/06/19 15:47:50 nscollins Exp $ 
ALL: lib 

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  = ESMF_DataMap.F90 ESMF_Field.F90 ESMF_Bundle.F90
SOURCEH	  = 
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
MANSEC	  = Fieldobj
LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Field/src


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =


ESMF_DataMap.o:

ESMF_Field.o:		ESMF_DataMap.o

ESMF_Bundle.o:	        ESMF_Field.o



