# $Id: makefile,v 1.1 2003/03/10 21:54:22 cdeluca Exp $ 
ALL: lib 

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  = ESMF_DataMap.F90 ESMF_Field.F90 ESMF_Bundle.F90
SOURCEH	  = 
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix .tex, $(basename $(SOURCEF)))
MANSEC	  = Fieldobj
LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Fields/src
CLEANFILES = $(LIBNAME) $(addprefix doc/, $(addsuffix .tex, $(basename $(SOURCEF))))

include $(ESMF_DIR)/makefile

DIRS	  = 


#ESMF_Array.o:	
#ESMF_DataMap.o:		ESMF_Array.o

ESMF_DataMap.o:

ESMF_Field.o:		ESMF_DataMap.o

ESMF_Bundle.o:	        ESMF_Field.o



