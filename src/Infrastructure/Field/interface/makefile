# $Id: makefile,v 1.3 2003/03/20 20:32:59 flanigan Exp $ 
ALL: lib 

CFLAGS    =
FFLAGS    =
SOURCEC	  = ESMC_Field.C ESMC_Bundle.C
SOURCEF	  = ESMF_Field_C.F90
STOREH	  = ESMC_Field.h ESMC_Bundle.h

OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
MANSEC	  = Fieldobj
LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Field/interface
CLEANFILES = $(LIBNAME) $(addprefix doc/, $(addsuffix .tex, $(basename $(SOURCEF))))

include $(ESMF_DIR)/makefile

DIRS	  = 




