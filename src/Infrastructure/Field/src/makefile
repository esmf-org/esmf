# $Id: makefile,v 1.9 2004/02/05 21:50:12 nscollins Exp $ 

#
ALL: build_here 

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  = ESMF_Field.F90 ESMF_FieldGet.F90
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


ESMF_Bundle.o:	        ESMF_Field.o

# The rules below generate a value Fortran file using cpp.
# The -P option prevents putting #line directives in the output, and
# -E stops after preprocessing.  The 'tr' command substitutes one-for-one,
# translating @s into newlines to separate multiline macros, and
# also translate ^ into # so that other include files are ready to
# be processed by the second runthru of the preprocessor during the 
# actual compile. (These lines are: ^include "fred" in the
# original source to shield them from the first preprocess pass.)
#
#ESMF_FieldGet.F90: ESMF_FieldGet_F90.cpp
#       gcc -E -P ESMF_FieldGet_F90.cpp|tr "@^" "\n#" >ESMF_FieldGet.F90
#




