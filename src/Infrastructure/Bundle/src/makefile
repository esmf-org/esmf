# $Id: makefile,v 1.3 2004/02/05 21:51:17 nscollins Exp $ 


ALL: build_here 

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  = ESMF_Bundle.F90  ESMF_BundleGet.F90
SOURCEH	  = 
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = Bundleobj
LIBBASE	  = libesmf
LOCDIR	  = src/Infrastructure/Bundle/src


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

ESMF_Bundle.o:	        ESMF_Field.o


# The rules below generate the F90 file from the .cpp file
# The -P option prevents putting #line directives in the output, and
# -E stops after preprocessing.  The 'tr' command substitutes one-for-one,
# translating @s into newlines to separate multiline macros, and
# also translate ^ into # so that other include files are ready to
# be processed by the second runthru of the preprocessor during the 
# actual compile. (These lines are: ^include "fred" in the
# original source to shield them from the first preprocess pass.)
#
#ESMF_BundleGet.F90: ESMF_BundleGet_F90.cpp
#       gcc -E -P ESMF_BundleGet_F90.cpp|tr "@^" "\n#" >ESMF_BundleGet.F90
#


