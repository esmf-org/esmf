# $Id: makefile,v 1.2 2004/02/05 21:53:55 nscollins Exp $

# Leave alone.
ALL: lib 

# Don't set these unless absolutely necessary - they've got to be
# architecture and compiler independent and should be inherited
# from the upper level makefile if possible.
CFLAGS    = ${MPI_INCLUDE}
FFLAGS    =

# If you have C files in this directory, list them all on the line below.
SOURCEC	  = 
# If you have F90 files in this directory, list them all on the line below.
SOURCEF	  = ESMF_State.F90  ESMF_StateGet.F90
# If you have .h include files in this dir, list them all on the line below.
SOURCEH	  = 
# If you have public .h files in this dir, list them here
STOREH    =

# Leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = Stateobj
LIBBASE	  = libesmf


# The following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Superstructure/State/src

include $(ESMF_DIR)/makefile

# leave the following line blank.
DIRS	  = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

# The rules below work on the mac with gcc and absoft fortran.
# The -P option prevents putting #line directives in the output, and
# -E stops after preprocessing.  The 'tr' command substitutes one-for-one,
# translating @s into newlines to separate multiline macros, and
# also translate ^ into # so that other include files are ready to
# be processed by the second runthru of the preprocessor during the 
# actual compile. (These lines are: ^include "fred" in the
# original source to shield them from the first preprocess pass.)
#
#ESMF_StateGet.F90: ESMF_StateGet_F90.cpp
#       gcc -E -P ESMF_StateGet_F90.cpp|tr "@^" "\n#" >ESMF_StateGet.F90
#

