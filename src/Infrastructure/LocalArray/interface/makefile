# $Id: makefile,v 1.3 2003/09/30 14:55:07 nscollins Exp $

# leave alone.
ALL: lib 
# if you have C files in this directory, list them all on the line below.
SOURCEC	  = ESMC_LocalArray_F.C 
# if you have F90 files in this directory, list them all on the line below.
SOURCEF	  =  ESMF_LocalArray.F90 ESMF_LocalAlloc_C.F90
# if you have .h include files in this dir, list them all on the line below.
SOURCEH	  = 
# include files which go into the public src dir
STOREH	  = 

# leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = Dataobj
LIBBASE	  = libesmf


# the following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/LocalArray/interface


include $(ESMF_DIR)/makefile

# on the absoft compiler on the mac, files in this dir have to have
# > 16 bit internal jump tables.  see the absoft faq's for more info.
ifeq ($(ESMF_COMPILER),absoft)
FFLAGS += -N11
endif

# leave the following line blank.
DIRS	  = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

# rule for making ESMF_LocalArray.F90 from ESMF_LocalArray_F90.cpp:
# either use the F90 with cpp only, or gcc and ignore the
# warnings about the fortran syntax, depending what is possible on each
# architecture.  this needs to run on each arch.  nsc 10feb03
#
# The rules below work on the mac with gcc and absoft fortran.
# The -P option prevents putting #line directives in the output, and
# -E stops after preprocessing.  The 'tr' command substitutes one-for-one,
# translating @s into newlines to separate multiline macros, and
# also translate ^ into # so that other include files are ready to
# be processed by the second runthru of the preprocessor during the 
# actual compile. (These lines are: ^include "fred" in the
# original source to shield them from the first preprocess pass.)
#
#ESMF_LocalArray.F90: ESMF_LocalArray_F90.cpp
#	gcc -E -P ESMF_LocalArray_F90.cpp | tr "@^" "\n#" > ESMF_LocalArray.F90
#



