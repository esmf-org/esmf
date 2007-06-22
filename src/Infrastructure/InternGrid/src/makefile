# $Id: makefile,v 1.1 2007/06/22 23:21:38 cdeluca Exp $

# leave alone.
ALL: build_here 

# don't set these unless absolutely necessary - they've got to be
#  architecture and compiler independent and should be inherited
#  from the upper level makefile if possible.
CFLAGS    =
FFLAGS    =

# if you have C files in this directory, list them all on the line below.
SOURCEC	  = 
# if you have F90 files in this directory, list them all on the line below.
SOURCEF	  = ESMF_InternGridTypes.F90 ESMF_LogRectInternGrid.F90 ESMF_InternGrid.F90
# if you have .h include files in this dir, list them all on the line below.
STOREH	  = ESMC_InternGrid.h

# leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = InternGridobj
LIBBASE	  = libesmf



# the following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/InternGrid/src


include $(ESMF_DIR)/makefile

# this section must be *after* the line above, and it only seems to apply
# to absoft on mac os x, the 8.2 version.  it incorrectly optimizes
# ESMF_LogRectInternGrid.F90, referencing a subroutine argument which is not
# present (and is inside an "if (present())" statement.  if you are
# compiling with absoft 9+, you can comment out this code if you want
# the default level of optimization (which is -O2).
ifeq ($(ESMF_COMPILER),absoft)
 ifeq ($(ESMF_OS),Darwin)
  ifeq ($(ESMF_BOPT),O)
   ESMF_F90OPTFLAG_O = -O1
  endif
 endif
endif


# leave the following line blank.
DIRS	  = 

CLEANDIRS   =
CLEANFILES  =
CLOBBERDIRS =

# if you have class files which need to be compiled in a particular
# order, list the dependencies below, with no rules following.
#
# e.g.:
#
# ESMF_Field.o:   ESMF_DataArray.o
#
# ESMF_Bundle.o:  ESMF_Field.o
#




