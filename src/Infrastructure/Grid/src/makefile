# $Id: makefile,v 1.13 2004/10/26 21:46:09 nscollins Exp $

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
SOURCEF	  = ESMF_GridTypes.F90 ESMF_LogRectGrid.F90 ESMF_Grid.F90
# if you have .h include files in this dir, list them all on the line below.
STOREH	  = ESMC_Grid.h

# leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))

MANSEC	  = Gridobj
LIBBASE	  = libesmf



# the following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/Grid/src


include $(ESMF_DIR)/makefile

# this section must be *after* the line above, and it only seems to apply
# to absoft on mac os x, the 8.2 version.  it incorrectly optimizes
# ESMF_LogRectGrid.F90, referencing a subroutine argument which is not
# present (and is inside an "if (present())" statement.  if you are
# compiling with absoft 9+, you can comment out this code if you want
# the default level of optimization (which is -O2).
ifeq ($(ESMF_COMPILER),absoft)
 ifeq ($(ESMF_ARCH),Darwin)
  ifeq ($(ESMF_BOPT),O)
   O_FOPTFLAGS += -O1
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




