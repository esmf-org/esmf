# $Id: makefile,v 1.2 2002/12/06 16:42:02 nscollins Exp $

# leave alone.
ALL: lib 

# don't set these unless absolutely necessary - they've got to be
#  architecture and compiler independent and should be inherited
#  from the upper level makefile if possible.
CFLAGS    =
FFLAGS    =

# if you have C files in this directory, list them all on the line below.
SOURCEC	  =  ESMC_Array.C ESMC_ArrayMap.C
# if you have F90 files in this directory, list them all on the line below.
SOURCEF	  =  ESMF_Alloc_F.F90
# if you have .h include files in this dir, list them all on the line below.
SOURCEH	  = 

# leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix .tex, $(basename $(SOURCEF)))

# is MANSEC something about documentation?
MANSEC	  = Dataobj
LIBBASE	  = libesmf

# leave the following line blank.
DIRS	  = 

# the following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/Data/src

# leave these alone 
CLEANFILES = $(LIBNAME) $(addprefix doc/, $(addsuffix .tex, $(basename $(SOURCEF))))

include ${ESMF_DIR}/build/${ESMF_ARCH}/base


# if you have class files which need to be compiled in a particular
# order, list the dependencies below, with no rules following.
#
# e.g.:
#
# ESMF_Field.o:   ESMF_DataArray.o
#
# ESMF_Bundle.o:  ESMF_Field.o
#




