# $Id: makefile,v 1.4 2003/02/27 17:54:47 flanigan Exp $

# leave alone.
ALL: lib 

# don't set these unless absolutely necessary - they've got to be
#  architecture and compiler independent and should be inherited
#  from the upper level makefile if possible.
CFLAGS    =
FFLAGS    =

# if you have C files in this directory, list them all on the line below.
SOURCEC	  = 
# if you have F90 files in this directory, list them all on the line below.
SOURCEF	  = ESMF_PhysGrid.F90
# if you have .h include files in this dir, list them all on the line below.
SOURCEH	  = 

# leave these alone.
OBJSC     = $(addsuffix .o, $(basename $(SOURCEC)))
OBJSF     = $(addsuffix .o, $(basename $(SOURCEF)))
TEXFILES  = $(addsuffix .tex, $(basename $(SOURCEF)))

# is MANSEC something about documentation?
MANSEC	  = PhysGridobj
LIBBASE	  = libesmf


# the following line must be set to the current subdir name relative
# to the top level esmf directory. (e.g. Base/src, TimeMgr/interface, etc)
LOCDIR	  = src/Infrastructure/PhysGrid/src

# leave these alone 
CLEANFILES = $(LIBNAME) $(addprefix doc/, $(addsuffix .tex, $(basename $(SOURCEF))))

include $(ESMF_DIR)/makefile

# leave the following line blank.
DIRS	  = 



# if you have class files which need to be compiled in a particular
# order, list the dependencies below, with no rules following.
#
# e.g.:
#
# ESMF_Field.o:   ESMF_DataArray.o
#
# ESMF_Bundle.o:  ESMF_Field.o
#




