# $Id: makefile,v 1.2 2003/02/27 17:54:46 flanigan Exp $

# makefile template for the examples directory.

ALL: 

# don't set these unless absolutely necessary - they've got to be
#  architecture and compiler independent and should be inherited
#  from the upper level makefile if possible.
CFLAGS    =
FFLAGS    =

# in general, leave these blank at this point.
SOURCEC	  = 
SOURCEF	  = 
SOURCEH	  = 
OBJSC     = 
OBJSF     = 
TEXFILES  = 

# fill in the names of the F90 source files for the tests/examples
EXAMPLESF =
# fill in the names of the C++ source files for the tests/examples
EXAMPLESC =

# leave the following lines blank.
MANSEC	  = 
LIBBASE	  = 

# fill in the following with the executable names
TESTEXAMPLES =

# the following line must be set to the current subdir name relative
# to the top level esmf directory. 
LOCDIR	  = src/Infrastructure/PhysGrid/examples

# leave these alone 
CLEANFILES = $(LIBNAME) $(addprefix doc/, $(addsuffix .tex, $(basename $(SOURCEF))))

include $(ESMF_DIR)/makefile

DIRS	  = 


# if you have dependencies between source files in this dir,
#   list the dependencies below, with no rules following.
#
# e.g.:
#
# ESMF_Field.o:   ESMF_DataArray.o
#
# ESMF_Bundle.o:  ESMF_Field.o
#




