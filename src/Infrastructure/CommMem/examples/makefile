# $Id: makefile,v 1.5 2003/01/10 21:12:45 eschwab Exp $

# makefile template for the examples directory.

ALL: ESMF_Layout_FEx

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
EXAMPLESF = ESMF_Layout_FEx.F90
# fill in the names of the C++ source files for the tests/examples
EXAMPLESC = 

# leave the following lines blank.
MANSEC	  = 
LIBBASE	  = 
DIRS	  = 

# fill in the following with the executable names
TESTEXAMPLES = ESMF_Layout_FEx

# the following line must be set to the current subdir name relative
# to the top level esmf directory. 
LOCDIR	  = src/Infrastructure/CommMem/examples

# leave these alone 
CLEANFILES = $(ESMC_MODDIR)/ESMF_Layout_FEx $(LIBNAME) $(addprefix doc/, $(addsuffix .tex, $(basename $(SOURCEF))))

include ${ESMF_DIR}/build/${ESMF_ARCH}/base

ESMF_Layout_FEx: ESMF_Layout_FEx.o chkopts
	-${F90CXXLD} -o ESMF_Layout_FEx ESMF_Layout_FEx.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} -lpthread
	${RM} ESMF_Layout_FEx.o

# if you have dependencies between source files in this dir,
#   list the dependencies below, with no rules following.
#
# e.g.:
#
# ESMF_Field.o:   ESMF_DataArray.o
#
# ESMF_Bundle.o:  ESMF_Field.o
#




