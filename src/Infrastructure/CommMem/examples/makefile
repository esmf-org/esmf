# $Id: makefile,v 1.11 2003/03/28 17:24:56 svasquez Exp $

# makefile template for the examples directory.

ALL: ESMF_DELayout_FEx ESMF_DELayout_FEx2 ESMC_CommMemEx

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
EXAMPLESF = ESMF_DELayout_FEx.F90 ESMF_DELayout_FEx2.F90
# fill in the names of the C++ source files for the tests/examples
EXAMPLESC = ESMC_CommMemEx.C

# leave the following lines blank.
MANSEC	  = 
LIBBASE	  = 

# fill in the following with the executable names
TESTEXAMPLES = ESMF_DELayout_FEx ESMF_DELayout_FEx2 ESMC_CommMemEx

# the following line must be set to the current subdir name relative
# to the top level esmf directory. 
LOCDIR	  = src/Infrastructure/CommMem/examples

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod
CLOBBERDIRS =

ESMF_DELayout_FEx: ESMF_DELayout_FEx.o chkopts
	${F90CXXLD} -o ESMF_DELayout_FEx ESMF_DELayout_FEx.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMF_DELayout_FEx.o

ESMF_DELayout_FEx2: ESMF_DELayout_FEx2.o chkopts
	${F90CXXLD} -o ESMF_DELayout_FEx2 ESMF_DELayout_FEx2.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMF_DELayout_FEx2.o

ESMC_CommMemEx: ESMC_CommMemEx.o  chkopts
	${CXXF90LD} -o ESMC_CommMemEx ESMC_CommMemEx.o -lesmf ${CXXF90LIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} -lm
	${RM} ESMC_CommMemEx.o

# if you have dependencies between source files in this dir,
#   list the dependencies below, with no rules following.
#
# e.g.:
#
# ESMF_Field.o:   ESMF_DataArray.o
#
# ESMF_Bundle.o:  ESMF_Field.o
#




