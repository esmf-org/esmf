# $Id: makefile,v 1.2 2003/01/08 21:26:43 jwolfe Exp $

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
EXAMPLESF = ESMF_GridCreateEx.F90
# fill in the names of the C++ source files for the tests/examples
EXAMPLESC =

# leave the following lines blank.
MANSEC	  = 
LIBBASE	  = 
DIRS	  = 

# fill in the following with the executable names
TESTEXAMPLES_3 = \
	ESMF_GridCreateEx RUN_ESMF_GridCreateEx ESMF_GridCreateEx.rm

TESTEXAMPLES_9 = \
	ESMF_GridCreateEx RUN_ESMF_GridCreateExUNI ESMF_GridCreateEx.rm

# the following line must be set to the current subdir name relative
# to the top level esmf directory. 
LOCDIR	  = src/Infrastructure/Grid/examples

# leave these alone 
CLEANFILES = $(LIBNAME) $(addprefix doc/, $(addsuffix .tex, $(basename $(SOURCEF))))

include ${ESMF_DIR}/build/${ESMF_ARCH}/base

ESMF_GridCreateDir:
	mkdir -p ${ESMC_TESTDIR}

ESMF_GridCreateEx: ESMF_GridCreateEx.o  chkopts
	-${F90CXXLD} -o ESMF_GridCreateEx ESMF_GridCreateEx.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMF_GridCreateEx.o

RUN_ESMF_GridCreateEx:
	-@rm -f testlog stdlog.* logfile.* ESMF_GridCreateEx.tmps
	${MPIRUN} -np 4 ESMF_GridCreateEx > ESMF_GridCreateEx.stdout
	#@sort -t, -k2 testlog | cut -d, -f4- > ESMF_GridCreateEx.tmp
	#@if (diff ESMF_GridCreateEx.tmp ${ESMF_DIR}/${LOCDIR}/output/ESMF_GridCreateEx.out) then \
	#echo "PASS GridCreateEx test" > ${ESMC_TESTDIR}/ESMF_GridCreateEx.log; else \
	#echo "FAIL GridCreateEx test" > ${ESMC_TESTDIR}/ESMF_GridCreateEx.log; fi
	#@rm -f testlog stdlog.* logfile.* ESMF_GridCreateEx.tmp optlog

RUN_ESMF_GridCreateExUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_GridCreateEx.tmps
	${MPIRUN} -np 1 ESMF_GridCreateEx > ESMF_GridCreateEx.stdout
	#@sort -t, -k2 testlog | cut -d, -f4- > ESMF_GridCreateEx.tmp
	#@if (diff ESMF_GridCreateEx.tmp ${ESMF_DIR}/${LOCDIR}/output/ESMF_GridCreateEx.outuni) then \
	#echo "PASS GridCreateEx test" > ${ESMC_TESTDIR}/ESMF_GridCreateEx.log; else \
	#echo "FAIL GridCreateEx test" > ${ESMC_TESTDIR}/ESMF_GridCreateEx.log; fi
	#@rm -f testlog stdlog.* logfile.* ESMF_GridCreateEx.tmp optlog

ESMF_GridCreateEx.rm:
	echo "ESMF_GridCreateEx.rm target called here."

# if you have dependencies between source files in this dir,
#   list the dependencies below, with no rules following.
#




