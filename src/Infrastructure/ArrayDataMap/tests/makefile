# $Id: makefile,v 1.3 2004/06/11 18:45:04 svasquez Exp $

ALL:  build_tests

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Infrastructure/ArrayDataMap/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_ArrayDataMapUTest \
	        $(ESMC_TESTDIR)/ESMC_ArrayDataMapUTest 

TESTS_RUN     = RUN_ESMF_ArrayDataMapUTest \
		RUN_ESMC_ArrayDataMapUTest 

TESTS_RUN_UNI = RUN_ESMF_ArrayDataMapUTestUNI \
		RUN_ESMC_ArrayDataMapUTestUNI 

TESTS_RM      = ESMF_ArrayDataMapUTest.rm \
                ESMC_ArrayDataMapUTest.rm


include ${ESMF_DIR}/makefile

DIRS	  = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt NULL core $(TESTS_BUILD)
CLOBBERDIRS =

#
# Fortran unit test
#
RUN_ESMF_ArrayDataMapUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_ArrayDataMapUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_ArrayDataMapUTest > $(ESMC_TESTDIR)/ESMF_ArrayDataMapUTest.stdout

RUN_ESMF_ArrayDataMapUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_ArrayDataMapUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_ArrayDataMapUTest > $(ESMC_TESTDIR)/ESMF_ArrayDataMapUTest.stdout

ESMF_ArrayDataMapUTest.rm:
	echo "ESMF_ArrayDataMapTest.rm target called here."


#
# C++ Unit tests
#
$(ESMC_TESTDIR)/ESMC_ArrayDataMapUTest: chkopts ESMC_ArrayDataMapUTest.o
	-${CXXF90LD} -o $(ESMC_TESTDIR)/ESMC_ArrayDataMapUTest ESMC_ArrayDataMapUTest.o -lesmf ${CXXF90LIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} ${NETCDF_LIB} $(SL_LINKOPTS)
	${RM} ESMC_ArrayDataMapUTest.o

RUN_ESMC_ArrayDataMapUTest:
	-@rm -f testlog stdlog.* logfile.* ESMC_ArrayDataMapUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMC_ArrayDataMapUTest > $(ESMC_TESTDIR)/ESMC_ArrayDataMapUTest.stdout

RUN_ESMC_ArrayDataMapUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMC_ArrayDataMapUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMC_ArrayDataMapUTest > $(ESMC_TESTDIR)/ESMC_ArrayDataMapUTest.stdout

ESMC_ArrayDataMapUTest.rm:
	echo "ESMC_ArrayDataMapTest.rm target called here."



