# $Id: makefile,v 1.5 2004/08/03 19:33:49 nscollins Exp $

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
LOCDIR	  = src/Infrastructure/FieldDataMap/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_FieldDataMapUTest \
#	        $(ESMC_TESTDIR)/ESMC_FieldDataMapUTest 

TESTS_RUN     = RUN_ESMF_FieldDataMapUTest \
#		RUN_ESMC_FieldDataMapUTest 

TESTS_RUN_UNI = RUN_ESMF_FieldDataMapUTestUNI \
#		RUN_ESMC_FieldDataMapUTestUNI 

TESTS_RM      = ESMF_FieldDataMapUTest.rm \
#                ESMC_FieldDataMapUTest.rm


include ${ESMF_DIR}/makefile

DIRS	  = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt NULL core ESMF_LogFile $(TESTS_BUILD)
CLOBBERDIRS =

#
# Fortran unit test
#
RUN_ESMF_FieldDataMapUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldDataMapUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_FieldDataMapUTest > $(ESMC_TESTDIR)/ESMF_FieldDataMapUTest.stdout

RUN_ESMF_FieldDataMapUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldDataMapUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_FieldDataMapUTest > $(ESMC_TESTDIR)/ESMF_FieldDataMapUTest.stdout

ESMF_FieldDataMapUTest.rm:
	echo "ESMF_FieldDataMapTest.rm target called here."


#
# C++ Unit tests
#
$(ESMC_TESTDIR)/ESMC_FieldDataMapUTest: chkopts ESMC_FieldDataMapUTest.o
	-${CXXF90LD} -o $(ESMC_TESTDIR)/ESMC_FieldDataMapUTest ESMC_FieldDataMapUTest.o -lesmf ${CXXF90LIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} ${NETCDF_LIB} $(SL_LINKOPTS)
	-${RM} ESMC_FieldDataMapUTest.o

RUN_ESMC_FieldDataMapUTest:
	-@rm -f testlog stdlog.* logfile.* ESMC_FieldDataMapUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMC_FieldDataMapUTest > $(ESMC_TESTDIR)/ESMC_FieldDataMapUTest.stdout

RUN_ESMC_FieldDataMapUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMC_FieldDataMapUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMC_FieldDataMapUTest > $(ESMC_TESTDIR)/ESMC_FieldDataMapUTest.stdout

ESMC_FieldDataMapUTest.rm:
	echo "ESMC_FieldDataMapTest.rm target called here."



