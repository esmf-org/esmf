# $Id: makefile,v 1.4 2003/04/02 21:02:21 svasquez Exp $

ALL:  tests

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Infrastructure/TimeMgr/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_ClockUTest \
	        $(ESMC_TESTDIR)/ESMC_ClockUTest 

TESTS_RUN     = RUN_ESMF_ClockUTest \
		RUN_ESMC_ClockUTest 

TESTS_RUN_UNI = RUN_ESMF_ClockUTestUNI \
		RUN_ESMC_ClockUTestUNI 

TESTS_RM      = ESMF_ClockUTest.rm \
                ESMC_ClockUTest.rm


include ${ESMF_DIR}/makefile

DIRS	  = 

$(ESMC_TESTDIR)/ESMF_ClockUTest: chkopts ESMF_ClockUTest.o
	-${F90CXXLD} -o $(ESMC_TESTDIR)/ESMF_ClockUTest ESMF_ClockUTest.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMF_ClockUTest.o

RUN_ESMF_ClockUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_ClockUTest.tmps
	${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_ClockUTest > $(ESMC_TESTDIR)/ESMF_ClockUTest.stdout

RUN_ESMF_ClockUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_ClockUTest.tmps
	${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_ClockUTest > $(ESMC_TESTDIR)/ESMF_ClockUTest.stdout

ESMF_ClockUTest.rm:
	echo "ESMF_ClockTest.rm target called here."


$(ESMC_TESTDIR)/ESMC_ClockUTest: chkopts ESMC_ClockUTest.o
	-${CXXF90LD} -o $(ESMC_TESTDIR)/ESMC_ClockUTest ESMC_ClockUTest.o -lesmf ${CXXF90LIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMC_ClockUTest.o

RUN_ESMC_ClockUTest:
	-@rm -f testlog stdlog.* logfile.* ESMC_ClockUTest.tmps
	${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMC_ClockUTest > $(ESMC_TESTDIR)/ESMC_ClockUTest.stdout

RUN_ESMC_ClockUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMC_ClockUTest.tmps
	${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMC_ClockUTest > $(ESMC_TESTDIR)/ESMC_ClockUTest.stdout

ESMC_ClockUTest.rm:
	echo "ESMC_ClockTest.rm target called here."



