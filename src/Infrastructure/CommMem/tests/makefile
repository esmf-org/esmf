# $Id: makefile,v 1.2 2003/03/17 18:05:38 cdeluca Exp $

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
LOCDIR	  = src/Infrastructure/CommMem/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_DELayoutUTest \
	        $(ESMC_TESTDIR)/ESMC_DELayoutUTest 

TESTS_RUN     = RUN_ESMF_DELayoutUTest \
		RUN_ESMC_DELayoutUTest 

TESTS_RUN_UNI = RUN_ESMF_DELayoutUTestUNI \
		RUN_ESMC_DELayoutUTestUNI 

TESTS_RM      = ESMF_DELayoutUTest.rm \
                ESMC_DELayoutUTest.rm


include ${ESMF_DIR}/makefile

DIRS	  = 

$(ESMC_TESTDIR)/ESMF_DELayoutUTest: chkopts ESMF_DELayoutUTest.o
	-${F90CXXLD} -o $(ESMC_TESTDIR)/ESMF_DELayoutUTest ESMF_DELayoutUTest.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMF_DELayoutUTest.o

RUN_ESMF_DELayoutUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_DELayoutUTest.tmps
	${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_DELayoutUTest > $(ESMC_TESTDIR)/ESMF_DELayoutUTest.stdout

RUN_ESMF_DELayoutUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_DELayoutUTest.tmps
	${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_DELayoutUTest > $(ESMC_TESTDIR)/ESMF_DELayoutUTest.stdout

ESMF_DELayoutUTest.rm:
	echo "ESMF_DELayoutTest.rm target called here."


$(ESMC_TESTDIR)/ESMC_DELayoutUTest: chkopts ESMC_DELayoutUTest.o
	-${CXXF90LD} -o $(ESMC_TESTDIR)/ESMC_DELayoutUTest ESMC_DELayoutUTest.o -lesmf ${CXXF90LIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMC_DELayoutUTest.o

RUN_ESMC_DELayoutUTest:
	-@rm -f testlog stdlog.* logfile.* ESMC_DELayoutUTest.tmps
	${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMC_DELayoutUTest > $(ESMC_TESTDIR)/ESMC_DELayoutUTest.stdout

RUN_ESMC_DELayoutUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMC_DELayoutUTest.tmps
	${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMC_DELayoutUTest > $(ESMC_TESTDIR)/ESMC_DELayoutUTest.stdout

ESMC_DELayoutUTest.rm:
	echo "ESMC_DELayoutTest.rm target called here."



