# $Id: makefile,v 1.1 2003/03/15 01:01:38 eschwab Exp $

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

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_DELayoutCreateUTest \
#	        $(ESMC_TESTDIR)/ESMC_DELayoutCreateUTest 

TESTS_RUN     = RUN_ESMF_DELayoutCreateUTest \
#		RUN_ESMC_DELayoutCreateUTest 

TESTS_RUN_UNI = RUN_ESMF_DELayoutCreateUTestUNI \
#		RUN_ESMC_DELayoutCreateUTestUNI 

TESTS_RM      = ESMF_DELayoutCreateUTest.rm \
#		ESMC_DELayoutCreateUTest.rm


include ${ESMF_DIR}/makefile

DIRS	  = 

$(ESMC_TESTDIR)/ESMF_DELayoutCreateUTest: chkopts ESMF_DELayoutCreateUTest.o
	-${F90CXXLD} -o $(ESMC_TESTDIR)/ESMF_DELayoutCreateUTest ESMF_DELayoutCreateUTest.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMF_DELayoutCreateUTest.o

RUN_ESMF_DELayoutCreateUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_DELayoutCreateUTest.tmps
	${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_DELayoutCreateUTest > $(ESMC_TESTDIR)/ESMF_DELayoutCreateUTest.stdout

RUN_ESMF_DELayoutCreateUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_DELayoutCreateUTest.tmps
	${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_DELayoutCreateUTest > $(ESMC_TESTDIR)/ESMF_DELayoutCreateUTest.stdout

ESMF_DELayoutCreateUTest.rm:
	echo "ESMF_DELayoutCreateTest.rm target called here."


#$(ESMC_TESTDIR)/ESMC_DELayoutCreateUTest: chkopts ESMC_DELayoutCreateUTest.o
#	-${CXXF90LD} -o $(ESMC_TESTDIR)/ESMC_DELayoutCreateUTest ESMC_DELayoutCreateUTest.o -lesmf ${CXXF90LIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
#	${RM} ESMC_DELayoutCreateUTest.o

#RUN_ESMC_DELayoutCreateUTest:
#	-@rm -f testlog stdlog.* logfile.* ESMC_DELayoutCreateUTest.tmps
#	${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMC_DELayoutCreateUTest > $(ESMC_TESTDIR)/ESMC_DELayoutCreateUTest.stdout

#RUN_ESMC_DELayoutCreateUTestUNI:
#	-@rm -f testlog stdlog.* logfile.* ESMC_DELayoutCreateUTest.tmps
#	${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMC_DELayoutCreateUTest > $(ESMC_TESTDIR)/ESMC_DELayoutCreateUTest.stdout

#ESMC_DELayoutCreateUTest.rm:
#	echo "ESMC_DELayoutCreateTest.rm target called here."



