# $Id: makefile,v 1.7 2003/04/15 20:20:13 svasquez Exp $

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

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_DELayoutUTest

TESTS_RUN     = RUN_ESMF_DELayoutUTest

TESTS_RUN_UNI = RUN_ESMF_DELayoutUTestUNI

TESTS_RM      = ESMF_DELayoutUTest.rm


include ${ESMF_DIR}/makefile

DIRS	  = 

$(ESMC_TESTDIR)/ESMF_DELayoutUTest: chkopts ESMF_DELayoutUTest.o
	-$(SL_F_LINKER) -o $(ESMC_TESTDIR)/ESMF_DELayoutUTest ESMF_DELayoutUTest.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	${RM} ESMF_DELayoutUTest.o

RUN_ESMF_DELayoutUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_DELayoutUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_DELayoutUTest > $(ESMC_TESTDIR)/ESMF_DELayoutUTest.stdout

RUN_ESMF_DELayoutUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_DELayoutUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_DELayoutUTest > $(ESMC_TESTDIR)/ESMF_DELayoutUTest.stdout

ESMF_DELayoutUTest.rm:
	echo "ESMF_DELayoutTest.rm target called here."



