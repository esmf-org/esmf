# $Id: makefile,v 1.6 2003/05/07 20:44:34 nscollins Exp $

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
LOCDIR	  =  src/Infrastructure/ConfigAttr/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_ConfigUTest
#	        $(ESMC_TESTDIR)/ESMC_ConfigUTest 

TESTS_RUN     = RUN_ESMF_ConfigUTest
#		RUN_ESMC_ConfigUTest 

TESTS_RUN_UNI = RUN_ESMF_ConfigUTestUNI
#		RUN_ESMC_ConfigUTestUNI 

TESTS_RM      = ESMF_ConfigUTest.rm
#                ESMC_ConfigUTest.rm




#TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_ConfigUTest 
#	        $(ESMC_TESTDIR)/ESMC_ConfigUTest 

#TESTS_RUN     = RUN_ESMF_ConfigUTest
#		RUN_ESMC_ConfigUTest 

#TESTS_RUN_UNI = RUN_ESMF_ConfigUTestUNI
#		RUN_ESMC_ConfigUTestUNI 

#TESTS_RM      = ESMF_ConfigUTest.rm
#                ESMC_ConfigUTest.rm


include ${ESMF_DIR}/makefile

DIRS	  = 

$(ESMC_TESTDIR)/ESMF_ConfigUTest: chkopts ESMF_ConfigUTest.o
	-$(SL_F_LINKER) -o $(ESMC_TESTDIR)/ESMF_ConfigUTest ESMF_ConfigUTest.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	${RM} ESMF_ConfigUTest.o

RUN_ESMF_ConfigUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_ConfigUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_ConfigUTest > $(ESMC_TESTDIR)/ESMF_ConfigUTest.stdout

RUN_ESMF_ConfigUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_ConfigUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_ConfigUTest > $(ESMC_TESTDIR)/ESMF_ConfigUTest.stdout

ESMF_ConfigUTest.rm:
	echo "ESMF_ConfigTest.rm target called here."


$(ESMC_TESTDIR)/ESMC_ConfigUTest: chkopts ESMC_ConfigUTest.o
	-$(SL_F_LINKER) -o $(ESMC_TESTDIR)/ESMC_ConfigUTest ESMC_ConfigUTest.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	${RM} ESMC_ConfigUTest.o

RUN_ESMC_ConfigUTest:
	-@rm -f testlog stdlog.* logfile.* ESMC_ConfigUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMC_ConfigUTest > $(ESMC_TESTDIR)/ESMC_ConfigUTest.stdout

RUN_ESMC_ConfigUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMC_ConfigUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMC_ConfigUTest > $(ESMC_TESTDIR)/ESMC_ConfigUTest.stdout

ESMC_ConfigUTest.rm:
	echo "ESMC_ConfigTest.rm target called here."



