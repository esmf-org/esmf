# $Id: makefile,v 1.4 2004/06/11 19:06:08 svasquez Exp $

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
LOCDIR	  = src/Superstructure/ESMFMod/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_ESMFModUTest \
	        $(ESMC_TESTDIR)/ESMC_ESMFModUTest 

TESTS_RUN     = RUN_ESMF_ESMFModUTest \
		RUN_ESMC_ESMFModUTest 

TESTS_RUN_UNI = RUN_ESMF_ESMFModUTestUNI \
		RUN_ESMC_ESMFModUTestUNI 

TESTS_RM      = ESMF_ESMFModUTest.rm \
                ESMC_ESMFModUTest.rm


include ${ESMF_DIR}/makefile

DIRS	  = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt NULL core $(TESTS_BUILD)
CLOBBERDIRS =

#
# Fortran unit test
#
RUN_ESMF_ESMFModUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_ESMFModUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_ESMFModUTest > $(ESMC_TESTDIR)/ESMF_ESMFModUTest.stdout

RUN_ESMF_ESMFModUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_ESMFModUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_ESMFModUTest > $(ESMC_TESTDIR)/ESMF_ESMFModUTest.stdout

ESMF_ESMFModUTest.rm:
	echo "ESMF_ESMFModTest.rm target called here."


#
# C++ Unit tests
#
$(ESMC_TESTDIR)/ESMC_ESMFModUTest: chkopts ESMC_ESMFModUTest.o
	-${CXXF90LD} -o $(ESMC_TESTDIR)/ESMC_ESMFModUTest ESMC_ESMFModUTest.o -lesmf ${CXXF90LIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} ${NETCDF_LIB} $(SL_LINKOPTS)
	-${RM} ESMC_ESMFModUTest.o

RUN_ESMC_ESMFModUTest:
	-@rm -f testlog stdlog.* logfile.* ESMC_ESMFModUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMC_ESMFModUTest > $(ESMC_TESTDIR)/ESMC_ESMFModUTest.stdout

RUN_ESMC_ESMFModUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMC_ESMFModUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMC_ESMFModUTest > $(ESMC_TESTDIR)/ESMC_ESMFModUTest.stdout

ESMC_ESMFModUTest.rm:
	echo "ESMC_ESMFModTest.rm target called here."



