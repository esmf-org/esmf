# $Id: makefile,v 1.7 2003/04/17 19:14:19 eschwab Exp $

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
DIRS	  =
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

CLEANDIRS   =$
CLEANFILES  = *.o *.mod $
CLOBBERDIRS =$


#
#
#
$(ESMC_TESTDIR)/ESMF_ClockUTest: ESMF_ClockUTest.o
	-$(SL_F_LINKER) -o $(ESMC_TESTDIR)/ESMF_ClockUTest ESMF_ClockUTest.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	${RM} ESMF_ClockUTest.o

RUN_ESMF_ClockUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_ClockUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_ClockUTest > $(ESMC_TESTDIR)/ESMF_ClockUTest.stdout

RUN_ESMF_ClockUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_ClockUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_ClockUTest > $(ESMC_TESTDIR)/ESMF_ClockUTest.stdout

ESMF_ClockUTest.rm:
	echo "ESMF_ClockTest.rm target called here."

#
#
#
$(ESMC_TESTDIR)/ESMC_ClockUTest: ESMC_ClockUTest.o
	-$(SL_C_LINKER) -o $(ESMC_TESTDIR)/ESMC_ClockUTest ESMC_ClockUTest.o -lesmf ${CXXF90LIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	${RM} ESMC_ClockUTest.o

RUN_ESMC_ClockUTest:
	-@rm -f testlog stdlog.* logfile.* ESMC_ClockUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMC_ClockUTest > $(ESMC_TESTDIR)/ESMC_ClockUTest.stdout

RUN_ESMC_ClockUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMC_ClockUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMC_ClockUTest > $(ESMC_TESTDIR)/ESMC_ClockUTest.stdout

ESMC_ClockUTest.rm:
	echo "ESMC_ClockTest.rm target called here."



