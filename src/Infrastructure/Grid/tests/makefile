# $Id: makefile,v 1.4 2003/04/15 20:30:36 svasquez Exp $ 

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
LOCDIR	  = src/Infrastructure/Grid/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_GridUTest 

TESTS_RUN     = RUN_ESMF_GridUTest 

TESTS_RUN_UNI = RUN_ESMF_GridUTestUNI 

TESTS_RM      = ESMF_GridUTest.rm 

include $(ESMF_DIR)/makefile

CLEANDIRS   =
CLEANFILES  = *.o *.mod 
CLOBBERDIRS =


#
#  Grid
#
$(ESMC_TESTDIR)/ESMF_GridUTest: ESMF_GridUTest.o
	-$(SL_F_LINKER) -o $(ESMC_TESTDIR)/ESMF_GridUTest ESMF_GridUTest.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	${RM} ESMF_GridUTest.o

RUN_ESMF_GridUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_GridUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_GridUTest > $(ESMC_TESTDIR)/ESMF_GridUTest.stdout

RUN_ESMF_GridUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_GridUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_GridUTest > $(ESMC_TESTDIR)/ESMF_GridUTest.stdout

ESMF_GridUTest.rm:
	echo "ESMF_GridUTest.rm target called here."

