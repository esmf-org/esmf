# $Id: makefile,v 1.6 2004/02/13 15:36:37 nscollins Exp $ 

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
DIRS	  = 
LOCDIR	  = src/Infrastructure/Grid/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_GridUTest 

TESTS_RUN     = RUN_ESMF_GridUTest 

TESTS_RUN_UNI = RUN_ESMF_GridUTestUNI 

TESTS_RM      = ESMF_GridUTest.rm 

include $(ESMF_DIR)/makefile

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt NULL stdoutfiles core $(TESTS_BUILD)
CLOBBERDIRS =


#
#  Grid
#
RUN_ESMF_GridUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_GridUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_GridUTest > $(ESMC_TESTDIR)/ESMF_GridUTest.stdout

RUN_ESMF_GridUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_GridUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_GridUTest > $(ESMC_TESTDIR)/ESMF_GridUTest.stdout

ESMF_GridUTest.rm:
	echo "ESMF_GridUTest.rm target called here."

