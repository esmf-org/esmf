# $Id: makefile,v 1.1 2004/08/12 17:38:51 svasquez Exp $ 

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
LOCDIR	  = src/Infrastructure/IOSpec/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_IOSpecUTest

TESTS_RUN     = RUN_ESMF_IOSpecUTest

TESTS_RUN_UNI = RUN_ESMF_IOSpecUTestUNI 

TESTS_RM      = ESMF_IOSpecUTest.rm

include ${ESMF_DIR}/makefile

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod NULL *.txt stdoutfiles core ESMF_LogFile $(TESTS_BUILD)
CLOBBERDIRS =


#
# IOSpec
#
RUN_ESMF_IOSpecUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_IOSpecUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_IOSpecUTest > $(ESMC_TESTDIR)/ESMF_IOSpecUTest.stdout

RUN_ESMF_IOSpecUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_IOSpecUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_IOSpecUTest > $(ESMC_TESTDIR)/ESMF_IOSpecUTest.stdout

ESMF_IOSpecUTest.rm:
	echo "ESMF_IOSpecUTest.rm target called here."
