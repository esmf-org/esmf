# $Id: makefile,v 1.1 2004/08/24 21:49:00 svasquez Exp $ 

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
LOCDIR	  = src/Infrastructure/VM/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_VMUTest

TESTS_RUN     = RUN_ESMF_VMUTest

TESTS_RUN_UNI = RUN_ESMF_VMUTestUNI 

TESTS_RM      = ESMF_VMUTest.rm

include ${ESMF_DIR}/makefile

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod NULL *.txt stdoutfiles core ESMF_LogFile $(TESTS_BUILD)
CLOBBERDIRS =


#
# VM
#
RUN_ESMF_VMUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_VMUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_VMUTest > $(ESMC_TESTDIR)/ESMF_VMUTest.stdout

RUN_ESMF_VMUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_VMUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_VMUTest > $(ESMC_TESTDIR)/ESMF_VMUTest.stdout

ESMF_VMUTest.rm:
	echo "ESMF_VMUTest.rm target called here."




