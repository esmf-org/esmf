# $Id: makefile,v 1.1 2004/08/16 19:09:51 svasquez Exp $ 

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
LOCDIR	  = src/Infrastructure/LogErr/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_LogErrUTest.F90

TESTS_RUN     = RUN_ESMF_LogErrUTest

TESTS_RUN_UNI = RUN_ESMF_LogErrUTestUNI 

TESTS_RM      = ESMF_LogErrUTest.rm

include ${ESMF_DIR}/makefile

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod NULL *.txt stdoutfiles core ESMF_LogFile $(TESTS_BUILD)
CLOBBERDIRS =


#
# LogErr
#
RUN_ESMF_LogErrUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_LogErrUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_LogErrUTest > $(ESMC_TESTDIR)/ESMF_LogErrUTest.stdout

RUN_ESMF_LoggErrUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_LogErrUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_LogErrUTest > $(ESMC_TESTDIR)/ESMF_LogErrUTest.stdout

ESMF_LogErrUTest.rm:
	echo "ESMF_LogErrUTest.rm target called here."


