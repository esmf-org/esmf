# $Id: makefile,v 1.8.2.1 2005/07/12 20:26:06 svasquez Exp $ 

ALL:  build_unit_tests

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

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_LogErrUTest
#		$(ESMC_TESTDIR)/ESMF_LogErrHaltUTest

TESTS_RUN     = RUN_ESMF_LogErrUTest
#                RUN_ESMF_LogErrHaltUTest

TESTS_RUN_UNI = RUN_ESMF_LogErrUTestUNI
#                RUN_ESMF_LogErrHaltUTestUNI
# Not running Halt on error test, it causes problems with lam.


include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD) Log*
CLOBBERDIRS =


#
# LogErr
#
RUN_ESMF_LogErrUTest:
	-$(RM) *LogErrUTest.Log
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_LogErrUTest > $(ESMC_TESTDIR)/ESMF_LogErrUTest.stdout
	-cat *LogErrUTest.Log > $(ESMC_TESTDIR)/ESMF_LogErrUTest.Log
	-$(RM) *LogErrUTest.Log

RUN_ESMF_LogErrUTestUNI:
	-$(RM) *LogErrUTest.Log
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_LogErrUTest > $(ESMC_TESTDIR)/ESMF_LogErrUTest.stdout
	-cat *LogErrUTest.Log > $(ESMC_TESTDIR)/ESMF_LogErrUTest.Log
	-$(RM) *LogErrUTest.Log

#
# LogErrHalt
#

RUN_ESMF_LogErrHaltUTest:
	-$(RM) *LogErrHaltUTest.Log
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_LogErrHaltUTest > $(ESMC_TESTDIR)/ESMF_LogErrHaltUTest.stdout
	-cat *LogErrHaltUTest.Log > $(ESMC_TESTDIR)/ESMF_LogErrHaltUTest.Log
	-$(RM) *LogErrHaltUTest.Log

RUN_ESMF_LogErrHaltUTestUNI:
	-$(RM) *LogErrHaltUTest.Log
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_LogErrHaltUTest > $(ESMC_TESTDIR)/ESMF_LogErrHaltUTest.stdout
	-cat *LogErrHaltUTest.Log > $(ESMC_TESTDIR)/ESMF_LogErrHaltUTest.Log
	-$(RM) *LogErrHaltUTest.Log



