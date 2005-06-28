# $Id: makefile,v 1.18 2005/06/28 20:11:58 svasquez Exp $ 

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
LOCDIR	  = src/Infrastructure/Base/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_BaseUTest

TESTS_RUN     = RUN_ESMF_BaseUTest

TESTS_RUN_UNI = RUN_ESMF_BaseUTestUNI


include ${ESMF_DIR}/makefile

DIRS        =

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
#  Base class
#

RUN_ESMF_BaseUTest:
	-$(RM) *BaseUTest.Log
	-@rm -f testlog stdlog.* logfile.* ESMF_BaseUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_BaseUTest > $(ESMC_TESTDIR)/ESMF_BaseUTest.stdout
	-cat *BaseUTest.Log > $(ESMC_TESTDIR)/ESMF_BaseUTest.Log
	-$(RM) *BaseUTest.Log

RUN_ESMF_BaseUTestUNI:
	-$(RM) *BaseUTest.Log
	-@rm -f testlog stdlog.* logfile.* ESMF_BaseUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_BaseUTest > $(ESMC_TESTDIR)/ESMF_BaseUTest.stdout
	-cat *BaseUTest.Log > $(ESMC_TESTDIR)/ESMF_BaseUTest.Log
	-$(RM) *BaseUTest.Log


