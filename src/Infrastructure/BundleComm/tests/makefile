# $Id: makefile,v 1.12 2005/06/28 20:16:11 svasquez Exp $ 

ALL:  build_unit_tests
run:  run_unit_tests

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
LOCDIR	  = src/Infrastructure/BundleComm/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_BundleCommUTest \
                $(ESMF_TESTDIR)/ESMF_BundleCommOptionsUTest

TESTS_RUN     = RUN_ESMF_BundleCommUTest \
                RUN_ESMF_BundleCommOptionsUTest 

TESTS_RUN_UNI = RUN_ESMF_BundleCommUTestUNI \
                RUN_ESMF_BundleCommOptionsUTestUNI 

include ${ESMF_DIR}/makefile

DIRS      =

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
#  BundleComms
#
RUN_ESMF_BundleCommUTest:
	-$(RM) *BundleCommUTest.Log
	-@rm -f testlog stdlog.* logfile.* ESMF_BundleCommUTest.tmps
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_BundleCommUTest > $(ESMF_TESTDIR)/ESMF_BundleCommUTest.stdout
	-cat *BundleCommUTest.Log > $(ESMF_TESTDIR)/ESMF_BundleCommUTest.Log
	-$(RM) *BundleCommUTest.Log

RUN_ESMF_BundleCommUTestUNI:
	-$(RM) *BundleCommUTest.Log
	-@rm -f testlog stdlog.* logfile.* ESMF_BundleCommUTest.tmps
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_BundleCommUTest > $(ESMF_TESTDIR)/ESMF_BundleCommUTest.stdout
	-cat *BundleCommUTest.Log > $(ESMF_TESTDIR)/ESMF_BundleCommUTest.Log
	-$(RM) *BundleCommUTest.Log


#
#  BundleCommOptions
#
RUN_ESMF_BundleCommOptionsUTest:
	-$(RM) *BundleCommOptionsUTest.Log
	-@rm -f testlog stdlog.* logfile.* ESMF_BundleCommOptionsUTest.tmps
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_BundleCommOptionsUTest > $(ESMF_TESTDIR)/ESMF_BundleCommOptionsUTest.stdout
	-cat *BundleCommOptionsUTest.Log > $(ESMF_TESTDIR)/ESMF_BundleCommOptionsUTest.Log
	-$(RM) *BundleCommOptionsUTest.Log

RUN_ESMF_BundleCommOptionsUTestUNI:
	-$(RM) *BundleCommOptionsUTest.Log
	-@rm -f testlog stdlog.* logfile.* ESMF_BundleCommOptionsUTest.tmps
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_BundleCommOptionsUTest > $(ESMF_TESTDIR)/ESMF_BundleCommOptionsUTest.stdout
	-cat *BundleCommOptionsUTest.Log > $(ESMF_TESTDIR)/ESMF_BundleCommOptionsUTest.Log
	-$(RM) *BundleCommOptionsUTest.Log



