# $Id: makefile,v 1.11 2005/06/24 21:01:58 nscollins Exp $ 

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
	-@rm -f testlog stdlog.* logfile.* ESMF_BundleCommUTest.tmps
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_BundleCommUTest > $(ESMF_TESTDIR)/ESMF_BundleCommUTest.stdout
	-cat *BundleCommUTest.Log > $(ESMF_TESTDIR)/ESMF_BundleCommUTest.Log
	-rm *BundleCommUTest.Log

RUN_ESMF_BundleCommUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_BundleCommUTest.tmps
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_BundleCommUTest > $(ESMF_TESTDIR)/ESMF_BundleCommUTest.stdout
	-cat *BundleCommUTest.Log > $(ESMF_TESTDIR)/ESMF_BundleCommUTest.Log
	-rm *BundleCommUTest.Log


#
#  BundleCommOptions
#
RUN_ESMF_BundleCommOptionsUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_BundleCommOptionsUTest.tmps
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_BundleCommOptionsUTest > $(ESMF_TESTDIR)/ESMF_BundleCommOptionsUTest.stdout
	-cat *BundleCommOptionsUTest.Log > $(ESMF_TESTDIR)/ESMF_BundleCommOptionsUTest.Log
	-rm *BundleCommOptionsUTest.Log

RUN_ESMF_BundleCommOptionsUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_BundleCommOptionsUTest.tmps
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_BundleCommOptionsUTest > $(ESMF_TESTDIR)/ESMF_BundleCommOptionsUTest.stdout
	-cat *BundleCommOptionsUTest.Log > $(ESMF_TESTDIR)/ESMF_BundleCommOptionsUTest.Log
	-rm *BundleCommOptionsUTest.Log



