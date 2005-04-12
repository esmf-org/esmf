# $Id: makefile,v 1.7 2005/04/12 16:52:43 nscollins Exp $ 

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
LOCDIR	  = src/Infrastructure/Bundle/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_BundleUTest

TESTS_RUN     = RUN_ESMF_BundleUTest 

TESTS_RUN_UNI = RUN_ESMF_BundleUTestUNI 

TESTS_RM      = ESMF_BundleUTest.rm 

include ${ESMF_DIR}/makefile

DIRS      =

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
#  Bundles
#
RUN_ESMF_BundleUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_BundleUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_BundleUTest > $(ESMC_TESTDIR)/ESMF_BundleUTest.stdout
	-cat *BundleUTest.Log > $(ESMC_TESTDIR)/ESMF_BundleUTest.Log
	-rm *BundleUTest.Log

RUN_ESMF_BundleUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_BundleUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_BundleUTest > $(ESMC_TESTDIR)/ESMF_BundleUTest.stdout
	-cat *BundleUTest.Log > $(ESMC_TESTDIR)/ESMF_BundleUTest.Log
	-rm *BundleUTest.Log

ESMF_BundleUTest.rm:
	echo "ESMF_BundleUTest.rm target called here."



