# $Id: makefile,v 1.8 2005/06/28 20:13:59 svasquez Exp $ 

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
	-$(RM) *BundleUTest.Log
	-@rm -f testlog stdlog.* logfile.* ESMF_BundleUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_BundleUTest > $(ESMC_TESTDIR)/ESMF_BundleUTest.stdout
	-cat *BundleUTest.Log > $(ESMC_TESTDIR)/ESMF_BundleUTest.Log
	-$(RM) *BundleUTest.Log

RUN_ESMF_BundleUTestUNI:
	-$(RM) *BundleUTest.Log
	-@rm -f testlog stdlog.* logfile.* ESMF_BundleUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_BundleUTest > $(ESMC_TESTDIR)/ESMF_BundleUTest.stdout
	-cat *BundleUTest.Log > $(ESMC_TESTDIR)/ESMF_BundleUTest.Log
	-$(RM) *BundleUTest.Log

ESMF_BundleUTest.rm:
	echo "ESMF_BundleUTest.rm target called here."



