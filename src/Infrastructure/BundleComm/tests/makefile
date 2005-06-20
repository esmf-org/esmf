# $Id: makefile,v 1.10 2005/06/20 16:42:30 nscollins Exp $ 

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
LOCDIR	  = src/Infrastructure/BundleComm/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_BundleCommUTest

TESTS_RUN     = RUN_ESMF_BundleCommUTest 

TESTS_RUN_UNI = RUN_ESMF_BundleCommUTestUNI 

TESTS_RM      = ESMF_BundleCommUTest.rm 

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
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_BundleCommUTest > $(ESMC_TESTDIR)/ESMF_BundleCommUTest.stdout
	-cat *BundleCommUTest.Log > $(ESMC_TESTDIR)/ESMF_BundleCommUTest.Log
	-rm *BundleCommUTest.Log

RUN_ESMF_BundleCommUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_BundleCommUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_BundleCommUTest > $(ESMC_TESTDIR)/ESMF_BundleCommUTest.stdout
	-cat *BundleCommUTest.Log > $(ESMC_TESTDIR)/ESMF_BundleCommUTest.Log
	-rm *BundleCommUTest.Log

ESMF_BundleCommUTest.rm:
	echo "ESMF_BundleCommUTest.rm target called here."



