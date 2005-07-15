# $Id: makefile,v 1.13 2005/07/15 03:41:46 nscollins Exp $ 

ALL:  build_unit_tests
run:  tree_run_unit_tests

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
	$(MAKE) TNAME=BundleComm NP=4 ftest

RUN_ESMF_BundleCommUTestUNI:
	$(MAKE) TNAME=BundleComm NP=1 ftest


#
#  BundleCommOptions
#
RUN_ESMF_BundleCommOptionsUTest:
	$(MAKE) TNAME=BundleCommOptions NP=4 ftest

RUN_ESMF_BundleCommOptionsUTestUNI:
	$(MAKE) TNAME=BundleCommOptions NP=1 ftest



