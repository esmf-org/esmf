# $Id: makefile,v 1.17 2007/03/06 00:10:58 theurich Exp $ 

ALL:  build_unit_tests
run:  run_unit_tests

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =
CPFILES   = ESMF_BundleCommHelpers.F90 ESMF_BundleRedistUseHelpers.F90

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
DIRS	  = 
LOCDIR	  = src/Infrastructure/BundleComm/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_BundleCommUTest \
                $(ESMF_TESTDIR)/ESMF_BundleCommOptionsUTest \
                $(ESMF_TESTDIR)/ESMF_BundleRedistUTest \
                $(ESMF_TESTDIR)/ESMF_BundleRedistUseUTest

TESTS_RUN     = RUN_ESMF_BundleCommUTest \
                RUN_ESMF_BundleCommOptionsUTest \
                RUN_ESMF_BundleRedistUTest \
                RUN_ESMF_BundleRedistUseUTest 

TESTS_RUN_UNI = RUN_ESMF_BundleCommUTestUNI \
                RUN_ESMF_BundleCommOptionsUTestUNI \
                RUN_ESMF_BundleRedistUTestUNI \
                RUN_ESMF_BundleRedistUseUTestUNI 

include ${ESMF_DIR}/makefile

DIRS      =

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD) $(CPFILES)
CLOBBERDIRS =


#
#  BundleComms
#

# redisthelpers is the real code, but after building it, it deletes
# both the .o and the .mod file.  so make a copy of it for the
# bundlecomm tests as well. 
ESMF_BundleCommHelpers.F90:  ESMF_BundleRedistHelpers.F90
	cp -f ESMF_BundleRedistHelpers.F90 ESMF_BundleCommHelpers.F90
ESMF_BundleRedistUseHelpers.F90:  ESMF_BundleRedistHelpers.F90
	cp -f ESMF_BundleRedistHelpers.F90 ESMF_BundleRedistUseHelpers.F90


UTEST_BundleComm_OBJS =  ESMF_BundleCommHelpers.o

ESMF_BundleCommUTest.o: ESMF_BundleCommHelpers.o

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


#
#  BundleRedist
#
UTEST_BundleRedist_OBJS =  ESMF_BundleRedistHelpers.o

ESMF_BundleRedistUTest.o: ESMF_BundleRedistHelpers.o

RUN_ESMF_BundleRedistUTest:
	$(MAKE) TNAME=BundleRedist NP=4 ftest

RUN_ESMF_BundleRedistUTestUNI:
	$(MAKE) TNAME=BundleRedist NP=1 ftest


#
#  BundleRedist
#
UTEST_BundleRedistUse_OBJS =  ESMF_BundleRedistUseHelpers.o

ESMF_BundleRedistUseUTest.o: ESMF_BundleRedistUseHelpers.o

RUN_ESMF_BundleRedistUseUTest:
	$(MAKE) TNAME=BundleRedistUse NP=4 ftest

RUN_ESMF_BundleRedistUseUTestUNI:
	$(MAKE) TNAME=BundleRedistUse NP=1 ftest


