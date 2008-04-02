# $Id: makefile,v 1.18.2.1 2008/04/02 20:07:12 cdeluca Exp $ 

ALL:  build_unit_tests
run:  run_unit_tests

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =
CPFILES   = ESMF_FieldBundleCommHelpers.F90 ESMF_FieldBundleRedistUseHelpers.F90

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
DIRS	  = 
LOCDIR	  = src/Infrastructure/FieldBundleComm/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_FieldBundleCommUTest \
		$(ESMF_TESTDIR)/ESMF_FieldBundleCommOptionsUTest \
		$(ESMF_TESTDIR)/ESMF_FieldBundleRedistUTest \
		$(ESMF_TESTDIR)/ESMF_FieldBundleRedistUseUTest

TESTS_RUN     = RUN_ESMF_FieldBundleCommUTest \
		RUN_ESMF_FieldBundleCommOptionsUTest \
		RUN_ESMF_FieldBundleRedistUTest \
		RUN_ESMF_FieldBundleRedistUseUTest 

TESTS_RUN_UNI = RUN_ESMF_FieldBundleCommUTestUNI \
		RUN_ESMF_FieldBundleCommOptionsUTestUNI \
		RUN_ESMF_FieldBundleRedistUTestUNI \
		RUN_ESMF_FieldBundleRedistUseUTestUNI 

include ${ESMF_DIR}/makefile

DIRS      =

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD) $(CPFILES)
CLOBBERDIRS =


#
#  FieldBundleComms
#

# redisthelpers is the real code, but after building it, it deletes
# both the .o and the .mod file.  so make a copy of it for the
# bundlecomm tests as well. 
ESMF_FieldBundleCommHelpers.F90:  ESMF_FieldBundleRedistHelpers.F90
	cp -f ESMF_FieldBundleRedistHelpers.F90 ESMF_FieldBundleCommHelpers.F90
ESMF_FieldBundleRedistUseHelpers.F90:  ESMF_FieldBundleRedistHelpers.F90
	cp -f ESMF_FieldBundleRedistHelpers.F90 ESMF_FieldBundleRedistUseHelpers.F90


UTEST_FieldBundleComm_OBJS =  ESMF_FieldBundleCommHelpers.o

ESMF_FieldBundleCommUTest.o: ESMF_FieldBundleCommHelpers.o

RUN_ESMF_FieldBundleCommUTest:
	$(MAKE) TNAME=FieldBundleComm NP=4 ftest

RUN_ESMF_FieldBundleCommUTestUNI:
	$(MAKE) TNAME=FieldBundleComm NP=1 ftest

#
#  FieldBundleCommOptions
#
RUN_ESMF_FieldBundleCommOptionsUTest:
	$(MAKE) TNAME=FieldBundleCommOptions NP=4 ftest

RUN_ESMF_FieldBundleCommOptionsUTestUNI:
	$(MAKE) TNAME=FieldBundleCommOptions NP=1 ftest


#
#  FieldBundleRedist
#
UTEST_FieldBundleRedist_OBJS =  ESMF_FieldBundleRedistHelpers.o

ESMF_FieldBundleRedistUTest.o: ESMF_FieldBundleRedistHelpers.o

RUN_ESMF_FieldBundleRedistUTest:
	$(MAKE) TNAME=FieldBundleRedist NP=4 ftest

RUN_ESMF_FieldBundleRedistUTestUNI:
	$(MAKE) TNAME=FieldBundleRedist NP=1 ftest


#
#  FieldBundleRedist
#
UTEST_FieldBundleRedistUse_OBJS =  ESMF_FieldBundleRedistUseHelpers.o

ESMF_FieldBundleRedistUseUTest.o: ESMF_FieldBundleRedistUseHelpers.o

RUN_ESMF_FieldBundleRedistUseUTest:
	$(MAKE) TNAME=FieldBundleRedistUse NP=4 ftest

RUN_ESMF_FieldBundleRedistUseUTestUNI:
	$(MAKE) TNAME=FieldBundleRedistUse NP=1 ftest


