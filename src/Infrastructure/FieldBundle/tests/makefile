# $Id$ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Infrastructure/FieldBundle/tests

.NOTPARALLEL:
TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_FieldBundleUTest \
                $(ESMC_TESTDIR)/ESMF_FieldBundleCrGetUTest \
                $(ESMC_TESTDIR)/ESMF_FieldBundleRedistUTest \
                $(ESMC_TESTDIR)/ESMF_FieldBundleSMMUTest \
                $(ESMC_TESTDIR)/ESMF_FieldBundleIOUTest \
                $(ESMC_TESTDIR)/ESMF_FieldBundleRegridUTest

TESTS_RUN     = RUN_ESMF_FieldBundleUTest \
                RUN_ESMF_FieldBundleCrGetUTest \
                RUN_ESMF_FieldBundleRedistUTest \
                RUN_ESMF_FieldBundleSMMUTest \
                RUN_ESMF_FieldBundleIOUTest \
                RUN_ESMF_FieldBundleRegridUTest

TESTS_RUN_UNI = RUN_ESMF_FieldBundleUTestUNI \
                RUN_ESMF_FieldBundleRegridUTestUNI


include ${ESMF_DIR}/makefile

DIRS      =

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
#  FieldBundles
#
RUN_ESMF_FieldBundleUTest:
	$(MAKE) TNAME=FieldBundle NP=4 ftest

RUN_ESMF_FieldBundleUTestUNI:
	$(MAKE) TNAME=FieldBundle NP=1 ftest

RUN_ESMF_FieldBundleCrGetUTest:
	$(MAKE) TNAME=FieldBundleCrGet NP=4 ftest

RUN_ESMF_FieldBundleRedistUTest:
	$(MAKE) TNAME=FieldBundleRedist NP=4 ftest

RUN_ESMF_FieldBundleSMMUTest:
	$(MAKE) TNAME=FieldBundleSMM NP=4 ftest

RUN_ESMF_FieldBundleIOUTest:
	$(MAKE) TNAME=FieldBundleIO NP=4 ftest

RUN_ESMF_FieldBundleRegridUTest:
	$(MAKE) TNAME=FieldBundleRegrid NP=4 ftest

RUN_ESMF_FieldBundleRegridUTestUNI:
	$(MAKE) TNAME=FieldBundleRegrid NP=1 ftest

