# $Id$ 

ALL:  build_unit_tests
run:  run_unit_tests
run_uni: run_unit_tests_uni

LOCDIR	  = src/Superstructure/State/tests

.NOTPARALLEL:
TESTS_BUILD   =	$(ESMF_TESTDIR)/ESMC_StateUTest \
                $(ESMF_TESTDIR)/ESMF_StateCreateUTest \
                $(ESMF_TESTDIR)/ESMF_StateReconcileUTest \
                $(ESMF_TESTDIR)/ESMF_StateReadWriteUTest \
		$(ESMF_TESTDIR)/ESMF_StateUTest
#		$(ESMF_TESTDIR)/ESMF_StateReconcileProxyUTest \

# TODO:FIELDINTEGRATION Restore State reconcile and other tests.
#		$(ESMF_TESTDIR)/ESMF_StateUTest
#		$(ESMF_TESTDIR)/ESMF_StateReconcileUTest \
#		$(ESMF_TESTDIR)/ESMF_StateLimitUTest \

TESTS_RUN     =	RUN_ESMC_StateUTest \
         	RUN_ESMF_StateCreateUTest \
                RUN_ESMF_StateReconcileUTest \
                RUN_ESMF_StateReadWriteUTest \
		RUN_ESMF_StateUTest
#               RUN_ESMF_StateReconcileProxyUTest \
#		RUN_ESMF_StateLimitUTest \

TESTS_RUN_UNI =	RUN_ESMC_StateUTestUNI \
                RUN_ESMF_StateCreateUTestUNI \
                RUN_ESMF_StateReconcileUTestUNI \
                RUN_ESMF_StateReadWriteUTestUNI \
		RUN_ESMF_StateUTestUNI
#		RUN_ESMF_StateLimitUTestUNI \


include $(ESMF_DIR)/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

DIRS        = 

#       
# State Create
#

RUN_ESMF_StateCreateUTest:
	$(MAKE) TNAME=StateCreate NP=4 ftest


RUN_ESMF_StateCreateUTestUNI:
	$(MAKE) TNAME=StateCreate NP=1 ftest


#       
# State Unit Test
#

RUN_ESMF_StateUTest:
	$(MAKE) TNAME=State NP=4 ftest

RUN_ESMF_StateUTestUNI:
	$(MAKE) TNAME=State NP=1 ftest


#       
# StateReconcile Unit Test
#

RUN_ESMF_StateReconcileUTest:
	$(MAKE) TNAME=StateReconcile NP=4 ftest

RUN_ESMF_StateReconcileUTestUNI:
	$(MAKE) TNAME=StateReconcile NP=1 ftest

#       
# StateReconcileProxy Unit Test
#

RUN_ESMF_StateReconcileProxyUTest:
	$(MAKE) TNAME=StateReconcileProxy NP=4 ftest

#
# ESMC_StateUTest
#
RUN_ESMC_StateUTest:
	$(MAKE) TNAME=State NP=4 ctest

RUN_ESMC_StateUTestUNI:
	$(MAKE) TNAME=State NP=1 ctest

#       
# StateLimit Unit Test
#

ESMF_UTEST_StateLimit_OBJS = ESMF_StateHelpers.o

ESMF_StateLimitUTest.o: ESMF_StateHelpers.o

RUN_ESMF_StateLimitUTest:
	$(MAKE) TNAME=StateLimit NP=4 ftest

RUN_ESMF_StateLimitUTestUNI:
	$(MAKE) TNAME=StateLimit NP=1 ftest

#       
# State Read/Write Unit Test
#

RUN_ESMF_StateReadWriteUTest:
	cp -f $(ESMF_DIR)/src/Infrastructure/IO/tests/io_netcdf_testdata.nc $(ESMF_TESTDIR)
	$(MAKE) TNAME=StateReadWrite NP=4 ftest

RUN_ESMF_StateReadWriteUTestUNI:
	cp -f $(ESMF_DIR)/src/Infrastructure/IO/tests/io_netcdf_testdata.nc $(ESMF_TESTDIR)
	$(MAKE) TNAME=StateReadWrite NP=1 ftest

