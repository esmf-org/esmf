# $Id$ 

ALL:  build_unit_tests
run:  run_unit_tests
run_uni: run_unit_tests_uni

LOCDIR	  = src/Superstructure/State/tests

.NOTPARALLEL:
TESTS_BUILD   =	$(ESMF_TESTDIR)/ESMC_StateUTest \
                $(ESMF_TESTDIR)/ESMF_StateCreateUTest \
                $(ESMF_TESTDIR)/ESMF_StateReadWriteUTest \
                $(ESMF_TESTDIR)/ESMF_StateUTest

TESTS_RUN     =	RUN_ESMC_StateUTest \
                RUN_ESMF_StateCreateUTest \
                RUN_ESMF_StateReadWriteUTest \
                RUN_ESMF_StateUTest

TESTS_RUN_UNI =	RUN_ESMC_StateUTestUNI \
                RUN_ESMF_StateCreateUTestUNI \
                RUN_ESMF_StateReadWriteUTestUNI \
                RUN_ESMF_StateUTestUNI


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
# ESMC_StateUTest
#
RUN_ESMC_StateUTest:
	$(MAKE) TNAME=State NP=4 ctest

RUN_ESMC_StateUTestUNI:
	$(MAKE) TNAME=State NP=1 ctest

#       
# State Read/Write Unit Test
#

RUN_ESMF_StateReadWriteUTest:
	cp -f $(ESMF_DIR)/src/Infrastructure/IO/tests/io_netcdf_testdata.nc $(ESMF_TESTDIR)
	$(MAKE) TNAME=StateReadWrite NP=4 ftest

RUN_ESMF_StateReadWriteUTestUNI:
	cp -f $(ESMF_DIR)/src/Infrastructure/IO/tests/io_netcdf_testdata.nc $(ESMF_TESTDIR)
	$(MAKE) TNAME=StateReadWrite NP=1 ftest

