# $Id$ 

ALL:     build_unit_tests
run:     run_unit_tests
run_uni: run_unit_tests_uni

LOCDIR	  = src/Infrastructure/IO/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMCI_IO_NetCDFUTest 

TESTS_RUN     = RUN_ESMCI_IO_NetCDFUTest 

TESTS_RUN_UNI = RUN_ESMCI_IO_NetCDFUTestUNI


include ${ESMF_DIR}/makefile

DIRS        = 

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD) 
CLOBBERDIRS =



RUN_ESMCI_IO_NetCDFUTest:
	cp -f ./io_netcdf_testdata.nc $(ESMF_TESTDIR)
	$(MAKE) TNAME=IO_NetCDF NP=4 citest

RUN_ESMCI_IO_NetCDFUTestUNI:
	cp -f ./io_netcdf_testdata.nc $(ESMF_TESTDIR)
	$(MAKE) TNAME=IO_NetCDF NP=1 citest
