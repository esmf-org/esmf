# $Id: makefile,v 1.1 2009/07/21 05:55:46 eschwab Exp $ 

ALL:     build_unit_tests
run:     run_unit_tests
run_uni: run_unit_tests_uni

LOCDIR	  = src/Infrastructure/IO/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMC_IO_NetCDFUTest 

TESTS_RUN     = RUN_ESMC_IO_NetCDFUTest 

TESTS_RUN_UNI = RUN_ESMC_IO_NetCDFUTestUNI


include ${ESMF_DIR}/makefile

DIRS        = 

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD) 
CLOBBERDIRS =



RUN_ESMC_IO_NetCDFUTest:
	cp -f ./io_netcdf_testdata.nc $(ESMF_TESTDIR)
	$(MAKE) TNAME=IO_NetCDF NP=4 ctest

RUN_ESMC_IO_NetCDFUTestUNI:
	cp -f ./io_netcdf_testdata.nc $(ESMF_TESTDIR)
	$(MAKE) TNAME=IO_NetCDF NP=1 ctest
