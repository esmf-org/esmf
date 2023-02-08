# $Id$

ALL:     build_unit_tests
run:     run_unit_tests
run_uni: run_unit_tests_uni

LOCDIR	  = src/Infrastructure/IO/tests

.NOTPARALLEL:

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMCI_IO_NetCDFUTest  \
		$(ESMF_TESTDIR)/ESMCI_IO_PIOUTest \
		$(ESMF_TESTDIR)/ESMC_IO_InqUTest \
		$(ESMF_TESTDIR)/ESMF_IO_YAMLUTest \
		$(ESMF_TESTDIR)/ESMF_IOUTest \
		$(ESMF_TESTDIR)/ESMF_IO_MultitileUTest

TESTS_RUN     = RUN_ESMCI_IO_NetCDFUTest \
		RUN_ESMCI_IO_PIOUTest \
		RUN_ESMC_IO_InqUTest \
		RUN_ESMF_IO_YAMLUTest \
		RUN_ESMF_IOUTest \
		RUN_ESMF_IO_MultitileUTest

TESTS_RUN_UNI = RUN_ESMCI_IO_NetCDFUTestUNI \
		RUN_ESMCI_IO_PIOUTestUNI \
		RUN_ESMC_IO_InqUTestUNI \
		RUN_ESMF_IO_YAMLUTestUNI \
		RUN_ESMF_IOUTestUNI

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

RUN_ESMCI_IO_PIOUTest:
	$(MAKE) TNAME=IO_PIO NP=4 citest

RUN_ESMCI_IO_PIOUTestUNI:
	$(MAKE) TNAME=IO_PIO NP=1 citest

RUN_ESMC_IO_InqUTest:
	cp -f T42_grid.nc $(ESMF_TESTDIR)
	cp -f GRIDSPEC_320x160.nc $(ESMF_TESTDIR)
	$(MAKE) TNAME=IO_Inq NP=1 ctest

RUN_ESMC_IO_InqUTestUNI:
	cp -f T42_grid.nc $(ESMF_TESTDIR)
	cp -f GRIDSPEC_320x160.nc $(ESMF_TESTDIR)
	$(MAKE) TNAME=IO_Inq NP=1 ctest

RUN_ESMF_IOUTest:
	rm -f $(ESMF_TESTDIR)/doodle*.nc
	$(MAKE) TNAME=IO NP=4 ftest

RUN_ESMF_IOUTestUNI:
	rm -f $(ESMF_TESTDIR)/doodle*.nc
	$(MAKE) TNAME=IO NP=1 ftest

RUN_ESMF_IO_YAMLUTest:
	cp -f fd.yaml $(ESMF_TESTDIR)
	$(MAKE) TNAME=IO_YAML NP=4 ftest

RUN_ESMF_IO_YAMLUTestUNI:
	cp -f fd.yaml $(ESMF_TESTDIR)
	$(MAKE) TNAME=IO_YAML NP=1 ftest

RUN_ESMF_IO_MultitileUTest:
	rm -f $(ESMF_TESTDIR)/ESMF_IO_MultitileUTest*.nc
	$(MAKE) TNAME=IO_Multitile NP=8 ftest
