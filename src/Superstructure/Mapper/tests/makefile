# $Id$ 

ALL:  build_unit_tests
run:  
run_uni: run_unit_tests_uni

LOCDIR	  = src/Superstructure/Mapper/tests

.NOTPARALLEL:
TESTS_BUILD   =	$(ESMF_TESTDIR)/ESMCI_PolyUTest \
                $(ESMF_TESTDIR)/ESMCI_MatUTest \
                $(ESMF_TESTDIR)/ESMCI_PolyFitUTest

TESTS_RUN     =

TESTS_RUN_UNI =	RUN_ESMCI_PolyUTestUNI \
                RUN_ESMCI_MatUTestUNI \
                RUN_ESMCI_PolyFitUTestUNI


include $(ESMF_DIR)/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

DIRS        = 

ESMF_CXXCOMPILEOPTS += -std=c++11 -I/home/jayeshkrishna/parvis/lapack-3.4.2 -I/home/jayeshkrishna/parvis/lapack-3.4.2/lapacke/include

ESMF_CXXLINKLIBS += -L/home/jayeshkrishna/parvis/lapack-3.4.2 -llapacke -llapack -L/home/jayeshkrishna/parvis/BLAS -lblas_LINUX

#       
#       
# Univariate Polynomial test
#

RUN_ESMCI_PolyUTestUNI:
	$(MAKE) TNAME=UniVPoly NP=1 ctest

RUN_ESMCI_MatUTestUNI:
	$(MAKE) TNAME=Matrix NP=1 ctest

RUN_ESMCI_PolyFitUTestUNI:
	$(MAKE) TNAME=FitPoly NP=1 ctest

