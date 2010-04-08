# $Id: makefile,v 1.11 2010/04/08 16:03:16 theurich Exp $ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR    = src/Infrastructure/Mesh/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMCI_IntegrateUTest \
 		$(ESMF_TESTDIR)/ESMC_MeshUTest \
 		$(ESMF_TESTDIR)/ESMF_MeshUTest 

TESTS_OBJ = 

TESTS_RUN     = RUN_ESMCI_IntegrateUTest \
 		RUN_ESMC_MeshUTest \
 		RUN_ESMF_MeshUTest 

TESTS_RUN_UNI = RUN_ESMCI_IntegrateUTestUNI \
		RUN_ESMF_MeshUTestUNI



include ${ESMF_DIR}/makefile

CLEANDIRS   =
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =



#
# unit test targets
#

# --- Mesh tests

RUN_ESMCI_IntegrateUTest:
	$(MAKE) TNAME=Integrate NP=1 citest

RUN_ESMCI_IntegrateUTestUNI:
	$(MAKE) TNAME=IntegrateUNI NP=1 citest

RUN_ESMF_MeshUTest:
	$(MAKE) TNAME=Mesh NP=4 ftest

RUN_ESMF_MeshUTestUNI:
	$(MAKE) TNAME=Mesh NP=1 ftest

RUN_ESMC_MeshUTest:
	cp -r data $(ESMF_TESTDIR)
	chmod u+rw $(ESMF_TESTDIR)/data/*
	$(MAKE) TNAME=Mesh NP=6 ctest
