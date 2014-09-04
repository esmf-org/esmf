# $Id$ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR    = src/Infrastructure/Mesh/tests

.NOTPARALLEL:
TESTS_BUILD   = $(ESMF_TESTDIR)/ESMCI_IntegrateUTest \
 		$(ESMF_TESTDIR)/ESMC_MeshUTest \
 		$(ESMF_TESTDIR)/ESMC_MeshMOABUTest \
 		$(ESMF_TESTDIR)/ESMC_MeshVTKUTest \
 		$(ESMF_TESTDIR)/ESMF_MeshOpUTest \
 		$(ESMF_TESTDIR)/ESMF_MeshUTest 

TESTS_OBJ = 

TESTS_RUN     = RUN_ESMCI_IntegrateUTest \
 		RUN_ESMC_MeshUTest \
 		RUN_ESMC_MeshMOABUTest \
 		RUN_ESMC_MeshVTKUTest \
 		RUN_ESMF_MeshOpUTest \
 		RUN_ESMF_MeshUTest 

TESTS_RUN_UNI = RUN_ESMCI_IntegrateUTestUNI \
 		RUN_ESMC_MeshUTestUNI \
 		RUN_ESMC_MeshMOABUTestUNI \
 		RUN_ESMF_MeshOpUTestUNI \
		RUN_ESMF_MeshUTestUNI



include ${ESMF_DIR}/makefile

CLEANDIRS   =
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

ifeq ($(ESMF_MOAB),internal)
ESMF_CXXCOMPILECPPFLAGS += -I../src/Moab
endif


#
# unit test targets
#

# --- Mesh tests

RUN_ESMCI_IntegrateUTest:
	$(MAKE) TNAME=Integrate NP=1 citest

RUN_ESMCI_IntegrateUTestUNI:
	$(MAKE) TNAME=Integrate NP=1 citest

RUN_ESMF_MeshOpUTest:
	$(MAKE) TNAME=MeshOp NP=4 ftest

RUN_ESMF_MeshOpUTestUNI:
	$(MAKE) TNAME=MeshOp NP=1 ftest

RUN_ESMF_MeshUTest:
	$(MAKE) TNAME=Mesh NP=4 ftest

RUN_ESMF_MeshUTestUNI:
	$(MAKE) TNAME=Mesh NP=1 ftest

RUN_ESMC_MeshVTKUTest:
	cp -r data $(ESMF_TESTDIR)
	chmod u+rw $(ESMF_TESTDIR)/data/*
	$(MAKE) TNAME=MeshVTK NP=6 ctest

RUN_ESMC_MeshUTest:
	cp -r data $(ESMF_TESTDIR)
	$(MAKE) TNAME=Mesh NP=1 ctest

RUN_ESMC_MeshUTestUNI:
	cp -r data $(ESMF_TESTDIR)
	$(MAKE) TNAME=Mesh NP=1 ctest

RUN_ESMC_MeshMOABUTest:
	$(MAKE) TNAME=MeshMOAB NP=1 ctest

RUN_ESMC_MeshMOABUTestUNI:
	$(MAKE) TNAME=MeshMOAB NP=1 ctest

