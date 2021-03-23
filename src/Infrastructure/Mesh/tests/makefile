# $Id$ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR    = src/Infrastructure/Mesh/tests

.NOTPARALLEL:

TESTS_BUILD	  = \
                $(ESMF_TESTDIR)/ESMC_MBMeshUTest \
                $(ESMF_TESTDIR)/ESMC_MBMeshRegridUTest \
                $(ESMF_TESTDIR)/ESMC_MBMesh_ExtrapolateUTest \
                $(ESMF_TESTDIR)/ESMC_MBMesh_UtilUTest \
                $(ESMF_TESTDIR)/ESMC_MeshMOABUTest \
                $(ESMF_TESTDIR)/ESMCI_IntegrateUTest \
                $(ESMF_TESTDIR)/ESMC_MeshUTest \
                $(ESMF_TESTDIR)/ESMC_MeshVTKUTest \
                $(ESMF_TESTDIR)/ESMF_MeshOpUTest \
                $(ESMF_TESTDIR)/ESMF_MeshUTest \
                $(ESMF_TESTDIR)/ESMC_NearestUTest \
                $(ESMF_TESTDIR)/ESMC_Proj4UTest

# TESTS_BUILD   = $(ESMF_TESTDIR)/ESMC_MBMeshUTest
# TESTS_BUILD   = $(ESMF_TESTDIR)/ESMC_MBMeshRegridUTest
# TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_MeshUTest

TESTS_RUN    =  \
                RUN_ESMC_MBMeshUTest \
                RUN_ESMC_MBMeshRegridUTest \
                RUN_ESMC_MBMesh_ExtrapolateUTest \
                RUN_ESMC_MBMesh_UtilUTest \
                RUN_ESMC_MeshMOABUTest \
                RUN_ESMCI_IntegrateUTest \
                RUN_ESMC_MeshUTest \
                RUN_ESMC_MeshVTKUTest \
                RUN_ESMF_MeshOpUTest \
                RUN_ESMF_MeshUTest \
                RUN_ESMC_NearestUTest \
                RUN_ESMC_Proj4UTest

TESTS_RUN_UNI = \
                RUN_ESMC_MBMeshUTestUNI \
                RUN_ESMC_MBMeshRegridUTestUNI \
                RUN_ESMC_MBMesh_UtilUTestUNI \
                RUN_ESMC_MeshMOABUTestUNI \
                RUN_ESMCI_IntegrateUTestUNI \
                RUN_ESMC_MeshUTestUNI \
                RUN_ESMF_MeshOpUTestUNI \
                RUN_ESMF_MeshUTestUNI \
                RUN_ESMC_Proj4UTestUNI

include ${ESMF_DIR}/makefile

CLEANDIRS   =
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


ESMF_CXXCOMPILECPPFLAGS += -I$(ESMF_DIR)/$(LOCDIR)/../include \
                           -I$(ESMF_DIR)/$(LOCDIR)/../include/Legacy \
                           -I$(ESMF_DIR)/$(LOCDIR)/../include/Regridding

ifeq ($(ESMF_MOAB),internal)
ESMF_CXXCOMPILECPPFLAGS += -I$(ESMF_DIR)/$(LOCDIR)/../src/Moab \
                           -I$(ESMF_DIR)/$(LOCDIR)/../src/Moab/parallel \
                           -I$(ESMF_DIR)/$(LOCDIR)/../src/Moab/LocalDiscretization

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

RUN_ESMC_Proj4UTest:
	$(MAKE) TNAME=Proj4 NP=1 ctest

RUN_ESMC_Proj4UTestUNI:
	$(MAKE) TNAME=Proj4 NP=1 ctest


RUN_ESMC_MBMeshUTest:
	$(MAKE) TNAME=MBMesh NP=4 ctest

RUN_ESMC_MBMeshUTestUNI:
	$(MAKE) TNAME=MBMesh NP=1 ctest


RUN_ESMC_MBMeshRegridUTest:
	$(MAKE) TNAME=MBMeshRegrid NP=4 ctest

RUN_ESMC_MBMeshRegridUTestUNI:
	$(MAKE) TNAME=MBMeshRegrid NP=1 ctest


RUN_ESMC_MBMesh_ExtrapolateUTest:
	$(MAKE) TNAME=MBMesh_Extrapolate NP=4 ctest


RUN_ESMC_MBMesh_UtilUTest:
	$(MAKE) TNAME=MBMesh_Util NP=4 ctest

RUN_ESMC_MBMesh_UtilUTestUNI:
	$(MAKE) TNAME=MBMesh_Util NP=1 ctest


RUN_ESMC_NearestUTest:
	$(MAKE) TNAME=Nearest NP=4 ctest

