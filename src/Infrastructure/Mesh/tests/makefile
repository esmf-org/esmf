# $Id$ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR    = src/Infrastructure/Mesh/tests

.NOTPARALLEL:

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMCI_IntegrateUTest \
               $(ESMF_TESTDIR)/ESMC_MeshUTest \
               $(ESMF_TESTDIR)/ESMC_MeshMOABUTest \
               $(ESMF_TESTDIR)/ESMC_Proj4UTest \
               $(ESMF_TESTDIR)/ESMC_MeshVTKUTest \
               $(ESMF_TESTDIR)/ESMC_MBMesh_BilinearUTest \
               $(ESMF_TESTDIR)/ESMC_MBMesh_BilinearParUTest \
               $(ESMF_TESTDIR)/ESMC_MBMesh_BilinearSingleElemUTest \
               $(ESMF_TESTDIR)/ESMC_MBMesh_BilinearEdgeCasesUTest \
               $(ESMF_TESTDIR)/ESMC_MBMesh_BilinearEdgeCases3DUTest \
               $(ESMF_TESTDIR)/ESMC_MBMesh_CreateUTest \
               $(ESMF_TESTDIR)/ESMC_MBMesh_CreateParUTest \
               $(ESMF_TESTDIR)/ESMC_MBMesh_ExtrapolateUTest \
               $(ESMF_TESTDIR)/ESMC_MBMesh_RendezvousUTest \
               $(ESMF_TESTDIR)/ESMC_MBMesh_RendezvousParUTest \
               $(ESMF_TESTDIR)/ESMC_MBMesh_SearchUTest \
               $(ESMF_TESTDIR)/ESMC_MBMesh_UtilUTest \
               $(ESMF_TESTDIR)/ESMC_MBMesh_UtilParUTest \
               $(ESMF_TESTDIR)/ESMC_NearestUTest \
               $(ESMF_TESTDIR)/ESMF_MeshOpUTest \
               $(ESMF_TESTDIR)/ESMF_MeshUTest

# not quite ready for primetime
               # $(ESMF_TESTDIR)/ESMC_MBMesh_DualUTest \

TESTS_RUN     = RUN_ESMCI_IntegrateUTest \
                RUN_ESMC_MeshUTest \
                RUN_ESMC_MeshMOABUTest \
                RUN_ESMC_Proj4UTest \
                RUN_ESMC_MeshVTKUTest \
                RUN_ESMC_MBMesh_BilinearUTest \
                RUN_ESMC_MBMesh_BilinearParUTest \
                RUN_ESMC_MBMesh_BilinearSingleElemUTest \
                RUN_ESMC_MBMesh_BilinearEdgeCasesUTest \
                RUN_ESMC_MBMesh_BilinearEdgeCases3DUTest \
                RUN_ESMC_MBMesh_CreateUTest \
                RUN_ESMC_MBMesh_CreateParUTest \
                RUN_ESMC_MBMesh_DualUTest \
                RUN_ESMC_MBMesh_ExtrapolateUTest \
                RUN_ESMC_MBMesh_SearchUTest \
                RUN_ESMC_MBMesh_UtilUTest \
                RUN_ESMC_MBMesh_UtilParUTest \
                RUN_ESMC_NearestUTest \
                RUN_ESMF_MeshOpUTest \
                RUN_ESMF_MeshUTest

# this test is not complete, should test both elem and node rendezvous 
#   current version fails in uni on electra intel
#                RUN_ESMC_MBMesh_RendezvousUTest \
#                RUN_ESMC_MBMesh_RendezvousParUTest \

TESTS_RUN_UNI = RUN_ESMCI_IntegrateUTestUNI \
                RUN_ESMC_MeshUTestUNI \
                RUN_ESMC_MeshMOABUTestUNI \
                RUN_ESMC_Proj4UTestUNI \
                RUN_ESMC_MBMesh_BilinearUTestUNI \
                RUN_ESMC_MBMesh_BilinearSingleElemUTestUNI \
                RUN_ESMC_MBMesh_BilinearEdgeCasesUTestUNI \
                RUN_ESMC_MBMesh_BilinearEdgeCases3DUTestUNI \
                RUN_ESMC_MBMesh_CreateUTestUNI \
                RUN_ESMC_MBMesh_DualUTestUNI \
                RUN_ESMC_MBMesh_SearchUTestUNI \
                RUN_ESMC_MBMesh_UtilUTestUNI \
                RUN_ESMF_MeshOpUTestUNI \
                RUN_ESMF_MeshUTestUNI

# this test is not complete, should test both elem and node rendezvous 
#   current version fails in uni on electra intel
#                RUN_ESMC_MBMesh_RendezvousUTestUNI \


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


RUN_ESMC_MBMesh_BilinearUTestUNI:
	$(MAKE) TNAME=MBMesh_Bilinear NP=1 ctest

RUN_ESMC_MBMesh_BilinearUTest:
	$(MAKE) TNAME=MBMesh_Bilinear NP=1 ctest

RUN_ESMC_MBMesh_BilinearParUTest:
	$(MAKE) TNAME=MBMesh_BilinearPar NP=4 ctest


RUN_ESMC_MBMesh_BilinearSingleElemUTestUNI:
	$(MAKE) TNAME=MBMesh_BilinearSingleElem NP=1 ctest

RUN_ESMC_MBMesh_BilinearSingleElemUTest:
	$(MAKE) TNAME=MBMesh_BilinearSingleElem NP=1 ctest


RUN_ESMC_MBMesh_BilinearEdgeCasesUTestUNI:
	$(MAKE) TNAME=MBMesh_BilinearEdgeCases NP=1 ctest

RUN_ESMC_MBMesh_BilinearEdgeCasesUTest:
	$(MAKE) TNAME=MBMesh_BilinearEdgeCases NP=1 ctest


RUN_ESMC_MBMesh_BilinearEdgeCases3DUTestUNI:
	$(MAKE) TNAME=MBMesh_BilinearEdgeCases3D NP=1 ctest

RUN_ESMC_MBMesh_BilinearEdgeCases3DUTest:
	$(MAKE) TNAME=MBMesh_BilinearEdgeCases3D NP=1 ctest


RUN_ESMC_MBMesh_CreateUTestUNI:
	$(MAKE) TNAME=MBMesh_Create NP=1 ctest

RUN_ESMC_MBMesh_CreateUTest:
	$(MAKE) TNAME=MBMesh_Create NP=1 ctest

RUN_ESMC_MBMesh_CreateParUTest:
	$(MAKE) TNAME=MBMesh_CreatePar NP=4 ctest


RUN_ESMC_MBMesh_DualUTestUNI:
	$(MAKE) TNAME=MBMesh_Dual NP=1 ctest

RUN_ESMC_MBMesh_DualUTest:
	$(MAKE) TNAME=MBMesh_Dual NP=4 ctest

RUN_ESMC_MBMesh_DualParUTest:
	$(MAKE) TNAME=MBMesh_DualPar NP=4 ctest


RUN_ESMC_MBMesh_ExtrapolateUTest:
	$(MAKE) TNAME=MBMesh_Extrapolate NP=4 ctest



RUN_ESMC_MBMesh_RendezvousUTest:
	$(MAKE) TNAME=MBMesh_Rendezvous NP=1 ctest

RUN_ESMC_MBMesh_RendezvousUTestUNI:
	$(MAKE) TNAME=MBMesh_Rendezvous NP=1 ctest

RUN_ESMC_MBMesh_RendezvousParUTest:
	$(MAKE) TNAME=MBMesh_RendezvousPar NP=4 ctest


RUN_ESMC_MBMesh_UtilUTest:
	$(MAKE) TNAME=MBMesh_Util NP=1 ctest

RUN_ESMC_MBMesh_UtilUTestUNI:
	$(MAKE) TNAME=MBMesh_Util NP=1 ctest

RUN_ESMC_MBMesh_UtilParUTest:
	$(MAKE) TNAME=MBMesh_UtilPar NP=4 ctest


RUN_ESMC_MBMesh_SearchUTest:
	$(MAKE) TNAME=MBMesh_Search NP=4 ctest

RUN_ESMC_MBMesh_SearchUTestUNI:
	$(MAKE) TNAME=MBMesh_Search NP=1 ctest


RUN_ESMC_NearestUTest:
	$(MAKE) TNAME=Nearest NP=4 ctest

