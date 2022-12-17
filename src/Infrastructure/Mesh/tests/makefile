# $Id$ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR    = src/Infrastructure/Mesh/tests

.NOTPARALLEL:

TESTS_BUILD	  = \
                $(ESMF_TESTDIR)/ESMCI_MBMesh_ExtrapolateUTest \
                $(ESMF_TESTDIR)/ESMCI_MBMesh_UtilUTest \
                $(ESMF_TESTDIR)/ESMCI_MeshCapUTest \
                $(ESMF_TESTDIR)/ESMCI_MeshCapRegridUTest \
                $(ESMF_TESTDIR)/ESMCI_MeshMOABUTest \
                $(ESMF_TESTDIR)/ESMCI_IntegrateUTest \
                $(ESMF_TESTDIR)/ESMCI_MeshUTest \
                $(ESMF_TESTDIR)/ESMCI_PgonUTest \
                $(ESMF_TESTDIR)/ESMC_MeshVTKUTest \
                $(ESMF_TESTDIR)/ESMF_MeshOpUTest \
                $(ESMF_TESTDIR)/ESMF_MeshUTest \
                $(ESMF_TESTDIR)/ESMCI_NearestUTest \
                $(ESMF_TESTDIR)/ESMF_MeshFileIOUTest \
                $(ESMF_TESTDIR)/ESMCI_Proj4UTest

# TESTS_BUILD   = $(ESMF_TESTDIR)/ESMCI_MeshCapUTest
# TESTS_BUILD   = $(ESMF_TESTDIR)/ESMCI_MeshCapRegridUTest
# TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_MeshUTest

TESTS_RUN    =  \
                RUN_ESMCI_MBMesh_ExtrapolateUTest \
                RUN_ESMCI_MBMesh_UtilUTest \
                RUN_ESMCI_MeshCapUTest \
                RUN_ESMCI_MeshCapRegridUTest \
                RUN_ESMCI_MeshMOABUTest \
                RUN_ESMCI_IntegrateUTest \
                RUN_ESMCI_MeshUTest \
                RUN_ESMCI_PgonUTest \
                RUN_ESMC_MeshVTKUTest \
                RUN_ESMF_MeshOpUTest \
                RUN_ESMF_MeshUTest \
                RUN_ESMF_MeshFileIOUTest \
                RUN_ESMCI_NearestUTest \
                RUN_ESMCI_Proj4UTest

TESTS_RUN_UNI = \
                RUN_ESMCI_MBMesh_UtilUTestUNI \
                RUN_ESMCI_MeshCapUTestUNI \
                RUN_ESMCI_MeshCapRegridUTestUNI \
                RUN_ESMCI_MeshMOABUTestUNI \
                RUN_ESMCI_IntegrateUTestUNI \
                RUN_ESMCI_MeshUTestUNI \
                RUN_ESMCI_PgonUTestUNI \
                RUN_ESMF_MeshOpUTestUNI \
                RUN_ESMF_MeshUTestUNI \
                RUN_ESMF_MeshFileIOUTestUNI \
                RUN_ESMCI_Proj4UTestUNI

include ${ESMF_DIR}/makefile

CLEANDIRS   =
CLEANFILES  = $(TESTS_BUILD) \
              $(ESMF_TESTDIR)/ESMCI_MeshCapGenUTest \
              $(ESMF_TESTDIR)/ESMCI_MeshCapGenUTest \
              $(ESMF_DIR)/$(LOCDIR)/ESMCI_*GenUTest.C
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

RUN_ESMF_MeshFileIOUTest:
	cp -r data $(ESMF_TESTDIR)
	$(MAKE) TNAME=MeshFileIO NP=4 ftest

RUN_ESMF_MeshFileIOUTestUNI:
	cp -r data $(ESMF_TESTDIR)
	$(MAKE) TNAME=MeshFileIO NP=1 ftest

RUN_ESMC_MeshVTKUTest:
	cp -r data $(ESMF_TESTDIR)
	chmod u+rw $(ESMF_TESTDIR)/data/*
	$(MAKE) TNAME=MeshVTK NP=6 ctest

RUN_ESMCI_MeshUTest:
	cp -r data $(ESMF_TESTDIR)
	$(MAKE) TNAME=Mesh NP=1 citest

RUN_ESMCI_MeshUTestUNI:
	cp -r data $(ESMF_TESTDIR)
	$(MAKE) TNAME=Mesh NP=1 citest

RUN_ESMCI_PgonUTest:
	cp -r data $(ESMF_TESTDIR)
	$(MAKE) TNAME=Pgon NP=1 citest

RUN_ESMCI_PgonUTestUNI:
	cp -r data $(ESMF_TESTDIR)
	$(MAKE) TNAME=Pgon NP=1 citest

RUN_ESMCI_MeshMOABUTest:
	$(MAKE) TNAME=MeshMOAB NP=1 citest

RUN_ESMCI_MeshMOABUTestUNI:
	$(MAKE) TNAME=MeshMOAB NP=1 citest

RUN_ESMCI_Proj4UTest:
	$(MAKE) TNAME=Proj4 NP=1 citest

RUN_ESMCI_Proj4UTestUNI:
	$(MAKE) TNAME=Proj4 NP=1 citest


RUN_ESMCI_MeshCapUTest:
	# copy executable to name of the alternate test file (in execution directory)
	cp $(ESMF_TESTDIR)/ESMCI_MeshCapUTest $(ESMF_TESTDIR)/ESMCI_MeshCapGenUTest
	# remove the old alternate test file in the source directory
	rm -f $(ESMF_DIR)/$(LOCDIR)/ESMCI_MeshCapGenUTest.C
	# copy the source file to the execution directory under the new name
	cp $(ESMF_DIR)/$(LOCDIR)/ESMCI_MeshCapUTest.C $(ESMF_TESTDIR)/ESMCI_MeshCapGenUTest.C
	# run the test
	$(MAKE) TNAME=MeshCapGen NP=4 citest
	# move the updated source file from the execution directory to the source directory
	mv $(ESMF_TESTDIR)/ESMCI_MeshCapGenUTest.C $(ESMF_DIR)/$(LOCDIR)/ESMCI_MeshCapGenUTest.C

RUN_ESMCI_MeshCapUTestUNI:
	# copy executable to name of the alternate test file (in execution directory)
	cp $(ESMF_TESTDIR)/ESMCI_MeshCapUTest $(ESMF_TESTDIR)/ESMCI_MeshCapGenUTest
	# remove the old alternate test file in the source directory
	rm -f $(ESMF_DIR)/$(LOCDIR)/ESMCI_MeshCapGenUTest.C
	# copy the source file to the execution directory under the new name
	cp $(ESMF_DIR)/$(LOCDIR)/ESMCI_MeshCapUTest.C $(ESMF_TESTDIR)/ESMCI_MeshCapGenUTest.C
	# run the test
	$(MAKE) TNAME=MeshCapGen NP=1 citest
	# move the updated source file from the execution directory to the source directory
	mv $(ESMF_TESTDIR)/ESMCI_MeshCapGenUTest.C $(ESMF_DIR)/$(LOCDIR)/ESMCI_MeshCapGenUTest.C


RUN_ESMCI_MeshCapRegridUTest:
	# copy executable to name of the alternate test file (in execution directory)
	cp $(ESMF_TESTDIR)/ESMCI_MeshCapRegridUTest $(ESMF_TESTDIR)/ESMCI_MeshCapRegridGenUTest
	# remove the old alternate test file in the source directory
	rm -f $(ESMF_DIR)/$(LOCDIR)/ESMCI_MeshCapRegridGenUTest.C
	# copy the source file to the execution directory under the new name
	cp $(ESMF_DIR)/$(LOCDIR)/ESMCI_MeshCapRegridUTest.C $(ESMF_TESTDIR)/ESMCI_MeshCapRegridGenUTest.C
	# run the test
	$(MAKE) TNAME=MeshCapRegridGen NP=4 citest
	# move the updated source file from the execution directory to the source directory
	mv $(ESMF_TESTDIR)/ESMCI_MeshCapRegridGenUTest.C $(ESMF_DIR)/$(LOCDIR)/ESMCI_MeshCapRegridGenUTest.C

RUN_ESMCI_MeshCapRegridUTestUNI:
	# copy executable to name of the alternate test file (in execution directory)
	cp $(ESMF_TESTDIR)/ESMCI_MeshCapRegridUTest $(ESMF_TESTDIR)/ESMCI_MeshCapRegridGenUTest
	# remove the old alternate test file in the source directory
	rm -f $(ESMF_DIR)/$(LOCDIR)/ESMCI_MeshCapRegridGenUTest.C
	# copy the source file to the execution directory under the new name
	cp $(ESMF_DIR)/$(LOCDIR)/ESMCI_MeshCapRegridUTest.C $(ESMF_TESTDIR)/ESMCI_MeshCapRegridGenUTest.C
	# run the test
	$(MAKE) TNAME=MeshCapRegridGen NP=1 citest
	# move the updated source file from the execution directory to the source directory
	mv $(ESMF_TESTDIR)/ESMCI_MeshCapRegridGenUTest.C $(ESMF_DIR)/$(LOCDIR)/ESMCI_MeshCapRegridGenUTest.C


RUN_ESMCI_MBMesh_ExtrapolateUTest:
	$(MAKE) TNAME=MBMesh_Extrapolate NP=4 citest


RUN_ESMCI_MBMesh_UtilUTest:
	$(MAKE) TNAME=MBMesh_Util NP=4 citest

RUN_ESMCI_MBMesh_UtilUTestUNI:
	$(MAKE) TNAME=MBMesh_Util NP=1 citest


RUN_ESMCI_NearestUTest:
	$(MAKE) TNAME=Nearest NP=4 citest

