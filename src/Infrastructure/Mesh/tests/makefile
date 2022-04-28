# $Id$

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR    = src/Infrastructure/Mesh/tests

.NOTPARALLEL:

TESTS_BUILD	  = \
                $(ESMF_TESTDIR)/ESMC_MBMesh_ExtrapolateUTest \
                $(ESMF_TESTDIR)/ESMC_MBMesh_UtilUTest \
                $(ESMF_TESTDIR)/ESMC_MeshCapUTest \
                $(ESMF_TESTDIR)/ESMC_MeshCapRegridUTest \
                $(ESMF_TESTDIR)/ESMC_MeshMOABUTest \
                $(ESMF_TESTDIR)/ESMCI_IntegrateUTest \
                $(ESMF_TESTDIR)/ESMC_MeshUTest \
                $(ESMF_TESTDIR)/ESMC_MeshVTKUTest \
                $(ESMF_TESTDIR)/ESMF_MeshOpUTest \
                $(ESMF_TESTDIR)/ESMF_MeshUTest \
                $(ESMF_TESTDIR)/ESMC_NearestUTest \
                $(ESMF_TESTDIR)/ESMF_MeshFileIOUTest \
                $(ESMF_TESTDIR)/ESMC_Proj4UTest

# TESTS_BUILD   = $(ESMF_TESTDIR)/ESMC_MeshCapUTest
# TESTS_BUILD   = $(ESMF_TESTDIR)/ESMC_MeshCapRegridUTest
# TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_MeshUTest

TESTS_RUN    =  \
                RUN_ESMC_MBMesh_ExtrapolateUTest \
                RUN_ESMC_MBMesh_UtilUTest \
                RUN_ESMC_MeshCapUTest \
                RUN_ESMC_MeshCapRegridUTest \
                RUN_ESMC_MeshMOABUTest \
                RUN_ESMCI_IntegrateUTest \
                RUN_ESMC_MeshUTest \
                RUN_ESMC_MeshVTKUTest \
                RUN_ESMF_MeshOpUTest \
                RUN_ESMF_MeshUTest \
                RUN_ESMF_MeshFileIOUTest \
                RUN_ESMC_NearestUTest \
                RUN_ESMC_Proj4UTest

TESTS_RUN_UNI = \
                RUN_ESMC_MBMesh_UtilUTestUNI \
                RUN_ESMC_MeshCapUTestUNI \
                RUN_ESMC_MeshCapRegridUTestUNI \
                RUN_ESMC_MeshMOABUTestUNI \
                RUN_ESMCI_IntegrateUTestUNI \
                RUN_ESMC_MeshUTestUNI \
                RUN_ESMF_MeshOpUTestUNI \
                RUN_ESMF_MeshUTestUNI \
                RUN_ESMF_MeshFileIOUTestUNI \
                RUN_ESMC_Proj4UTestUNI

include ${ESMF_DIR}/makefile

CLEANDIRS   =
CLEANFILES  = $(TESTS_BUILD) \
              $(ESMF_TESTDIR)/ESMC_MeshCapGenUTest \
              $(ESMF_TESTDIR)/ESMC_MeshCapGenUTest \
              $(ESMF_DIR)/$(LOCDIR)/ESMC_*GenUTest.C
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


RUN_ESMC_MeshCapUTest:
	# copy executable to name of the alternate test file (in execution directory)
	cp $(ESMF_TESTDIR)/ESMC_MeshCapUTest $(ESMF_TESTDIR)/ESMC_MeshCapGenUTest
	# remove the old alternate test file in the source directory
	rm -f $(ESMF_DIR)/$(LOCDIR)/ESMC_MeshCapGenUTest.C
	# copy the source file to the execution directory under the new name
	cp $(ESMF_DIR)/$(LOCDIR)/ESMC_MeshCapUTest.C $(ESMF_TESTDIR)/ESMC_MeshCapGenUTest.C
	# run the test
	$(MAKE) TNAME=MeshCapGen NP=4 ctest
	# move the updated source file from the execution directory to the source directory
	mv $(ESMF_TESTDIR)/ESMC_MeshCapGenUTest.C $(ESMF_DIR)/$(LOCDIR)/ESMC_MeshCapGenUTest.C

RUN_ESMC_MeshCapUTestUNI:
	# copy executable to name of the alternate test file (in execution directory)
	cp $(ESMF_TESTDIR)/ESMC_MeshCapUTest $(ESMF_TESTDIR)/ESMC_MeshCapGenUTest
	# remove the old alternate test file in the source directory
	rm -f $(ESMF_DIR)/$(LOCDIR)/ESMC_MeshCapGenUTest.C
	# copy the source file to the execution directory under the new name
	cp $(ESMF_DIR)/$(LOCDIR)/ESMC_MeshCapUTest.C $(ESMF_TESTDIR)/ESMC_MeshCapGenUTest.C
	# run the test
	$(MAKE) TNAME=MeshCapGen NP=1 ctest
	# move the updated source file from the execution directory to the source directory
	mv $(ESMF_TESTDIR)/ESMC_MeshCapGenUTest.C $(ESMF_DIR)/$(LOCDIR)/ESMC_MeshCapGenUTest.C


RUN_ESMC_MeshCapRegridUTest:
	# copy executable to name of the alternate test file (in execution directory)
	cp $(ESMF_TESTDIR)/ESMC_MeshCapRegridUTest $(ESMF_TESTDIR)/ESMC_MeshCapRegridGenUTest
	# remove the old alternate test file in the source directory
	rm -f $(ESMF_DIR)/$(LOCDIR)/ESMC_MeshCapRegridGenUTest.C
	# copy the source file to the execution directory under the new name
	cp $(ESMF_DIR)/$(LOCDIR)/ESMC_MeshCapRegridUTest.C $(ESMF_TESTDIR)/ESMC_MeshCapRegridGenUTest.C
	# run the test
	$(MAKE) TNAME=MeshCapRegridGen NP=4 ctest
	# move the updated source file from the execution directory to the source directory
	mv $(ESMF_TESTDIR)/ESMC_MeshCapRegridGenUTest.C $(ESMF_DIR)/$(LOCDIR)/ESMC_MeshCapRegridGenUTest.C

RUN_ESMC_MeshCapRegridUTestUNI:
	# copy executable to name of the alternate test file (in execution directory)
	cp $(ESMF_TESTDIR)/ESMC_MeshCapRegridUTest $(ESMF_TESTDIR)/ESMC_MeshCapRegridGenUTest
	# remove the old alternate test file in the source directory
	rm -f $(ESMF_DIR)/$(LOCDIR)/ESMC_MeshCapRegridGenUTest.C
	# copy the source file to the execution directory under the new name
	cp $(ESMF_DIR)/$(LOCDIR)/ESMC_MeshCapRegridUTest.C $(ESMF_TESTDIR)/ESMC_MeshCapRegridGenUTest.C
	# run the test
	$(MAKE) TNAME=MeshCapRegridGen NP=1 ctest
	# move the updated source file from the execution directory to the source directory
	mv $(ESMF_TESTDIR)/ESMC_MeshCapRegridGenUTest.C $(ESMF_DIR)/$(LOCDIR)/ESMC_MeshCapRegridGenUTest.C


RUN_ESMC_MBMesh_ExtrapolateUTest:
	$(MAKE) TNAME=MBMesh_Extrapolate NP=4 ctest


RUN_ESMC_MBMesh_UtilUTest:
	$(MAKE) TNAME=MBMesh_Util NP=4 ctest

RUN_ESMC_MBMesh_UtilUTestUNI:
	$(MAKE) TNAME=MBMesh_Util NP=1 ctest


RUN_ESMC_NearestUTest:
	$(MAKE) TNAME=Nearest NP=4 ctest
