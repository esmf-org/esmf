# $Id: makefile,v 1.1 2008/01/23 19:38:24 oehmke Exp $ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

CFLAGS    = 
FFLAGS    = 
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
DIRS	  = 
LOCDIR	  = src/Infrastructure/GridUtil/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_GridToMeshUTest 


TESTS_RUN     = RUN_ESMF_GridToMeshUTest 

TESTS_RUN_UNI = RUN_ESMF_GridToMeshUTestUNI 




include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
# unit test targets
#

# --- GridToMesh test

RUN_ESMF_GridToMeshUTest:
	$(MAKE) TNAME=GridToMesh NP=4 ftest

RUN_ESMF_GridCreateUTestUNI:
	$(MAKE) TNAME=GridToMesh NP=1 ftest

