# $Id: makefile,v 1.23 2009/06/30 15:41:37 feiliu Exp $ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Infrastructure/FieldComm/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_FieldGatherUTest \
	$(ESMF_TESTDIR)/ESMF_FieldRedistUTest \
	$(ESMF_TESTDIR)/ESMF_FieldLSSMMUTest \
	$(ESMF_TESTDIR)/ESMF_FieldMeshSMMUTest \
	$(ESMF_TESTDIR)/ESMF_FieldSMMUTest
               
TESTS_RUN     = RUN_ESMF_FieldGatherUTest \
	RUN_ESMF_FieldRedistUTest \
	RUN_ESMF_FieldLSSMMUTest \
	RUN_ESMF_FieldMeshSMMUTest \
	RUN_ESMF_FieldSMMUTest
                
TESTS_RUN_UNI = 

include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD) $(CPFILES)
CLOBBERDIRS =

DIRS        = 

#
# Field
#

RUN_ESMF_FieldGatherUTest:
	$(MAKE) TNAME=FieldGather NP=4 ftest

RUN_ESMF_FieldRedistUTest:
	$(MAKE) TNAME=FieldRedist NP=4 ftest

RUN_ESMF_FieldLSSMMUTest:
	$(MAKE) TNAME=FieldLSSMM NP=4 ftest

RUN_ESMF_FieldMeshSMMUTest:
	$(MAKE) TNAME=FieldMeshSMM NP=3 ftest

RUN_ESMF_FieldSMMUTest:
	$(MAKE) TNAME=FieldSMM NP=4 ftest
