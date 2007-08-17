# $Id: makefile,v 1.16 2007/08/17 20:53:25 cdeluca Exp $

ALL:  build_unit_tests
run:  run_unit_tests

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Infrastructure/FieldComm/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_FieldGatherUTest \
		$(ESMC_TESTDIR)/ESMF_FieldGather1DUTest

TESTS_RUN     = RUN_ESMF_FieldGatherUTest \
		RUN_ESMF_FieldGather1DUTest

TESTS_RUN_UNI = RUN_ESMF_FieldGatherUTestUNI \
		RUN_ESMF_FieldGather1DUTestUNI


include ${ESMF_DIR}/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

#
# FieldComm unit tests
#
RUN_ESMF_FieldGatherUTest:
	$(MAKE) TNAME=FieldGather NP=6 ftest

RUN_ESMF_FieldGatherUTestUNI:
	echo "PASS: ESMF_FieldGatherUTest not set up to run on one processor." > $(ESMF_TESTDIR)/ESMF_FieldGatherUTest.stdout


RUN_ESMF_FieldGather1DUTest:
	$(MAKE) TNAME=FieldGather1D NP=6 ftest

RUN_ESMF_FieldGather1DUTestUNI:
	$(MAKE) TNAME=FieldGather1D NP=1 ftest

