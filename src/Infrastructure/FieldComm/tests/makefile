# $Id: makefile,v 1.12 2005/04/13 21:36:20 svasquez Exp $

ALL:  build_unit_tests

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
	-${MPIRUN} -np 6 $(ESMC_TESTDIR)/ESMF_FieldGatherUTest > $(ESMC_TESTDIR)/ESMF_FieldGatherUTest.stdout
	-cat *FieldGatherUTest.Log > $(ESMC_TESTDIR)/ESMF_FieldGatherUTest.Log
	-rm *FieldGatherUTest.Log


RUN_ESMF_FieldGatherUTestUNI:
	echo "PASS: ESMF_FieldGatherUTest not set up to run on one processor." > $(ESMF_TESTDIR)/ESMF_FieldGatherUTest.stdout


RUN_ESMF_FieldGather1DUTest:
	-${MPIRUN} -np 6 $(ESMC_TESTDIR)/ESMF_FieldGather1DUTest > $(ESMC_TESTDIR)/ESMF_FieldGather1DUTest.stdout
	-cat *FieldGather1DUTest.Log > $(ESMC_TESTDIR)/ESMF_FieldGather1DUTest.Log
	-rm *FieldGather1DUTest.Log


RUN_ESMF_FieldGather1DUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_FieldGather1DUTest > $(ESMC_TESTDIR)/ESMF_FieldGather1DUTest.stdout
	-cat *FieldGather1DUTest.Log > $(ESMC_TESTDIR)/ESMF_FieldGather1DUTest.Log
	-rm *FieldGather1DUTest.Log

