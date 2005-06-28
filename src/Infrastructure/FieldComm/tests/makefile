# $Id: makefile,v 1.13 2005/06/28 20:29:18 svasquez Exp $

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
	-$(RM) *FieldGatherUTest.Log
	-${MPIRUN} -np 6 $(ESMC_TESTDIR)/ESMF_FieldGatherUTest > $(ESMC_TESTDIR)/ESMF_FieldGatherUTest.stdout
	-cat *FieldGatherUTest.Log > $(ESMC_TESTDIR)/ESMF_FieldGatherUTest.Log
	-$(RM) *FieldGatherUTest.Log


RUN_ESMF_FieldGatherUTestUNI:
	echo "PASS: ESMF_FieldGatherUTest not set up to run on one processor." > $(ESMF_TESTDIR)/ESMF_FieldGatherUTest.stdout


RUN_ESMF_FieldGather1DUTest:
	-$(RM) *FieldGather1DUTest.Log
	-${MPIRUN} -np 6 $(ESMC_TESTDIR)/ESMF_FieldGather1DUTest > $(ESMC_TESTDIR)/ESMF_FieldGather1DUTest.stdout
	-cat *FieldGather1DUTest.Log > $(ESMC_TESTDIR)/ESMF_FieldGather1DUTest.Log
	-$(RM) *FieldGather1DUTest.Log


RUN_ESMF_FieldGather1DUTestUNI:
	-$(RM) *FieldGather1DUTest.Log
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_FieldGather1DUTest > $(ESMC_TESTDIR)/ESMF_FieldGather1DUTest.stdout
	-cat *FieldGather1DUTest.Log > $(ESMC_TESTDIR)/ESMF_FieldGather1DUTest.Log
	-$(RM) *FieldGather1DUTest.Log

