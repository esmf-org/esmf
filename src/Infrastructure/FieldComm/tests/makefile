# $Id: makefile,v 1.11 2005/04/12 16:52:44 nscollins Exp $

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

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_FieldGatherUTest

TESTS_RUN     = RUN_ESMF_FieldGatherUTest

TESTS_RUN_UNI = RUN_ESMF_FieldGatherUTestUNI


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

