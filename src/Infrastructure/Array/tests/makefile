# $Id: makefile,v 1.5 2005/04/07 22:09:56 svasquez Exp $ 

ALL:  build_tests

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Infrastructure/Array/tests


TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_ArrayUTest

TESTS_RUN     = RUN_ESMF_ArrayUTest

TESTS_RUN_UNI = RUN_ESMF_ArrayUTestUNI


TESTS_RM      = 


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD) 
CLOBBERDIRS =


#
#  ESMF_ArrayUTest
#
RUN_ESMF_ArrayUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_ArrayUTest > $(ESMF_TESTDIR)/ESMF_ArrayUTest.stdout
	-cat *ArrayUTest.Log > $(ESMC_TESTDIR)/ESMF_ArrayUTest.Log
	-rm *ArrayUTest.Log

RUN_ESMF_ArrayUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_ArrayUTest > $(ESMF_TESTDIR)/ESMF_ArrayUTest.stdout
	-cat *ArrayUTest.Log > $(ESMC_TESTDIR)/ESMF_ArrayUTest.Log
	-rm *ArrayUTest.Log

ESMF_ArrayUTest.rm:
	-@echo "ESMF_Array.rm target called here."


