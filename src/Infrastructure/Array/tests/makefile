# $Id: makefile,v 1.7 2005/06/28 19:58:07 svasquez Exp $ 

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
	-$(RM) *ArrayUTest.Log
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_ArrayUTest > $(ESMF_TESTDIR)/ESMF_ArrayUTest.stdout
	-cat *ArrayUTest.Log > $(ESMC_TESTDIR)/ESMF_ArrayUTest.Log
	-$(RM) *ArrayUTest.Log

RUN_ESMF_ArrayUTestUNI:
	-$(RM) *ArrayUTest.Log
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_ArrayUTest > $(ESMF_TESTDIR)/ESMF_ArrayUTest.stdout
	-cat *ArrayUTest.Log > $(ESMC_TESTDIR)/ESMF_ArrayUTest.Log
	-$(RM) *ArrayUTest.Log

ESMF_ArrayUTest.rm:
	-@echo "ESMF_Array.rm target called here."


