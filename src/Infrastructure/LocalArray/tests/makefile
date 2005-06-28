# $Id: makefile,v 1.10 2005/06/28 20:37:07 svasquez Exp $ 

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
LOCDIR	  = src/Infrastructure/LocalArray/tests


TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_ArrayDataUTest \
		$(ESMF_TESTDIR)/ESMF_LocalArrayUTest 

TESTS_RUN     = RUN_ESMF_ArrayDataUTest \
		RUN_ESMF_LocalArrayUTest 

TESTS_RUN_UNI = RUN_ESMF_ArrayDataUTestUNI \
		RUN_ESMF_LocalArrayUTestUNI



include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD) TestArrayData
CLOBBERDIRS =



#
#  ESMF_ArrayDataUTest
#
RUN_ESMF_ArrayDataUTest:
	-$(RM) *ArrayDataUTest.Log
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_ArrayDataUTest > $(ESMF_TESTDIR)/ESMF_ArrayDataUTest.stdout
	-cat *ArrayDataUTest.Log > $(ESMC_TESTDIR)/ESMF_ArrayDataUTest.Log
	-$(RM) *ArrayDataUTest.Log

RUN_ESMF_ArrayDataUTestUNI:
	-$(RM) *ArrayDataUTest.Log
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_ArrayDataUTest > $(ESMF_TESTDIR)/ESMF_ArrayDataUTest.stdout
	-cat *ArrayDataUTest.Log > $(ESMC_TESTDIR)/ESMF_ArrayDataUTest.Log
	-$(RM) *ArrayDataUTest.Log


#
#  ESMF_LocalArrayUTest
#
RUN_ESMF_LocalArrayUTest:
	-$(RM) *LocalArrayUTest.Log
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_LocalArrayUTest > $(ESMF_TESTDIR)/ESMF_LocalArrayUTest.stdout
	-cat *LocalArrayUTest.Log > $(ESMC_TESTDIR)/ESMF_LocalArrayUTest.Log
	-$(RM) *LocalArrayUTest.Log

RUN_ESMF_LocalArrayUTestUNI:
	-$(RM) *LocalArrayUTest.Log
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_LocalArrayUTest > $(ESMF_TESTDIR)/ESMF_LocalArrayUTest.stdout
	-cat *LocalArrayUTest.Log > $(ESMC_TESTDIR)/ESMF_LocalArrayUTest.Log
	-$(RM) *LocalArrayUTest.Log
