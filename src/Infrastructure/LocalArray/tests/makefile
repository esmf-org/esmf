# $Id: makefile,v 1.6 2005/03/01 21:15:46 nscollins Exp $ 

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
LOCDIR	  = src/Infrastructure/LocalArray/tests


TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_ArrayF90PtrUTest \
		$(ESMF_TESTDIR)/ESMF_ArrayDataUTest \
		$(ESMF_TESTDIR)/ESMF_LocalArrayUTest 

TESTS_RUN     = RUN_ESMF_ArrayF90PtrUTest \
		RUN_ESMF_ArrayDataUTest \
		RUN_ESMF_LocalArrayUTest 

TESTS_RUN_UNI = RUN_ESMF_ArrayF90PtrUTestUNI \
		RUN_ESMF_ArrayDataUTestUNI \
		RUN_ESMF_LocalArrayUTestUNI



include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD) TestArrayData
CLOBBERDIRS =


#
#  ESMF_ArrayF90PtrUTest
#
RUN_ESMF_ArrayF90PtrUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_ArrayF90PtrUTest > $(ESMF_TESTDIR)/ESMF_ArrayF90PtrUTest.stdout

RUN_ESMF_ArrayF90PtrUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_ArrayF90PtrUTest > $(ESMF_TESTDIR)/ESMF_ArrayF90PtrUTest.stdout

#
#  ESMF_ArrayDataUTest
#
RUN_ESMF_ArrayDataUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_ArrayDataUTest > $(ESMF_TESTDIR)/ESMF_ArrayDataUTest.stdout

RUN_ESMF_ArrayDataUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_ArrayDataUTest > $(ESMF_TESTDIR)/ESMF_ArrayDataUTest.stdout


#
#  ESMF_LocalArrayUTest
#
RUN_ESMF_LocalArrayUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_LocalArrayUTest > $(ESMF_TESTDIR)/ESMF_LocalArrayUTest.stdout

RUN_ESMF_LocalArrayUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_LocalArrayUTest > $(ESMF_TESTDIR)/ESMF_LocalArrayUTest.stdout
