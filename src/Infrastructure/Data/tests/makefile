# $Id: makefile,v 1.15 2003/05/23 16:12:46 flanigan Exp $ 

ALL:  tests

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Infrastructure/Data/tests


TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_ArrayBasicUTest \
		$(ESMF_TESTDIR)/ESMF_ArrayF90PtrUTest \
		$(ESMF_TESTDIR)/ESMF_ArrayUTest 

TESTS_RUN     = RUN_ESMF_ArrayBasicUTest \
		RUN_ESMF_ArrayF90PtrUTest \
		RUN_ESMF_ArrayUTest 

TESTS_RUN_UNI = RUN_ESMF_ArrayBasicUTestUNI \
		RUN_ESMF_ArrayF90PtrUTestUNI \
		RUN_ESMF_ArrayUTestUNI


TESTS_RM      = 


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod $(TESTS_BUILD) NULL foo *.txt core stdoutfiles
CLOBBERDIRS =


#
#  ESMF_Arrayf90PtrUTest
#
RUN_ESMF_ArrayF90PtrUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_ArrayF90PtrUTest > $(ESMF_TESTDIR)/ESMF_ArrayF90PtrUTest.stdout

RUN_ESMF_ArrayF90PtrUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_ArrayF90PtrUTest > $(ESMF_TESTDIR)/ESMF_ArrayF90PtrUTest.stdout

ESMF_ArrayF90PtrUTest.rm:
	-@echo "ESMF_ArrayF90Ptr.rm target called here."
#
#  ESMF_ArrayUTest
#
RUN_ESMF_ArrayUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_ArrayUTest > $(ESMF_TESTDIR)/ESMF_ArrayUTest.stdout

RUN_ESMF_ArrayUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_ArrayUTest > $(ESMF_TESTDIR)/ESMF_ArrayUTest.stdout

ESMF_ArrayUTest.rm:
	-@echo "ESMF_ArrayTest.rm target called here."

#
#  ESMF_ArrayBasicUTest
#
RUN_ESMF_ArrayBasicUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_ArrayBasicUTest > $(ESMF_TESTDIR)/ESMF_ArrayBasicUTest.stdout

RUN_ESMF_ArrayBasicUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_ArrayBasicUTest > $(ESMF_TESTDIR)/ESMF_ArrayBasicUTest.stdout

ESMF_ArrayBasic.rm:
	-@echo "ESMF_ArrayBasic.rm target called here."


