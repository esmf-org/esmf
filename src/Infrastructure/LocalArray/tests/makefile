# $Id: makefile,v 1.3 2004/08/03 19:33:49 nscollins Exp $ 

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
		$(ESMF_TESTDIR)/ESMF_LocalArrayUTest 

TESTS_RUN     = RUN_ESMF_ArrayF90PtrUTest \
		RUN_ESMF_LocalArrayUTest 

TESTS_RUN_UNI = RUN_ESMF_ArrayF90PtrUTestUNI \
		RUN_ESMF_LocalArrayUTestUNI


TESTS_RM      = 


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod $(TESTS_BUILD) NULL foo *.txt core \
              ESMF_LogFile stdoutfiles
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
#  ESMF_LocalArrayUTest
#
RUN_ESMF_LocalArrayUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_LocalArrayUTest > $(ESMF_TESTDIR)/ESMF_LocalArrayUTest.stdout

RUN_ESMF_LocalArrayUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_LocalArrayUTest > $(ESMF_TESTDIR)/ESMF_LocalArrayUTest.stdout

ESMF_ArrayUTest.rm:
	-@echo "ESMF_LocalArrayTest.rm target called here."


