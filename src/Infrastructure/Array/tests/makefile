# $Id: makefile,v 1.1 2004/04/08 19:22:23 svasquez Exp $ 

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

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod $(TESTS_BUILD) NULL foo *.txt core stdoutfiles
CLOBBERDIRS =


#
#  ESMF_ArrayUTest
#
RUN_ESMF_ArrayUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_ArrayUTest > $(ESMF_TESTDIR)/ESMF_ArrayUTest.stdout

RUN_ESMF_ArrayF90PtrUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_ArrayUTest > $(ESMF_TESTDIR)/ESMF_ArrayUTest.stdout

ESMF_ArrayF90PtrUTest.rm:
	-@echo "ESMF_Array.rm target called here."


