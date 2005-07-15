# $Id: makefile,v 1.3 2005/07/15 03:41:48 nscollins Exp $ 

ALL:  build_unit_tests
run:  tree_run_unit_tests

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/prologue/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_StringUTest \
                $(ESMF_TESTDIR)/ESMF_ArrayF90PtrUTest

TESTS_RUN     = RUN_ESMF_StringUTest \
                RUN_ESMF_ArrayF90PtrUTest

TESTS_RUN_UNI = RUN_ESMF_StringUTestUNI \
                RUN_ESMF_ArrayF90PtrUTestUNI

include ${ESMF_DIR}/makefile

DIRS        =

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
# Special for strings passed between C++ and F90
#  this is NOT an MPI program and can just be run from the command line
#
UTEST_String_OBJS = ESMC_StringSubr.o
ESMF_StringUTest.o : ESMC_StringSubr.o

RUN_ESMF_StringUTest:
	-$(ESMF_TESTDIR)/ESMF_StringUTest > $(ESMF_TESTDIR)/ESMF_StringUTest.stdout

RUN_ESMF_StringUTestUNI:
	-$(ESMF_TESTDIR)/ESMF_StringUTest > $(ESMF_TESTDIR)/ESMF_StringUTest.stdout


#
#  Fortran 90 pointer size test.  This must pass before any of the
#  ESMF Array tests will give valid results.  If this test does not pass
#  adjust the #define values in $ESMF_DIR/build_config/your_arch/ESMC_Conf.h
#  The values ESMF_F90_PTR_BASE_SIZE and ESMF_F90_PTR_PLUS_RANK are the
#  ones to change.
#

RUN_ESMF_ArrayF90PtrUTest:
	$(MAKE) TNAME=ArrayF90Ptr NP=4 ftest

RUN_ESMF_ArrayF90PtrUTestUNI:
	$(MAKE) TNAME=ArrayF90Ptr NP=1 ftest



