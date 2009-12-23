# $Id: makefile,v 1.17.2.1 2009/12/23 19:44:29 svasquez Exp $ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/prologue/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_StringUTest \
                $(ESMF_TESTDIR)/ESMF_WordsizeUTest \
                $(ESMF_TESTDIR)/ESMF_F90ArrayPtrUTest \
		$(ESMF_TESTDIR)/ESMF_LAPACKUTest
#		$(ESMF_TESTDIR)/ESMF_F95PtrUTest

TESTS_RUN     = RUN_ESMF_StringUTest \
                RUN_ESMF_WordsizeUTest \
                RUN_ESMF_F90ArrayPtrUTest \
		RUN_ESMF_LAPACKUTest
#		RUN_ESMF_F95PtrUTest

TESTS_RUN_UNI = RUN_ESMF_StringUTestUNI \
                RUN_ESMF_WordsizeUTestUNI \
                RUN_ESMF_F90ArrayPtrUTestUNI \
		RUN_ESMF_LAPACKUTestUNI
#		RUN_ESMF_F95PtrUTestUNI

include ${ESMF_DIR}/makefile

DIRS        =

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
# Special for strings passed between C++ and F90
#

ESMF_UTEST_String_OBJS = ESMC_StringSubr.o
ESMF_StringUTest.o : ESMC_StringSubr.o

RUN_ESMF_StringUTest:
	$(MAKE) TNAME=String NP=2 ftest

RUN_ESMF_StringUTestUNI:
	$(MAKE) TNAME=String NP=1 ftest


#
#  Fortran 90 array pointer size test.  This must pass before any of the
#  ESMF LocalArray code can be assumed safe. If this test does not pass
#  please report it to esmf_support@list.woc.noaa.gov.
#

RUN_ESMF_F90ArrayPtrUTest:
	$(MAKE) TNAME=F90ArrayPtr NP=4 ftest

RUN_ESMF_F90ArrayPtrUTestUNI:
	$(MAKE) TNAME=F90ArrayPtr NP=1 ftest



#
# Special for testing wordsizes
#

ESMF_UTEST_Wordsize_OBJS = ESMC_WordsizeSubr.o
ESMF_WordsizeUTest.o : ESMC_WordsizeSubr.o

RUN_ESMF_WordsizeUTest:
	$(MAKE) TNAME=Wordsize NP=2 ftest

RUN_ESMF_WordsizeUTestUNI:
	$(MAKE) TNAME=Wordsize NP=1 ftest

#
# Test for pointers to derived types changing size based on type
#

ESMF_UTEST_F95Ptr_OBJS =	ESMF_F95PtrBData.o
ESMF_F95PtrUTest.o:	ESMF_F95PtrBData.o

RUN_ESMF_F95PtrUTest:
	$(MAKE) TNAME=F95Ptr NP=2 ftest

RUN_ESMF_F95PtrUTestUNI:
	$(MAKE) TNAME=F95Ptr NP=1 ftest


#
# Demonstrate that LAPACK is linkable and works
#

RUN_ESMF_LAPACKUTest:
	$(MAKE) TNAME=LAPACK NP=2 ftest

RUN_ESMF_LAPACKUTestUNI:
	$(MAKE) TNAME=LAPACK NP=1 ftest
