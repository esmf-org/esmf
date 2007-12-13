# $Id: makefile,v 1.1 2007/12/13 17:14:48 feiliu Exp $ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

CFLAGS    = 
FFLAGS    = 
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
DIRS	  = 
LOCDIR	  = src/Infrastructure/ArraySpec/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_ArraySpecUTest



TESTS_RUN     = RUN_ESMF_ArraySpecUTest

TESTS_RUN_UNI = RUN_ESMF_ArraySpecUTestUNI

include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
# unit test targets
#

# --- ArraySpec Test

RUN_ESMF_ArraySpecUTest:
	$(MAKE) TNAME=ArraySpec NP=4 ftest

RUN_ESMF_ArraySpecUTestUNI:
	$(MAKE) TNAME=ArraySpec NP=1 ftest
