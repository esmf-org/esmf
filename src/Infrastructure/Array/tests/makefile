# $Id: makefile,v 1.15 2007/07/17 22:16:27 svasquez Exp $ 

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
LOCDIR	  = src/Infrastructure/Array/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_ArrayCreateGetUTest \
                $(ESMF_TESTDIR)/ESMF_ArrayDataUTest \
                $(ESMF_TESTDIR)/ESMF_ArrayScatterUTest \
                $(ESMF_TESTDIR)/ESMF_ArrayArbIdxUTest


TESTS_RUN     = RUN_ESMF_ArrayCreateGetUTest \
                RUN_ESMF_ArrayDataUTest \
                RUN_ESMF_ArrayScatterUTest \
                RUN_ESMF_ArrayArbIdxUTest

TESTS_RUN_UNI = RUN_ESMF_ArrayCreateGetUTestUNI \
                RUN_ESMF_ArrayDataUTestUNI \
                RUN_ESMF_ArrayScatterUTestUNI


include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
# unit test targets
#

RUN_ESMF_ArrayCreateGetUTest:
	$(MAKE) TNAME=ArrayCreateGet NP=4 ftest

RUN_ESMF_ArrayCreateGetUTestUNI:
	$(MAKE) TNAME=ArrayCreateGet NP=1 ftest

# ---

RUN_ESMF_ArrayDataUTest:
	$(MAKE) TNAME=ArrayData NP=4 ftest

RUN_ESMF_ArrayDataUTestUNI:
	$(MAKE) TNAME=ArrayData NP=1 ftest
# ---

RUN_ESMF_ArrayScatterUTest:
	$(MAKE) TNAME=ArrayScatter NP=4 ftest

RUN_ESMF_ArrayScatterUTestUNI:
	$(MAKE) TNAME=ArrayScatter NP=1 ftest
# ---

RUN_ESMF_ArrayArbIdxUTest:
	$(MAKE) TNAME=ArrayArbIdx NP=6 ftest

