# $Id: makefile,v 1.18 2008/05/08 19:00:37 feiliu Exp $ 

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
LOCDIR	  = src/Infrastructure/FieldComm/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_FieldGatherUTest \
	$(ESMF_TESTDIR)/ESMF_FieldRedistUTest
               
TESTS_RUN     = RUN_ESMF_FieldGatherUTest \
	RUN_ESMF_FieldRedistUTest
                
TESTS_RUN_UNI = 

include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD) $(CPFILES)
CLOBBERDIRS =

#
# Field
#

RUN_ESMF_FieldGatherUTest:
	$(MAKE) TNAME=FieldGather NP=4 ftest

RUN_ESMF_FieldRedistUTest:
	$(MAKE) TNAME=FieldRedist NP=4 ftest
