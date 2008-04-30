# $Id: makefile,v 1.17 2008/04/30 17:27:42 feiliu Exp $ 

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

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_FieldGatherUTest
               
TESTS_RUN     = RUN_ESMF_FieldGatherUTest
                
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
