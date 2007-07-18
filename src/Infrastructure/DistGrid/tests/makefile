# $Id: makefile,v 1.1 2007/07/18 05:36:52 theurich Exp $ 

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
LOCDIR	  = src/Infrastructure/DistGrid/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_DistGridCreateGetUTest

TESTS_RUN     = RUN_ESMF_DistGridCreateGetUTest

TESTS_RUN_UNI = RUN_ESMF_DistGridCreateGetUTestUNI


include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
# unit test targets
#

RUN_ESMF_DistGridCreateGetUTest:
	$(MAKE) TNAME=DistGridCreateGet NP=4 ftest

RUN_ESMF_DistGridCreateGetUTestUNI:
	$(MAKE) TNAME=DistGridCreateGet NP=1 ftest

