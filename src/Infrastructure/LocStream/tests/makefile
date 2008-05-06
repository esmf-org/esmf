# $Id: makefile,v 1.1 2008/05/06 21:42:14 oehmke Exp $ 

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
LOCDIR	  = src/Infrastructure/LocStream/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_LocStreamUTest \


TESTS_RUN     = RUN_ESMF_LocStreamUTest \

TESTS_RUN_UNI = RUN_ESMF_LocStreamUTestUNI \

include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
# unit test targets
#

# --- LocStream Creation

RUN_ESMF_LocStreamUTest:
	$(MAKE) TNAME=LocStream NP=4 ftest

RUN_ESMF_LocStreamUTestUNI:
	$(MAKE) TNAME=LocStream NP=1 ftest



