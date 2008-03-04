# $Id: makefile,v 1.22 2008/03/04 03:43:30 rokuingh Exp $ 

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

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_GridCreateUTest \
	        $(ESMF_TESTDIR)/ESMF_GridCoordUTest \
		$(ESMF_TESTDIR)/ESMF_GridAttrUTest



TESTS_RUN     = RUN_ESMF_GridCreateUTest \
	        RUN_ESMF_GridCoordUTest \
		RUN_ESMF_GridAttrUTest

TESTS_RUN_UNI = RUN_ESMF_GridCreateUTestUNI \
	        RUN_ESMF_GridCoordUTestUNI \
		RUN_ESMF_GridAttrUTestUNI


include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
# unit test targets
#

# --- Grid Creation

RUN_ESMF_GridCreateUTest:
	$(MAKE) TNAME=GridCreate NP=4 ftest

RUN_ESMF_GridCreateUTestUNI:
	$(MAKE) TNAME=GridCreate NP=1 ftest


# --- Coord Manipulation

RUN_ESMF_GridCoordUTest:
	$(MAKE) TNAME=GridCoord NP=4 ftest

RUN_ESMF_GridCoordUTestUNI:
	$(MAKE) TNAME=GridCoord NP=1 ftest 

# --- Grid Attributes

RUN_ESMF_GridAttrUTest:
	$(MAKE) TNAME=GridAttr NP=4 ftest

RUN_ESMF_GridAttrUTestUNI:
	$(MAKE) TNAME=GridAttr NP=1 ftest 

