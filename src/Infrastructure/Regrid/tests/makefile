# $Id: makefile,v 1.44 2008/02/25 21:33:53 dneckels Exp $

ALL:  build_unit_tests
run:  run_unit_tests

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Infrastructure/Regrid/tests

TESTS_OBJ = 

ESMF_RegridUTest.o:$(TESTS_OBJ)

TESTS_BUILD   = \
                $(ESMF_TESTDIR)/ESMF_RegridUTest

TESTS_RUN     = 

TESTS_RUN_UNI =	

include $(ESMF_DIR)/makefile


DIRS =

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
# Regrid Unit Test
#
RUN_ESMF_RegridUTest:
	$(MAKE) TNAME=Regrid NP=8 ftest

RUN_ESMF_RegridUTestUNI:
	$(MAKE) TNAME=Regrid NP=1 ftest



