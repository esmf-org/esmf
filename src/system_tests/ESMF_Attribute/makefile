
ALL:  tree_build_use_test_cases
run:  tree_run_use_test_cases

FFLAGS    =
CFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/use_test_cases/ESMF_WaterML

USE_TEST_CASES_BUILD       = $(ESMF_TESTDIR)/ESMF_WaterMLUseTestCase

USE_TEST_CASES_OBJ         = user_model1.o user_model2.o user_coupler.o

USE_TEST_CASES_RUN     = RUN_WaterML
USE_TEST_CASES_RUN_UNI = RUN_WaterMLUNI

include ${ESMF_DIR}/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(TESTS_BUILD) *.stdout *.xml
CLOBBERDIRS =

ESMF_WaterMLUseTestCase.o : $(USE_TEST_CASES_OBJ)

RUN_WaterML:
	@if [ $(ESMF_MAX_PROCS)foo = foo ] || [ "$(ESMF_MAX_PROCS)" -ge "6" ] ; then \
		$(MAKE) TNAME=WaterML  NP=6 uctest ; \
		mv $(ESMF_TESTDIR)/ESMF_WaterMLUseTestCase.stdout $(ESMF_TESTDIR)/ESMF_WaterMLUseTestCase.stdout6 ; \
		echo "ESMF_WaterMLUseTestCase:6:"  > $(ESMF_TESTDIR)/ESMF_WaterMLUseTestCase.testlist ; \
	else \
		rm -f $(ESMF_TESTDIR)/ESMF_WaterMLUseTestCase.testlist ; \
	fi; \


RUN_WaterMLUNI:
	$(MAKE) TNAME=WaterML NP=1 uctest
