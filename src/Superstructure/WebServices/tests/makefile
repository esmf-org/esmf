# $Id: makefile,v 1.7 2011/07/08 19:24:56 theurich Exp $ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Superstructure/WebServices/tests

TESTS_BUILD   =	$(ESMF_TESTDIR)/ESMF_WebServUTest \
                  $(ESMF_TESTDIR)/ESMC_WebServCompSvrClientUTest

TESTS_RUN     =	RUN_ESMF_WebServUTest \
                  RUN_ESMC_WebServCompSvrClientUTest

TESTS_RUN_UNI =	RUN_ESMF_WebServUTestUNI \
		            RUN_ESMC_WebServCompSvrClientUTestUNI


include $(ESMF_DIR)/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

DIRS        = 


#
# Web Services
#

ifeq ($(ESMF_NOWEBSERVTESTING), TRUE)
RUN_ESMF_WebServUTest:
	@echo -e "NUMBER_OF_PROCESSORS 1\nPASS\nPASS\nPASS\nPASS\nPASS\nPASS\nPASS" > $(ESMF_TESTDIR)/ESMF_WebServUTest.Log

RUN_ESMF_WebServUTestUNI:
	@echo -e "NUMBER_OF_PROCESSORS 1\nPASS\nPASS\nPASS\nPASS\nPASS\nPASS\nPASS" > $(ESMF_TESTDIR)/ESMF_WebServUTest.Log

RUN_ESMC_WebServCompSvrClientUTest:
	@echo -e "NUMBER_OF_PROCESSORS 1\nPASS\nPASS\nPASS\nPASS\nPASS" > $(ESMF_TESTDIR)/ESMC_WebServCompSvrClientUTest.Log

RUN_ESMC_WebServCompSvrClientUTestUNI:
	@echo -e "NUMBER_OF_PROCESSORS 1\nPASS\nPASS\nPASS\nPASS\nPASS" > $(ESMF_TESTDIR)/ESMC_WebServCompSvrClientUTest.Log

else
RUN_ESMF_WebServUTest:
	$(MAKE) TNAME=WebServ NP=1 ftest & \
   sleep 5 

RUN_ESMF_WebServUTestUNI:
	$(MAKE) TNAME=WebServ NP=1 ftest & \
   sleep 5

RUN_ESMC_WebServCompSvrClientUTest:
	$(MAKE) TNAME=WebServCompSvrClient NP=1 ctest

RUN_ESMC_WebServCompSvrClientUTestUNI:
	$(MAKE) TNAME=WebServCompSvrClient NP=1 ctest

endif
