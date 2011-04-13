# $Id: makefile,v 1.2 2011/04/13 01:28:55 ksaint Exp $ 

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

RUN_ESMF_WebServUTest:
	$(MAKE) TNAME=WebServ NP=4 ftest & \
   sleep 3 

RUN_ESMF_WebServUTestUNI:
	$(MAKE) TNAME=WebServ NP=1 ftest


#ESMC_WebServCompSvrClientUTest.o: ESMC_WebServCompSvrClientUTest.o
#ESMC_UTEST_WebServCompSvrClient_OBJS = ESMC_WebServCompSvrClientUTest.o

RUN_ESMC_WebServCompSvrClientUTest:
	$(MAKE) TNAME=WebServCompSvrClient NP=1 ctest

RUN_ESMC_WebServCompSvrClientUTestUNI:
	$(MAKE) TNAME=WebServCompSvrClient NP=1 ctest

