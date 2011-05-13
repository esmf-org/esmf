# $Id: makefile,v 1.4 2011/05/13 22:57:38 svasquez Exp $ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Superstructure/WebServices/tests

TESTS_BUILD   =	$(ESMF_TESTDIR)/ESMF_WebServUTest \
                  $(ESMF_TESTDIR)/ESMC_WebServCompSvrClientUTest

TESTS_RUN     =	RUN_ESMC_WebServCompSvrClientUTest
		#RUN_ESMF_WebServUTest \

TESTS_RUN_UNI =	 RUN_ESMC_WebServCompSvrClientUTestUNI
		#RUN_ESMF_WebServUTestUNI \


include $(ESMF_DIR)/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

DIRS        = 


#
# Web Services
#

RUN_ESMF_WebServUTest:
	$(MAKE) TNAME=WebServ NP=3 ftest & \
   sleep 3 

RUN_ESMF_WebServUTestUNI:
	$(MAKE) TNAME=WebServ NP=1 ftest & \
   sleep 3


RUN_ESMC_WebServCompSvrClientUTest:
	$(MAKE) TNAME=WebServCompSvrClient NP=1 ctest

RUN_ESMC_WebServCompSvrClientUTestUNI:
	$(MAKE) TNAME=WebServCompSvrClient NP=1 ctest

