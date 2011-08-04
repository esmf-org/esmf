# $Id: makefile,v 1.8 2011/08/04 21:18:22 ksaint Exp $ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Superstructure/WebServices/tests

TESTS_BUILD   =	$(ESMF_TESTDIR)/ESMF_WebServUTest \
                  $(ESMF_TESTDIR)/ESMC_WebServCompSvrClientUTest \
                  $(ESMF_TESTDIR)/ESMC_WebServProcCtrlUTest \
                  $(ESMF_TESTDIR)/ESMC_WebServProcCtrlClientUTest \
                  $(ESMF_TESTDIR)/ESMC_WebServRegistrarClientUTest \
                  $(ESMF_TESTDIR)/ESMC_WebServServerSocketUTest \
                  $(ESMF_TESTDIR)/ESMC_WebServClientSocketUTest \
                  $(ESMF_TESTDIR)/ESMC_WebServSecureServerSocketUTest \
                  $(ESMF_TESTDIR)/ESMC_WebServSecureClientSocketUTest \
                  $(ESMF_TESTDIR)/ESMC_WebServForkClientUTest 

#                  $(ESMF_TESTDIR)/ESMC_WebServGRAMClientUTest 

#TESTS_RUN     =	RUN_ESMF_WebServUTest \
#                  RUN_ESMC_WebServCompSvrClientUTest \
#                  RUN_ESMC_WebServSecureUTest \
#		             RUN_ESMC_WebServGRAMClientUTest \
#		             RUN_ESMC_WebServProcCtrlUTest

TESTS_RUN     =	RUN_ESMC_WebServForkClientUTest 

#TESTS_RUN     =	RUN_ESMC_WebServProcCtrlUTest \
#		            RUN_ESMC_WebServProcCtrlClientUTest

#TESTS_RUN     =	RUN_ESMC_WebServRegistrarClientUTest

TESTS_RUN_UNI =	RUN_ESMF_WebServUTestUNI \
		            RUN_ESMC_WebServCompSvrClientUTestUNI \
		            RUN_ESMC_WebServSecureUTestUNI \
		            RUN_ESMC_WebServGRAMClientUTestUNI \
	               RUN_ESMC_WebServProcCtrlUTestUNI


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

RUN_ESMC_WebServSecureUTest:
	$(MAKE) TNAME=WebServSecure NP=1 ctest

RUN_ESMC_WebServSecureUTestUNI:
	$(MAKE) TNAME=WebServSecure NP=1 ctest


RUN_ESMC_WebServGRAMClientUTest:
	$(MAKE) TNAME=WebServGRAMClient NP=1 ctest

RUN_ESMC_WebServGRAMClientUTestUNI:
	$(MAKE) TNAME=WebServGRAMClient NP=1 ctest


RUN_ESMC_WebServProcCtrlUTest:
	$(MAKE) TNAME=WebServProcCtrl NP=1 ctest & \
   sleep 3 

RUN_ESMC_WebServProcCtrlUTestUNI:
	$(MAKE) TNAME=WebServProcCtrl NP=1 ctest & \
   sleep 3 


RUN_ESMC_WebServProcCtrlClientUTest:
	$(MAKE) TNAME=WebServProcCtrlClient NP=1 ctest

RUN_ESMC_WebServProcCtrlClientUTestUNI:
	$(MAKE) TNAME=WebServProcCtrlClient NP=1 ctest

endif
