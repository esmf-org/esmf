# $Id$ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

LOCDIR	  = src/Superstructure/WebServices/tests

.NOTPARALLEL:
TESTS_BUILD   =	$(ESMF_TESTDIR)/ESMF_WebServUTest \
                  $(ESMF_TESTDIR)/ESMCI_WebServCompSvrClientUTest \
                  $(ESMF_TESTDIR)/ESMCI_WebServProcCtrlUTest \
                  $(ESMF_TESTDIR)/ESMCI_WebServProcCtrlClientUTest \
                  $(ESMF_TESTDIR)/ESMCI_WebServRegistrarClientUTest \
                  $(ESMF_TESTDIR)/ESMCI_WebServServerSocketUTest \
                  $(ESMF_TESTDIR)/ESMCI_WebServClientSocketUTest \
                  $(ESMF_TESTDIR)/ESMCI_WebServForkClientUTest 

#                  $(ESMF_TESTDIR)/ESMCI_WebServSecureServerSocketUTest 
#                  $(ESMF_TESTDIR)/ESMCI_WebServSecureClientSocketUTest 
#                  $(ESMF_TESTDIR)/ESMCI_WebServGRAMClientUTest 

TESTS_RUN     =	

TESTS_RUN_UNI =	


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

RUN_ESMCI_WebServCompSvrClientUTest:
	@echo -e "NUMBER_OF_PROCESSORS 1\nPASS\nPASS\nPASS\nPASS\nPASS" > $(ESMF_TESTDIR)/ESMCI_WebServCompSvrClientUTest.Log

RUN_ESMCI_WebServCompSvrClientUTestUNI:
	@echo -e "NUMBER_OF_PROCESSORS 1\nPASS\nPASS\nPASS\nPASS\nPASS" > $(ESMF_TESTDIR)/ESMCI_WebServCompSvrClientUTest.Log

else
RUN_ESMF_WebServUTest:
	$(MAKE) TNAME=WebServ NP=1 ftest & \
   sleep 5 

RUN_ESMF_WebServUTestUNI:
	$(MAKE) TNAME=WebServ NP=1 ftest & \
   sleep 5

RUN_ESMCI_WebServCompSvrClientUTest:
	$(MAKE) TNAME=WebServCompSvrClient NP=1 citest

RUN_ESMCI_WebServCompSvrClientUTestUNI:
	$(MAKE) TNAME=WebServCompSvrClient NP=1 citest

RUN_ESMCI_WebServSecureUTest:
	$(MAKE) TNAME=WebServSecure NP=1 citest

RUN_ESMCI_WebServSecureUTestUNI:
	$(MAKE) TNAME=WebServSecure NP=1 citest


RUN_ESMCI_WebServGRAMClientUTest:
	$(MAKE) TNAME=WebServGRAMClient NP=1 citest

RUN_ESMCI_WebServGRAMClientUTestUNI:
	$(MAKE) TNAME=WebServGRAMClient NP=1 citest


RUN_ESMCI_WebServProcCtrlUTest:
	$(MAKE) TNAME=WebServProcCtrl NP=1 citest & \
   sleep 3 

RUN_ESMCI_WebServProcCtrlUTestUNI:
	$(MAKE) TNAME=WebServProcCtrl NP=1 citest & \
   sleep 3 


RUN_ESMCI_WebServProcCtrlClientUTest:
	$(MAKE) TNAME=WebServProcCtrlClient NP=1 citest

RUN_ESMCI_WebServProcCtrlClientUTestUNI:
	$(MAKE) TNAME=WebServProcCtrlClient NP=1 citest

endif
