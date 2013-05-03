# $Id$

ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_WebServices


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_WebServicesSTest \
                        $(ESMC_TESTDIR)/ESMF_WebServicesClientSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = 

SYSTEM_TESTS_RUN      = RUN_WebServices \
                        RUN_WebServicesClient

SYSTEM_TESTS_RUN_UNI  =

ESMF_WebServicesSTest.o : $(SYSTEM_TESTS_OBJ)

ESMF_WebServicesClientSTest.o: 

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_WebServices:
   $(MAKE) TNAME=WebServices NP=1 stest & \
   sleep 3

RUN_WebServicesClient:
   $(MAKE) TNAME=WebServicesClient NP=1 stest

