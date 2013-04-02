# $Id$ 

ALL: tree_build_system_tests

run: tree_run_system_tests

LOCDIR	  = src/system_tests/ESMF_FaultTolerance


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_FaultToleranceSTest $(ESMC_TESTDIR)/ESMF_FaultToleranceCompASTest

# Object files other than STest%.o that the system tests executable will depend
# on. List objects files in the order that they are to be compiled/created.
#SYSTEM_TESTS_OBJ_A    = user_model1.o
#SYSTEM_TESTS_OBJ_B    = user_model2.o
SYSTEM_TESTS_OBJ      =
SYSTEM_TESTS_OBJ_A    =
SYSTEM_TESTS_OBJ_B    =

SYSTEM_TESTS_RUN      = RUN_FaultTolerance
SYSTEM_TESTS_RUN_UNI  =

ESMF_FaultToleranceSTest.o : $(SYSTEM_TESTS_OBJ)
ESMF_FaultToleranceCompASTest.o : $(SYSTEM_TESTS_OBJ_A)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_FaultToleranceMain:
	cp -f server.txt $(ESMF_TESTDIR)
	$(MAKE) TNAME=FaultTolerance NP=4 stest

RUN_FaultToleranceCompA:
	$(MAKE) TNAME=FaultToleranceCompA NP=2 stest

RUN_FaultTolerance:
	cp -f server.txt $(ESMF_TESTDIR)
	$(MAKE) TNAME=FaultTolerance NP=4 stest &
	sleep 2
	$(MAKE) TNAME=FaultToleranceCompA NP=2 stest

RUN_FaultToleranceTV:
	cp -f server.txt $(ESMF_TESTDIR)
	mpirun -tv -np 4 $(ESMF_TESTDIR)/ESMF_FaultToleranceSTest &
	sleep 25
	$(MAKE) TNAME=FaultToleranceCompA NP=2 stest 


RUN_FaultToleranceSingle:
	cp -f server.txt $(ESMF_TESTDIR)
	$(MAKE) TNAME=FaultTolerance NP=4 stest &


RUN_FaultToleranceMochaFudge:
	cp -f server.txt $(ESMF_TESTDIR)
	$(MAKE) TNAME=FaultTolerance NP=4 stest &
	sleep 2
	ssh fudge "cd /mochahome/gerhard/WorkNASA/Projects/Support_ESMF/ESMF.devel/src/system_tests/ESMF_FaultTolerance; /opt/GCC/4.5.2/mpich/1.2.7.1gjt/bin/mpirun -np 2 $(ESMF_TESTDIR)/ESMF_FaultToleranceCompASTest >& $(ESMF_TESTDIR)/ESMF_FaultToleranceCompASTest.stdout"
	cat PET*.Log > $(ESMF_TESTDIR)/ESMF_FaultToleranceCompASTest.Log
	rm PET*.Log
