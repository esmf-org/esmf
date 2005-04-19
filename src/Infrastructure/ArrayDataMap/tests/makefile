# $Id: makefile,v 1.9 2005/04/19 16:06:38 svasquez Exp $

ALL:  build_unit_tests

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Infrastructure/ArrayDataMap/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_ArrayDataMapUTest \
#	        $(ESMC_TESTDIR)/ESMC_ArrayDataMapUTest 

TESTS_RUN     = RUN_ESMF_ArrayDataMapUTest \
#		RUN_ESMC_ArrayDataMapUTest 

TESTS_RUN_UNI = RUN_ESMF_ArrayDataMapUTestUNI \
#		RUN_ESMC_ArrayDataMapUTestUNI 

TESTS_RM      = ESMF_ArrayDataMapUTest.rm \
#                ESMC_ArrayDataMapUTest.rm


include ${ESMF_DIR}/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

#
# Fortran unit test
#
RUN_ESMF_ArrayDataMapUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_ArrayDataMapUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_ArrayDataMapUTest > $(ESMC_TESTDIR)/ESMF_ArrayDataMapUTest.stdout
        -cat *ArrayDataMapUTest.Log > $(ESMC_TESTDIR)/ESMF_ArrayDataMapUTest.Log
        -rm *ArrayDataMapUTest.Log


RUN_ESMF_ArrayDataMapUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_ArrayDataMapUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_ArrayDataMapUTest > $(ESMC_TESTDIR)/ESMF_ArrayDataMapUTest.stdout
        -cat *ArrayDataMapUTest.Log > $(ESMC_TESTDIR)/ESMF_ArrayDataMapUTest.Log
        -rm *ArrayDataMapUTest.Log

ESMF_ArrayDataMapUTest.rm:
	echo "ESMF_ArrayDataMapTest.rm target called here."


#
# C++ Unit tests
#

RUN_ESMC_ArrayDataMapUTest:
	-@rm -f testlog stdlog.* logfile.* ESMC_ArrayDataMapUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMC_ArrayDataMapUTest > $(ESMC_TESTDIR)/ESMC_ArrayDataMapUTest.stdout
        -cat *ArrayDataMapUTest.Log > $(ESMC_TESTDIR)/ESMC_ArrayDataMapUTest.Log
        -rm *ArrayDataMapUTest.Log

RUN_ESMC_ArrayDataMapUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMC_ArrayDataMapUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMC_ArrayDataMapUTest > $(ESMC_TESTDIR)/ESMC_ArrayDataMapUTest.stdout
        -cat *ArrayDataMapUTest.Log > $(ESMC_TESTDIR)/ESMC_ArrayDataMapUTest.Log
        -rm *ArrayDataMapUTest.Log

ESMC_ArrayDataMapUTest.rm:
	echo "ESMC_ArrayDataMapTest.rm target called here."



