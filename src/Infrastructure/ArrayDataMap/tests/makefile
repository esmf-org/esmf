# $Id: makefile,v 1.6 2004/12/15 22:42:36 nscollins Exp $

ALL:  build_tests

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

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt NULL core ESMF_LogFile $(TESTS_BUILD)
CLOBBERDIRS =

#
# Fortran unit test
#
RUN_ESMF_ArrayDataMapUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_ArrayDataMapUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_ArrayDataMapUTest > $(ESMC_TESTDIR)/ESMF_ArrayDataMapUTest.stdout

RUN_ESMF_ArrayDataMapUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_ArrayDataMapUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_ArrayDataMapUTest > $(ESMC_TESTDIR)/ESMF_ArrayDataMapUTest.stdout

ESMF_ArrayDataMapUTest.rm:
	echo "ESMF_ArrayDataMapTest.rm target called here."


#
# C++ Unit tests
#

RUN_ESMC_ArrayDataMapUTest:
	-@rm -f testlog stdlog.* logfile.* ESMC_ArrayDataMapUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMC_ArrayDataMapUTest > $(ESMC_TESTDIR)/ESMC_ArrayDataMapUTest.stdout

RUN_ESMC_ArrayDataMapUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMC_ArrayDataMapUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMC_ArrayDataMapUTest > $(ESMC_TESTDIR)/ESMC_ArrayDataMapUTest.stdout

ESMC_ArrayDataMapUTest.rm:
	echo "ESMC_ArrayDataMapTest.rm target called here."



