# $Id: makefile,v 1.7 2005/03/01 21:02:24 nscollins Exp $

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
LOCDIR	  = src/Infrastructure/FieldDataMap/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_FieldDataMapUTest \
#	        $(ESMC_TESTDIR)/ESMC_FieldDataMapUTest 

TESTS_RUN     = RUN_ESMF_FieldDataMapUTest \
#		RUN_ESMC_FieldDataMapUTest 

TESTS_RUN_UNI = RUN_ESMF_FieldDataMapUTestUNI \
#		RUN_ESMC_FieldDataMapUTestUNI 

TESTS_RM      = ESMF_FieldDataMapUTest.rm \
#                ESMC_FieldDataMapUTest.rm


include ${ESMF_DIR}/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

#
# Fortran unit test
#
RUN_ESMF_FieldDataMapUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldDataMapUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_FieldDataMapUTest > $(ESMC_TESTDIR)/ESMF_FieldDataMapUTest.stdout

RUN_ESMF_FieldDataMapUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldDataMapUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_FieldDataMapUTest > $(ESMC_TESTDIR)/ESMF_FieldDataMapUTest.stdout

ESMF_FieldDataMapUTest.rm:
	echo "ESMF_FieldDataMapTest.rm target called here."


#
# C++ Unit tests
#
RUN_ESMC_FieldDataMapUTest:
	-@rm -f testlog stdlog.* logfile.* ESMC_FieldDataMapUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMC_FieldDataMapUTest > $(ESMC_TESTDIR)/ESMC_FieldDataMapUTest.stdout

RUN_ESMC_FieldDataMapUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMC_FieldDataMapUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMC_FieldDataMapUTest > $(ESMC_TESTDIR)/ESMC_FieldDataMapUTest.stdout

ESMC_FieldDataMapUTest.rm:
	echo "ESMC_FieldDataMapTest.rm target called here."



