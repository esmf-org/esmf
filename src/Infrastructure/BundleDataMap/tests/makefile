# $Id: makefile,v 1.7 2005/03/01 20:59:33 nscollins Exp $

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
LOCDIR	  = src/Infrastructure/BundleDataMap/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_BundleDataMapUTest 
#	        $(ESMC_TESTDIR)/ESMC_BundleDataMapUTest 
# Comment out temporarily

TESTS_RUN     = RUN_ESMF_BundleDataMapUTest \
#		RUN_ESMC_BundleDataMapUTest 
# Comment out temporarily

TESTS_RUN_UNI = RUN_ESMF_BundleDataMapUTestUNI \
#		RUN_ESMC_BundleDataMapUTestUNI 
# Comment out temporarily

TESTS_RM      = ESMF_BundleDataMapUTest.rm \
#                ESMC_BundleDataMapUTest.rm
# Comment out temporarily


include ${ESMF_DIR}/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

#
# Fortran unit test
#
RUN_ESMF_BundleDataMapUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_BundleDataMapUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_BundleDataMapUTest > $(ESMC_TESTDIR)/ESMF_BundleDataMapUTest.stdout

RUN_ESMF_BundleDataMapUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_BundleDataMapUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_BundleDataMapUTest > $(ESMC_TESTDIR)/ESMF_BundleDataMapUTest.stdout

ESMF_BundleDataMapUTest.rm:
	echo "ESMF_BundleDataMapTest.rm target called here."


#
# C++ Unit tests
#
RUN_ESMC_BundleDataMapUTest:
	-@rm -f testlog stdlog.* logfile.* ESMC_BundleDataMapUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMC_BundleDataMapUTest > $(ESMC_TESTDIR)/ESMC_BundleDataMapUTest.stdout

RUN_ESMC_BundleDataMapUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMC_BundleDataMapUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMC_BundleDataMapUTest > $(ESMC_TESTDIR)/ESMC_BundleDataMapUTest.stdout

ESMC_BundleDataMapUTest.rm:
	echo "ESMC_BundleDataMapTest.rm target called here."



