# $Id: makefile,v 1.10 2005/06/28 20:18:21 svasquez Exp $

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
	-$(RM) *BundleDataMapUTest.Log
	-@rm -f testlog stdlog.* logfile.* ESMF_BundleDataMapUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_BundleDataMapUTest > $(ESMC_TESTDIR)/ESMF_BundleDataMapUTest.stdout
	-cat *BundleDataMapUTest.Log > $(ESMC_TESTDIR)/ESMF_BundleDataMapUTest.Log
	-$(RM) *BundleDataMapUTest.Log

RUN_ESMF_BundleDataMapUTestUNI:
	-$(RM) *BundleDataMapUTest.Log
	-@rm -f testlog stdlog.* logfile.* ESMF_BundleDataMapUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_BundleDataMapUTest > $(ESMC_TESTDIR)/ESMF_BundleDataMapUTest.stdout
	-cat *BundleDataMapUTest.Log > $(ESMC_TESTDIR)/ESMF_BundleDataMapUTest.Log
	-$(RM) *BundleDataMapUTest.Log

ESMF_BundleDataMapUTest.rm:
	echo "ESMF_BundleDataMapTest.rm target called here."


#
# C++ Unit tests
#
RUN_ESMC_BundleDataMapUTest:
	-$(RM) *BundleDataMapUTest.Log
	-@rm -f testlog stdlog.* logfile.* ESMC_BundleDataMapUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMC_BundleDataMapUTest > $(ESMC_TESTDIR)/ESMC_BundleDataMapUTest.stdout
	-cat *BundleDataMapUTest.Log > $(ESMC_TESTDIR)/ESMC_BundleDataMapUTest.Log
	-$(RM) *BundleDataMapUTest.Log

RUN_ESMC_BundleDataMapUTestUNI:
	-$(RM) *BundleDataMapUTest.Log
	-@rm -f testlog stdlog.* logfile.* ESMC_BundleDataMapUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMC_BundleDataMapUTest > $(ESMC_TESTDIR)/ESMC_BundleDataMapUTest.stdout
	-cat *BundleDataMapUTest.Log > $(ESMC_TESTDIR)/ESMC_BundleDataMapUTest.Log
	-$(RM) *BundleDataMapUTest.Log

ESMC_BundleDataMapUTest.rm:
	echo "ESMC_BundleDataMapTest.rm target called here."



