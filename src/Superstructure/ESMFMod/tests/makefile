# $Id: makefile,v 1.9 2005/06/28 19:56:50 svasquez Exp $

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
LOCDIR	  = src/Superstructure/ESMFMod/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_ESMFModUTest \
	        $(ESMC_TESTDIR)/ESMC_ESMFModUTest 

TESTS_RUN     = RUN_ESMF_ESMFModUTest \
		RUN_ESMC_ESMFModUTest 

TESTS_RUN_UNI = RUN_ESMF_ESMFModUTestUNI \
		RUN_ESMC_ESMFModUTestUNI 


include ${ESMF_DIR}/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

#
# Fortran unit test
#
RUN_ESMF_ESMFModUTest:
	-$(RM) *ESMFModUTest.Log
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_ESMFModUTest > $(ESMC_TESTDIR)/ESMF_ESMFModUTest.stdout
	-cat *ESMFModUTest.Log > $(ESMC_TESTDIR)/ESMF_ESMFModUTest.Log
	-$(RM) *ESMFModUTest.Log


RUN_ESMF_ESMFModUTestUNI:
	-$(RM) *ESMFModUTest.Log
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_ESMFModUTest > $(ESMC_TESTDIR)/ESMF_ESMFModUTest.stdout
	-cat *ESMFModUTest.Log > $(ESMC_TESTDIR)/ESMF_ESMFModUTest.Log
	-$(RM) *ESMFModUTest.Log


#
# C++ Unit tests
#

RUN_ESMC_ESMFModUTest:
	-$(RM) *ESMFModUTest.Log
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMC_ESMFModUTest > $(ESMC_TESTDIR)/ESMC_ESMFModUTest.stdout
	-cat *ESMFModUTest.Log > $(ESMC_TESTDIR)/ESMF_ESMCModUTest.Log
	-$(RM) *ESMFModUTest.Log

RUN_ESMC_ESMFModUTestUNI:
	-$(RM) *ESMFModUTest.Log
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMC_ESMFModUTest > $(ESMC_TESTDIR)/ESMC_ESMFModUTest.stdout
	-cat *ESMFModUTest.Log > $(ESMC_TESTDIR)/ESMF_ESMCModUTest.Log
	-$(RM) *ESMFModUTest.Log




