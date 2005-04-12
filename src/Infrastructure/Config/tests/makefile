# $Id: makefile,v 1.7 2005/04/12 16:52:43 nscollins Exp $

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
LOCDIR	  =  src/Infrastructure/Config/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_ConfigUTest
#	        $(ESMC_TESTDIR)/ESMC_ConfigUTest 

TESTS_RUN     = RUN_ESMF_ConfigUTest
#		RUN_ESMC_ConfigUTest 

TESTS_RUN_UNI = RUN_ESMF_ConfigUTestUNI
#		RUN_ESMC_ConfigUTestUNI 




include ${ESMF_DIR}/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

#
# ESMF_ConfigUTest
#
RUN_ESMF_ConfigUTest:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_ConfigUTest > $(ESMC_TESTDIR)/ESMF_ConfigUTest.stdout
	-cat *ConfigUTest.Log > $(ESMC_TESTDIR)/ESMF_ConfigUTest.Log
	-rm *ConfigUTest.Log


RUN_ESMF_ConfigUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_ConfigUTest > $(ESMC_TESTDIR)/ESMF_ConfigUTest.stdout
	-cat *ConfigUTest.Log > $(ESMC_TESTDIR)/ESMF_ConfigUTest.Log
	-rm *ConfigUTest.Log


#
# ESMC_ConfigUTest
#
RUN_ESMC_ConfigUTest:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMC_ConfigUTest > $(ESMC_TESTDIR)/ESMC_ConfigUTest.stdout
	-cat *ConfigUTest.Log > $(ESMC_TESTDIR)/ESMC_ConfigUTest.Log
	-rm *ConfigUTest.Log

RUN_ESMC_ConfigUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMC_ConfigUTest > $(ESMC_TESTDIR)/ESMC_ConfigUTest.stdout
	-cat *ConfigUTest.Log > $(ESMC_TESTDIR)/ESMC_ConfigUTest.Log
	-rm *ConfigUTest.Log



