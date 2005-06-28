# $Id: makefile,v 1.8 2005/06/28 20:20:44 svasquez Exp $

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
	-$(RM) *ConfigUTest.Log
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_ConfigUTest > $(ESMC_TESTDIR)/ESMF_ConfigUTest.stdout
	-cat *ConfigUTest.Log > $(ESMC_TESTDIR)/ESMF_ConfigUTest.Log
	-$(RM) *ConfigUTest.Log


RUN_ESMF_ConfigUTestUNI:
	-$(RM) *ConfigUTest.Log
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_ConfigUTest > $(ESMC_TESTDIR)/ESMF_ConfigUTest.stdout
	-cat *ConfigUTest.Log > $(ESMC_TESTDIR)/ESMF_ConfigUTest.Log
	-$(RM) *ConfigUTest.Log


#
# ESMC_ConfigUTest
#
RUN_ESMC_ConfigUTest:
	-$(RM) *ConfigUTest.Log
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMC_ConfigUTest > $(ESMC_TESTDIR)/ESMC_ConfigUTest.stdout
	-cat *ConfigUTest.Log > $(ESMC_TESTDIR)/ESMC_ConfigUTest.Log
	-$(RM) *ConfigUTest.Log

RUN_ESMC_ConfigUTestUNI:
	-$(RM) *ConfigUTest.Log
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMC_ConfigUTest > $(ESMC_TESTDIR)/ESMC_ConfigUTest.stdout
	-cat *ConfigUTest.Log > $(ESMC_TESTDIR)/ESMC_ConfigUTest.Log
	-$(RM) *ConfigUTest.Log



