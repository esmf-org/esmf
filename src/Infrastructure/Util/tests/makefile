# $Id: makefile,v 1.2 2005/06/28 20:57:43 svasquez Exp $ 

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
LOCDIR	  = src/Infrastructure/Util/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_UtilUTest

TESTS_RUN     = RUN_ESMF_UtilUTest

TESTS_RUN_UNI = RUN_ESMF_UtilUTestUNI


include ${ESMF_DIR}/makefile

DIRS        =

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
#  Util class
#

RUN_ESMF_UtilUTest:
	-$(RM) *UtilUTest.Log
	-@rm -f testlog stdlog.* logfile.* ESMF_UtilUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_UtilUTest > $(ESMC_TESTDIR)/ESMF_UtilUTest.stdout
	-cat *UtilUTest.Log > $(ESMC_TESTDIR)/ESMF_UtilUTest.Log
	-$(RM) *UtilUTest.Log

RUN_ESMF_UtilUTestUNI:
	-$(RM) *UtilUTest.Log
	-@rm -f testlog stdlog.* logfile.* ESMF_UtilUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_UtilUTest > $(ESMC_TESTDIR)/ESMF_UtilUTest.stdout
	-cat *UtilUTest.Log > $(ESMC_TESTDIR)/ESMF_UtilUTest.Log
	-$(RM) *UtilUTest.Log


