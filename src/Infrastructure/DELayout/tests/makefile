# $Id: makefile,v 1.8 2005/06/28 20:23:30 svasquez Exp $ 

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
DIRS	  = 
LOCDIR	  = src/Infrastructure/DELayout/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_DELayoutUTest

TESTS_RUN     = RUN_ESMF_DELayoutUTest

TESTS_RUN_UNI = RUN_ESMF_DELayoutUTestUNI

TESTS_RM      =

include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
# VM
#
RUN_ESMF_DELayoutUTest:
	-$(RM) *DELayoutUTest.Log
	-@rm -f testlog stdlog.* logfile.* ESMF_DELayoutUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_DELayoutUTest > $(ESMC_TESTDIR)/ESMF_DELayoutUTest.stdout
	-cat *DELayoutUTest.Log > $(ESMC_TESTDIR)/ESMF_DELayoutUTest.Log
	-$(RM) *DELayoutUTest.Log


RUN_ESMF_DELayoutUTestUNI:
	-$(RM) *DELayoutUTest.Log
	-@rm -f testlog stdlog.* logfile.* ESMF_DELayoutUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_DELayoutUTest > $(ESMC_TESTDIR)/ESMF_DELayoutUTest.stdout
	-cat *DELayoutUTest.Log > $(ESMC_TESTDIR)/ESMF_DELayoutUTest.Log
	-$(RM) *DELayoutUTest.Log
