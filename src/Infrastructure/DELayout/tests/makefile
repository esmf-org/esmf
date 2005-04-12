# $Id: makefile,v 1.7 2005/04/12 16:52:44 nscollins Exp $ 

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
	-@rm -f testlog stdlog.* logfile.* ESMF_DELayoutUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_DELayoutUTest > $(ESMC_TESTDIR)/ESMF_DELayoutUTest.stdout
	-cat *DELayoutUTest.Log > $(ESMC_TESTDIR)/ESMF_DELayoutUTest.Log
	-rm *DELayoutUTest.Log


RUN_ESMF_DELayoutUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_DELayoutUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_DELayoutUTest > $(ESMC_TESTDIR)/ESMF_DELayoutUTest.stdout
	-cat *DELayoutUTest.Log > $(ESMC_TESTDIR)/ESMF_DELayoutUTest.Log
	-rm *DELayoutUTest.Log
