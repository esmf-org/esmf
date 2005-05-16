# $Id: makefile,v 1.17 2005/05/16 23:02:55 nscollins Exp $ 

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
LOCDIR	  = src/Infrastructure/Base/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_BaseUTest

TESTS_RUN     = RUN_ESMF_BaseUTest

TESTS_RUN_UNI = RUN_ESMF_BaseUTestUNI


include ${ESMF_DIR}/makefile

DIRS        =

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
#  Base class
#

RUN_ESMF_BaseUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_BaseUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_BaseUTest > $(ESMC_TESTDIR)/ESMF_BaseUTest.stdout
	-cat *BaseUTest.Log > $(ESMC_TESTDIR)/ESMF_BaseUTest.Log
	-rm *BaseUTest.Log

RUN_ESMF_BaseUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_BaseUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_BaseUTest > $(ESMC_TESTDIR)/ESMF_BaseUTest.stdout
	-cat *BaseUTest.Log > $(ESMC_TESTDIR)/ESMF_BaseUTest.Log
	-rm *BaseUTest.Log


