# $Id: makefile,v 1.3 2005/03/01 21:03:35 nscollins Exp $ 

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
DIRS	  = 
LOCDIR	  = src/Infrastructure/LogErr/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_LogErrUTest

TESTS_RUN     = RUN_ESMF_LogErrUTest

TESTS_RUN_UNI = RUN_ESMF_LogErrUTestUNI


include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
# LogErr
#
RUN_ESMF_LogErrUTest:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_LogErrUTest > $(ESMC_TESTDIR)/ESMF_LogErrUTest.stdout

RUN_ESMF_LogErrUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_LogErrUTest > $(ESMC_TESTDIR)/ESMF_LogErrUTest.stdout


