# $Id: makefile,v 1.4 2005/04/05 16:49:22 svasquez Exp $ 

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

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_LogErrUTest \
		$(ESMC_TESTDIR)/ESMF_LogErrHaltUTest

TESTS_RUN     = RUN_ESMF_LogErrUTest \
                RUN_ESMF_LogErrHaltUTest

TESTS_RUN_UNI = RUN_ESMF_LogErrUTestUNI \
                RUN_ESMF_LogErrHaltUTestUNI


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

#
# LogErrHalt
#

RUN_ESMF_LogErrHaltUTest:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_LogErrHaltUTest > $(ESMC_TESTDIR)/ESMF_LogErrHaltUTest.stdout

RUN_ESMF_LogErrHaltUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_LogErrHaltUTest > $(ESMC_TESTDIR)/ESMF_LogErrHaltUTest.stdout



