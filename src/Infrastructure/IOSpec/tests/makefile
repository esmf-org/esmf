# $Id: makefile,v 1.3 2005/04/08 16:17:06 svasquez Exp $ 

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
LOCDIR	  = src/Infrastructure/IOSpec/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_IOSpecUTest

TESTS_RUN     = RUN_ESMF_IOSpecUTest

TESTS_RUN_UNI = RUN_ESMF_IOSpecUTestUNI 


include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
# IOSpec
#
RUN_ESMF_IOSpecUTest:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_IOSpecUTest > $(ESMC_TESTDIR)/ESMF_IOSpecUTest.stdout
	-cat *IOSpecUTest.Log > $(ESMC_TESTDIR)/ESMF_IOSpecUTest.Log
	-rm *IOSpecUTest.Log

RUN_ESMF_IOSpecUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_IOSpecUTest > $(ESMC_TESTDIR)/ESMF_IOSpecUTest.stdout
	-cat *IOSpecUTest.Log > $(ESMC_TESTDIR)/ESMF_IOSpecUTest.Log
	-rm *IOSpecUTest.Log

