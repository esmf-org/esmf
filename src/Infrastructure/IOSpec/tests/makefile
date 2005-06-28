# $Id: makefile,v 1.5 2005/06/28 20:34:59 svasquez Exp $ 

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
	-$(RM) *IOSpecUTest.Log
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_IOSpecUTest > $(ESMC_TESTDIR)/ESMF_IOSpecUTest.stdout
	-cat *IOSpecUTest.Log > $(ESMC_TESTDIR)/ESMF_IOSpecUTest.Log
	-$(RM) *IOSpecUTest.Log

RUN_ESMF_IOSpecUTestUNI:
	-$(RM) *IOSpecUTest.Log
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_IOSpecUTest > $(ESMC_TESTDIR)/ESMF_IOSpecUTest.stdout
	-cat *IOSpecUTest.Log > $(ESMC_TESTDIR)/ESMF_IOSpecUTest.Log
	-$(RM) *IOSpecUTest.Log

