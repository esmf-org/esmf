# $Id: makefile,v 1.2 2005/03/01 21:03:09 nscollins Exp $ 

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

RUN_ESMF_IOSpecUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_IOSpecUTest > $(ESMC_TESTDIR)/ESMF_IOSpecUTest.stdout

