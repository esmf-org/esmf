# $Id: makefile,v 1.5 2005/02/24 22:21:46 nscollins Exp $ 

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
LOCDIR	  = src/Superstructure/State/tests

TESTS_BUILD   =	$(ESMC_TESTDIR)/ESMF_StateCreateUTest \
		$(ESMC_TESTDIR)/ESMF_StateUTest

TESTS_RUN     =	RUN_ESMF_StateCreateUTest \
		RUN_ESMF_StateUTest

TESTS_RUN_UNI =	RUN_ESMF_StateCreateUTestUNI \
		RUN_ESMF_StateUTestUNI


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt *.stdout NULL core ESMF_LogFile $(TESTS_BUILD)
CLOBBERDIRS =

#       
# State Create
#

RUN_ESMF_StateCreateUTest:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_StateCreateUTest > $(ESMC_TESTDIR)/ESMF_StateCreateUTest.stdout

RUN_ESMF_StateCreateUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_StateCreateUTest > $(ESMC_TESTDIR)/ESMF_StateCreateUTest.stdout



#       
# State Unit Test
#

RUN_ESMF_StateUTest:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_StateUTest > $(ESMC_TESTDIR)/ESMF_StateUTest.stdout

RUN_ESMF_StateUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_StateUTest > $(ESMC_TESTDIR)/ESMF_StateUTest.stdout



