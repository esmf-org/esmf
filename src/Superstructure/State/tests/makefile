# $Id: makefile,v 1.6 2005/02/28 19:53:15 nscollins Exp $ 

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

TESTS_BUILD   =	$(ESMF_TESTDIR)/ESMF_StateCreateUTest \
		$(ESMF_TESTDIR)/ESMF_StateReconcileUTest \
		$(ESMF_TESTDIR)/ESMF_StateUTest

TESTS_RUN     =	RUN_ESMF_StateCreateUTest \
		RUN_ESMF_StateReconcileUTest \
		RUN_ESMF_StateUTest

TESTS_RUN_UNI =	RUN_ESMF_StateCreateUTestUNI \
		RUN_ESMF_StateReconcileUTestUNI \
		RUN_ESMF_StateUTestUNI


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt *.stdout NULL core UTestLog *ESMF_LogFile \
               $(TESTS_BUILD)
CLOBBERDIRS =

#       
# State Create
#

RUN_ESMF_StateCreateUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_StateCreateUTest > $(ESMF_TESTDIR)/ESMF_StateCreateUTest.stdout

RUN_ESMF_StateCreateUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_StateCreateUTest > $(ESMF_TESTDIR)/ESMF_StateCreateUTest.stdout



#       
# State Unit Test
#

RUN_ESMF_StateUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_StateUTest > $(ESMF_TESTDIR)/ESMF_StateUTest.stdout

RUN_ESMF_StateUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_StateUTest > $(ESMF_TESTDIR)/ESMF_StateUTest.stdout




#       
# StateReconcile Unit Test
#

RUN_ESMF_StateReconcileUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_StateReconcileUTest > $(ESMF_TESTDIR)/ESMF_StateReconcileUTest.stdout

RUN_ESMF_StateReconcileUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_StateReconcileUTest > $(ESMF_TESTDIR)/ESMF_StateReconcileUTest.stdout



