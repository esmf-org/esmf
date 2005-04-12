# $Id: makefile,v 1.9 2005/04/12 16:52:57 nscollins Exp $ 

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

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

#       
# State Create
#

RUN_ESMF_StateCreateUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_StateCreateUTest > $(ESMF_TESTDIR)/ESMF_StateCreateUTest.stdout
	-cat *StateCreateUTest.Log > $(ESMC_TESTDIR)/ESMF_StateCreateUTest.Log
	-rm *StateCreateUTest.Log


RUN_ESMF_StateCreateUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_StateCreateUTest > $(ESMF_TESTDIR)/ESMF_StateCreateUTest.stdout
	-cat *StateCreateUTest.Log > $(ESMC_TESTDIR)/ESMF_StateCreateUTest.Log
	-rm *StateCreateUTest.Log



#       
# State Unit Test
#

RUN_ESMF_StateUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_StateUTest > $(ESMF_TESTDIR)/ESMF_StateUTest.stdout
	-cat *StateUTest.Log > $(ESMC_TESTDIR)/ESMF_StateUTest.Log
	-rm *StateUTest.Log

RUN_ESMF_StateUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_StateUTest > $(ESMF_TESTDIR)/ESMF_StateUTest.stdout
	-cat *StateUTest.Log > $(ESMC_TESTDIR)/ESMF_StateUTest.Log
	-rm *StateUTest.Log




#       
# StateReconcile Unit Test
#

RUN_ESMF_StateReconcileUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_StateReconcileUTest > $(ESMF_TESTDIR)/ESMF_StateReconcileUTest.stdout
	-cat *StateReconcileUTest.Log > $(ESMC_TESTDIR)/ESMF_StateReconcileUTest.Log
	-rm *StateReconcileUTest.Log

RUN_ESMF_StateReconcileUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_StateReconcileUTest > $(ESMF_TESTDIR)/ESMF_StateReconcileUTest.stdout
	-cat *StateReconcileUTest.Log > $(ESMC_TESTDIR)/ESMF_StateReconcileUTest.Log
	-rm *StateReconcileUTest.Log



