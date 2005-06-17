# $Id: makefile,v 1.25 2005/06/17 21:57:09 rfaincht Exp $

ALL:  build_unit_tests
 
run:  tree_run_unit_tests

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Infrastructure/Regrid/tests


TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_RegridUTest \
		$(ESMF_TESTDIR)/ESMF_RegridOptionsUTest \
		$(ESMF_TESTDIR)/ESMF_Regrid1UTest \
		$(ESMF_TESTDIR)/ESMF_Regrid2UTest \
		$(ESMF_TESTDIR)/ESMF_Regrid_bUTest \
		$(ESMF_TESTDIR)/ESMF_RegridSampleUTest

TESTS_RUN     = RUN_ESMF_RegridUTest \
		RUN_ESMF_RegridOptionsUTest \
		RUN_ESMF_Regrid1UTest \
		RUN_ESMF_Regrid2UTest \
		RUN_ESMF_Regrid_bUTest \
		RUN_ESMF_RegridSampleUTest

TESTS_RUN_UNI = RUN_ESMF_RegridUTestUNI \
	        RUN_ESMF_RegridOptionsUTestUNI \
	        RUN_ESMF_Regrid1UTestUNI \
	        RUN_ESMF_Regrid2UTestUNI \
		RUN_ESMF_Regrid_bUTestUNI \
		RUN_ESMF_RegridSampleUTestUNI


include $(ESMF_DIR)/makefile


DIRS =

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

#
#
#
RUN_ESMF_RegridUTest:
	-${MPIRUN} -np 8 $(ESMF_TESTDIR)/ESMF_RegridUTest > $(ESMF_TESTDIR)/ESMF_RegridUTest.stdout
	-cat *RegridUTest.Log > $(ESMC_TESTDIR)/ESMF_RegridUTest.Log
	-rm *RegridUTest.Log

RUN_ESMF_RegridUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_RegridUTest > $(ESMF_TESTDIR)/ESMF_RegridUTest.stdout
	-cat *RegridUTest.Log > $(ESMC_TESTDIR)/ESMF_RegridUTest.Log
	-rm *RegridUTest.Log

#
#
#
RUN_ESMF_RegridOptionsUTest:
	-${MPIRUN} -np 8 $(ESMF_TESTDIR)/ESMF_RegridOptionsUTest > $(ESMF_TESTDIR)/ESMF_RegridOptionsUTest.stdout
	-cat *RegridOptionsUTest.Log > $(ESMC_TESTDIR)/ESMF_RegridOptionsUTest.Log
	-rm *RegridOptionsUTest.Log

RUN_ESMF_RegridOptionsUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_RegridOptionsUTest > $(ESMF_TESTDIR)/ESMF_RegridOptionsUTest.stdout
	-cat *RegridOptionsUTest.Log > $(ESMC_TESTDIR)/ESMF_RegridOptionsUTest.Log
	-rm *RegridOptionsUTest.Log


#
#
#
RUN_ESMF_Regrid1UTest:
	-${MPIRUN} -np 8 $(ESMF_TESTDIR)/ESMF_Regrid1UTest > $(ESMF_TESTDIR)/ESMF_Regrid1UTest.stdout
	-cat *Regrid1UTest.Log > $(ESMC_TESTDIR)/ESMF_Regrid1UTest.Log
	-rm *Regrid1UTest.Log

RUN_ESMF_Regrid1UTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_Regrid1UTest > $(ESMF_TESTDIR)/ESMF_Regrid1UTest.stdout
	-cat *Regrid1UTest.Log > $(ESMC_TESTDIR)/ESMF_Regrid1UTest.Log
	-rm *Regrid1UTest.Log


#
#
#
RUN_ESMF_Regrid2UTest:
	-${MPIRUN} -np 8 $(ESMF_TESTDIR)/ESMF_Regrid2UTest > $(ESMF_TESTDIR)/ESMF_Regrid2UTest.stdout
	-cat *Regrid2UTest.Log > $(ESMC_TESTDIR)/ESMF_Regrid2UTest.Log
	-rm *Regrid2UTest.Log

RUN_ESMF_Regrid2UTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_Regrid2UTest > $(ESMF_TESTDIR)/ESMF_Regrid2UTest.stdout
	-cat *Regrid2UTest.Log > $(ESMC_TESTDIR)/ESMF_Regrid2UTest.Log
	-rm *Regrid2UTest.Log
#
#
#
RUN_ESMF_Regrid_bUTest:
	-${MPIRUN} -np 8 $(ESMF_TESTDIR)/ESMF_Regrid_bUTest > $(ESMF_TESTDIR)/ESMF_Regrid_bUTest.stdout
	-cat *Regrid_bUTest.Log > $(ESMC_TESTDIR)/ESMF_Regrid_bUTest.Log
	-rm *Regrid_bUTest.Log

RUN_ESMF_Regrid_bUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_Regrid_bUTest > $(ESMF_TESTDIR)/ESMF_Regrid_bUTest.stdout
	-cat *Regrid_bUTest.Log > $(ESMC_TESTDIR)/ESMF_Regrid_bUTest.Log
	-rm *Regrid_bUTest.Log
#
#
#
RUN_ESMF_RegridSampleUTest:
	-${MPIRUN} -np 8 $(ESMF_TESTDIR)/ESMF_RegridSampleUTest > $(ESMF_TESTDIR)/ESMF_RegridSampleUTest.stdout
	-cat *RegridSampleUTest.Log > $(ESMC_TESTDIR)/ESMF_RegridSampleUTest.Log
	-rm *RegridSampleUTest.Log

RUN_ESMF_RegridSampleUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_RegridSampleUTest > $(ESMF_TESTDIR)/ESMF_RegridSampleUTest.stdout
	-cat *RegridSampleUTest.Log > $(ESMC_TESTDIR)/ESMF_RegridSampleUTest.Log
	-rm *RegridSampleUTest.Log

