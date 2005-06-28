# $Id: makefile,v 1.26 2005/06/28 20:47:20 svasquez Exp $

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
	-$(RM) *RegridUTest.Log
	-${MPIRUN} -np 8 $(ESMF_TESTDIR)/ESMF_RegridUTest > $(ESMF_TESTDIR)/ESMF_RegridUTest.stdout
	-cat *RegridUTest.Log > $(ESMC_TESTDIR)/ESMF_RegridUTest.Log
	-$(RM) *RegridUTest.Log

RUN_ESMF_RegridUTestUNI:
	-$(RM) *RegridUTest.Log
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_RegridUTest > $(ESMF_TESTDIR)/ESMF_RegridUTest.stdout
	-cat *RegridUTest.Log > $(ESMC_TESTDIR)/ESMF_RegridUTest.Log
	-$(RM) *RegridUTest.Log

#
#
#
RUN_ESMF_RegridOptionsUTest:
	-$(RM) *RegridOptionsUTest.Log
	-${MPIRUN} -np 8 $(ESMF_TESTDIR)/ESMF_RegridOptionsUTest > $(ESMF_TESTDIR)/ESMF_RegridOptionsUTest.stdout
	-cat *RegridOptionsUTest.Log > $(ESMC_TESTDIR)/ESMF_RegridOptionsUTest.Log
	-$(RM) *RegridOptionsUTest.Log

RUN_ESMF_RegridOptionsUTestUNI:
	-$(RM) *RegridOptionsUTest.Log
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_RegridOptionsUTest > $(ESMF_TESTDIR)/ESMF_RegridOptionsUTest.stdout
	-cat *RegridOptionsUTest.Log > $(ESMC_TESTDIR)/ESMF_RegridOptionsUTest.Log
	-$(RM) *RegridOptionsUTest.Log


#
#
#
RUN_ESMF_Regrid1UTest:
	-$(RM) *Regrid1UTest.Log
	-${MPIRUN} -np 8 $(ESMF_TESTDIR)/ESMF_Regrid1UTest > $(ESMF_TESTDIR)/ESMF_Regrid1UTest.stdout
	-cat *Regrid1UTest.Log > $(ESMC_TESTDIR)/ESMF_Regrid1UTest.Log
	-$(RM) *Regrid1UTest.Log

RUN_ESMF_Regrid1UTestUNI:
	-$(RM) *Regrid1UTest.Log
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_Regrid1UTest > $(ESMF_TESTDIR)/ESMF_Regrid1UTest.stdout
	-cat *Regrid1UTest.Log > $(ESMC_TESTDIR)/ESMF_Regrid1UTest.Log
	-$(RM) *Regrid1UTest.Log


#
#
#
RUN_ESMF_Regrid2UTest:
	-$(RM) *Regrid2UTest.Log
	-${MPIRUN} -np 8 $(ESMF_TESTDIR)/ESMF_Regrid2UTest > $(ESMF_TESTDIR)/ESMF_Regrid2UTest.stdout
	-cat *Regrid2UTest.Log > $(ESMC_TESTDIR)/ESMF_Regrid2UTest.Log
	-$(RM) *Regrid2UTest.Log

RUN_ESMF_Regrid2UTestUNI:
	-$(RM) *Regrid2UTest.Log
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_Regrid2UTest > $(ESMF_TESTDIR)/ESMF_Regrid2UTest.stdout
	-cat *Regrid2UTest.Log > $(ESMC_TESTDIR)/ESMF_Regrid2UTest.Log
	-$(RM) *Regrid2UTest.Log
#
#
#
RUN_ESMF_Regrid_bUTest:
	-$(RM) *Regrid_bUTest.Log
	-${MPIRUN} -np 8 $(ESMF_TESTDIR)/ESMF_Regrid_bUTest > $(ESMF_TESTDIR)/ESMF_Regrid_bUTest.stdout
	-cat *Regrid_bUTest.Log > $(ESMC_TESTDIR)/ESMF_Regrid_bUTest.Log
	-$(RM) *Regrid_bUTest.Log

RUN_ESMF_Regrid_bUTestUNI:
	-$(RM) *Regrid_bUTest.Log
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_Regrid_bUTest > $(ESMF_TESTDIR)/ESMF_Regrid_bUTest.stdout
	-cat *Regrid_bUTest.Log > $(ESMC_TESTDIR)/ESMF_Regrid_bUTest.Log
	-$(RM) *Regrid_bUTest.Log
#
#
#
RUN_ESMF_RegridSampleUTest:
	-$(RM) *RegridSampleUTest.Log
	-${MPIRUN} -np 8 $(ESMF_TESTDIR)/ESMF_RegridSampleUTest > $(ESMF_TESTDIR)/ESMF_RegridSampleUTest.stdout
	-cat *RegridSampleUTest.Log > $(ESMC_TESTDIR)/ESMF_RegridSampleUTest.Log
	-$(RM) *RegridSampleUTest.Log

RUN_ESMF_RegridSampleUTestUNI:
	-$(RM) *RegridSampleUTest.Log
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_RegridSampleUTest > $(ESMF_TESTDIR)/ESMF_RegridSampleUTest.stdout
	-cat *RegridSampleUTest.Log > $(ESMC_TESTDIR)/ESMF_RegridSampleUTest.Log
	-$(RM) *RegridSampleUTest.Log

