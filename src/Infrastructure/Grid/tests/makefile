# $Id: makefile,v 1.13 2005/06/28 20:33:37 svasquez Exp $ 

ALL:  tree_build_unit_tests

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
DIRS	  = 
LOCDIR	  = src/Infrastructure/Grid/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_GridUTest \
	        $(ESMC_TESTDIR)/ESMF_GridCreateUTest

TESTS_RUN     = RUN_ESMF_GridUTest \
                RUN_ESMF_GridCreateUTest

TESTS_RUN_UNI = RUN_ESMF_GridUTestUNI \
                RUN_ESMF_GridCreateUTestUNI

TESTS_RM      = ESMF_GridUTest.rm  ESMF_GridCreateUTest.rm

include $(ESMF_DIR)/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
#  Grid
#
RUN_ESMF_GridUTest:
	-$(RM) *GridUTest.Log
	-@rm -f testlog stdlog.* logfile.* ESMF_GridUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_GridUTest > $(ESMC_TESTDIR)/ESMF_GridUTest.stdout
	-cat *GridUTest.Log > $(ESMC_TESTDIR)/ESMF_GridUTest.Log
	-$(RM) *GridUTest.Log

RUN_ESMF_GridUTestUNI:
	-$(RM) *GridUTest.Log
	-@rm -f testlog stdlog.* logfile.* ESMF_GridUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_GridUTest > $(ESMC_TESTDIR)/ESMF_GridUTest.stdout
	-cat *GridUTest.Log > $(ESMC_TESTDIR)/ESMF_GridUTest.Log
	-$(RM) *GridUTest.Log

ESMF_GridUTest.rm:
	echo "ESMF_GridUTest.rm target called here."

##
RUN_ESMF_GridCreateUTest:
	-$(RM) *GridCreateUTest.Log
	-@rm -f testlog stdlog.* logfile.* ESMF_GridCreateUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_GridCreateUTest > $(ESMC_TESTDIR)/ESMF_GridCreateUTest.stdout
	-cat *GridCreateUTest.Log > $(ESMC_TESTDIR)/ESMF_GridCreateUTest.Log
	-$(RM) *GridCreateUTest.Log

RUN_ESMF_GridCreateUTestUNI:
	-$(RM) *GridCreateUTest.Log
	-@rm -f testlog stdlog.* logfile.* ESMF_GridCreateUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_GridCreateUTest > $(ESMC_TESTDIR)/ESMF_GridCreateUTest.stdout
	-cat *GridCreateUTest.Log > $(ESMC_TESTDIR)/ESMF_GridCreateUTest.Log
	-$(RM) *GridCreateUTest.Log

ESMF_GridCreateUTest.rm:
	echo "ESMF_GridCreateUTest.rm target called here."

