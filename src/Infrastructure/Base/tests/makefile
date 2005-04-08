# $Id: makefile,v 1.15 2005/04/08 17:49:24 svasquez Exp $ 

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
LOCDIR	  = src/Infrastructure/Base/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_BaseUTest \
		$(ESMC_TESTDIR)/ESMF_StringUTest

TESTS_RUN     = RUN_ESMF_BaseUTest \
		RUN_ESMF_StringUTest

TESTS_RUN_UNI = RUN_ESMF_BaseUTestUNI \
		RUN_ESMF_StringUTestUNI

TESTS_RM      = ESMF_BaseUTest.rm \
		ESMF_StringUTest.rm

include ${ESMF_DIR}/makefile

DIRS        =

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
#  Base class
#

RUN_ESMF_BaseUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_BaseUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_BaseUTest > $(ESMC_TESTDIR)/ESMF_BaseUTest.stdout
	-cat *BaseUTest.Log > $(ESMC_TESTDIR)/ESMF_BaseUTest.Log
	-rm *BaseUTest.Log

RUN_ESMF_BaseUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_BaseUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_BaseUTest > $(ESMC_TESTDIR)/ESMF_BaseUTest.stdout
	-cat *BaseUTest.Log > $(ESMC_TESTDIR)/ESMF_BaseUTest.Log
	-rm *BaseUTest.Log

ESMF_BaseUTest.rm:
	echo "ESMF_BaseUTest.rm target called here."

#
# Special for strings passed between C++ and F90
#  this is NOT an MPI program and can just be run from the command line
#
UTEST_String_OBJS = ESMC_StringSubr.o
ESMF_StringUTest.o : ESMC_StringSubr.o

RUN_ESMF_StringUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_StringUTest.tmps
	-$(ESMC_TESTDIR)/ESMF_StringUTest > $(ESMC_TESTDIR)/ESMF_StringUTest.stdout

RUN_ESMF_StringUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_StringUTest.tmps
	-$(ESMC_TESTDIR)/ESMF_StringUTest > $(ESMC_TESTDIR)/ESMF_StringUTest.stdout



