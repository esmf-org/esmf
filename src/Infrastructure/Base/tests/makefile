# $Id: makefile,v 1.8 2003/05/23 16:12:45 flanigan Exp $ 

ALL:  tests

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

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt NULL core $(TESTS_BUILD)
CLOBBERDIRS =


#
#  Base class
#

RUN_ESMF_BaseUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_BaseUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_BaseUTest > $(ESMC_TESTDIR)/ESMF_BaseUTest.stdout

RUN_ESMF_BaseUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_BaseUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_BaseUTest > $(ESMC_TESTDIR)/ESMF_BaseUTest.stdout

ESMF_BaseUTest.rm:
	echo "ESMF_BaseUTest.rm target called here."

#
# Special for strings passed between C++ and F90
#
UTEST_String_OBJS = ESMC_StringSubr.o
ESMF_StringUTest.o : ESMC_StringSubr.o

RUN_ESMF_StringUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_StringUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_StringUTest > $(ESMC_TESTDIR)/ESMF_StringUTest.stdout

RUN_ESMF_StringUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_StringUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_StringUTest > $(ESMC_TESTDIR)/ESMF_StringUTest.stdout

ESMF_StringUTest.rm:
	echo "ESMF_StringUTest.rm target called here."


