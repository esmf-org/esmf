# $Id: makefile,v 1.9 2003/06/17 19:51:08 jwolfe Exp $

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
LOCDIR	  = src/Infrastructure/CommMem/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_DELayoutUTest

TESTS_RUN     = RUN_ESMF_DELayoutUTest

TESTS_RUN_UNI = RUN_ESMF_DELayoutUTestUNI

TESTS_RM      = ESMF_DELayoutUTest.rm


include ${ESMF_DIR}/makefile

DIRS	  = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt NULL core $(TESTS_BUILD)
CLOBBERDIRS =


RUN_ESMF_DELayoutUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_DELayoutUTest.tmps
	-${MPIRUN} -np 8 $(ESMC_TESTDIR)/ESMF_DELayoutUTest > $(ESMC_TESTDIR)/ESMF_DELayoutUTest.stdout

RUN_ESMF_DELayoutUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_DELayoutUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_DELayoutUTest > $(ESMC_TESTDIR)/ESMF_DELayoutUTest.stdout

ESMF_DELayoutUTest.rm:
	echo "ESMF_DELayoutTest.rm target called here."



