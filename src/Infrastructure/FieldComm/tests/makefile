# $Id: makefile,v 1.6 2004/09/23 20:44:47 jwolfe Exp $

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
LOCDIR	  = src/Infrastructure/FieldComm/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_FieldGatherUTest

TESTS_RUN     = RUN_ESMF_FieldGatherUTest

TESTS_RM      = ESMF_FieldGatherUTest.rm

include ${ESMF_DIR}/makefile

DIRS	  = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt NULL core ESMF_LogFile $(TESTS_BUILD)
CLOBBERDIRS =

#
# FieldComm unit tests
#
RUN_ESMF_FieldGatherUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldGatherUTest.tmps
	-${MPIRUN} -np 6 $(ESMC_TESTDIR)/ESMF_FieldGatherUTest > $(ESMC_TESTDIR)/ESMF_FieldGatherUTest.stdout

ESMF_FieldGatherUTest.rm:
	echo "ESMF_FieldGatherTest.rm target called here."


