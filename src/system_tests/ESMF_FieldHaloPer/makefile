# $Id: makefile,v 1.3 2004/08/03 19:33:53 nscollins Exp $ 

ALL:  build_system_tests

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
LOCDIR	  = src/system_tests/ESMF_FieldHaloPer


SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_FieldHaloPerSTest

# Object files other than ESMF_%STest.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     =

SYSTEM_TESTS_RUN     = RUN_FieldHaloPeriodic  

SYSTEM_TESTS_RUN_UNI = RUN_FieldHaloPeriodicUNI

SYSTEM_TESTS_RM      = FieldHaloPeriodic.rm

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt core ESMF_LogFile $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_FieldHaloPeriodic:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldHaloPerSTest.tmps
	-${MPIRUN} -np 12 $(ESMC_TESTDIR)/ESMF_FieldHaloPerSTest > $(ESMC_TESTDIR)/ESMF_FieldHaloPerSTest.stdout

RUN_FieldHaloPeriodicUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldHaloPerSTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_FieldHaloPerSTest > $(ESMC_TESTDIR)/ESMF_FieldHaloPerSTest.stdout

FieldHaloPeriodic.rm:
	echo "FieldHaloPeriodic.rm target called here."


