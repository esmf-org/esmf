# $Id: makefile,v 1.2 2003/09/18 22:32:11 cdeluca Exp $ 

ALL:  system_tests

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
LOCDIR	  = src/system_tests/FieldHaloPeriodic


SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/FieldHaloPeriodic

# Object files other than ESMF_SysTest%.o that the
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
CLEANFILES  = *.o *.mod *.txt core $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_FieldHaloPeriodic:
	-@rm -f testlog stdlog.* logfile.* FieldHaloPeriodic.tmps
	-${MPIRUN} -np 12 $(ESMC_TESTDIR)/FieldHaloPeriodic > $(ESMC_TESTDIR)/FieldHaloPeriodic.stdout

RUN_FieldHaloPeriodicUNI:
	-@rm -f testlog stdlog.* logfile.* FieldHaloPeriodic.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/FieldHaloPeriodic > $(ESMC_TESTDIR)/FieldHaloPeriodic.stdout

FieldHaloPeriodic.rm:
	echo "FieldHaloPeriodic.rm target called here."


