# $Id: makefile,v 1.2 2003/09/18 22:26:08 cdeluca Exp $ 

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
LOCDIR	  = src/system_tests/FieldHalo


SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/FieldHalo

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     =

SYSTEM_TESTS_RUN     = RUN_FieldHalo  

SYSTEM_TESTS_RUN_UNI = RUN_FieldHaloUNI

SYSTEM_TESTS_RM      = FieldHalo.rm

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt core $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_FieldHalo:
	-@rm -f testlog stdlog.* logfile.* FieldHalo.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/FieldHalo > $(ESMC_TESTDIR)/FieldHalo.stdout

RUN_FieldHaloUNI:
	-@rm -f testlog stdlog.* logfile.* FieldHalo.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/FieldHalo > $(ESMC_TESTDIR)/FieldHalo.stdout

FieldHalo.rm:
	echo "FieldHalo.rm target called here."


