# $Id: makefile,v 1.1 2003/10/09 20:56:12 cdeluca Exp $ 

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
LOCDIR	  = src/system_tests/ESMF_FieldHalo


SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_FieldHaloSTest

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
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldHaloSTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_FieldHaloSTest > $(ESMC_TESTDIR)/ESMF_FieldHaloSTest.stdout

RUN_FieldHaloUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldHaloSTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_FieldHaloSTest > $(ESMC_TESTDIR)/ESMF_FieldHaloSTest.stdout

FieldHalo.rm:
	echo "FieldHalo.rm target called here."


