# $Id: makefile,v 1.2 2003/09/18 22:47:18 cdeluca Exp $ 

ALL: system_tests

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  = 

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/system_tests/FlowComp


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/FlowComp

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = ArraysGlobalMod.o FlowMod.o

SYSTEM_TESTS_RUN      = RUN_FlowComp  

SYSTEM_TESTS_RUN_UNI  = RUN_FlowCompUNI

SYSTEM_TESTS_RM       = FlowComp.rm

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod core *velocity.* SIE.* $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_FlowComp:
	-@rm -f testlog stdlog.* logfile.* FlowComp.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/FlowComp >$(ESMC_TESTDIR)/FlowComp.stdout

RUN_FlowCompUNI:
	-@rm -f testlog stdlog.* logfile.* FlowComp.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/FlowComp >$(ESMC_TESTDIR)/FlowComp.stdout

FlowComp.rm:
	echo "FlowComp.rm target called here."



