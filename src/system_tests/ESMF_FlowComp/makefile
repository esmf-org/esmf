# $Id: makefile,v 1.3 2004/02/09 17:57:26 nscollins Exp $ 

ALL: build_system_tests

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  = 

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/system_tests/ESMF_FlowComp


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_FlowCompSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = ArraysGlobalMod.o FlowMod.o

SYSTEM_TESTS_RUN      = RUN_FlowComp  

SYSTEM_TESTS_RUN_UNI  = RUN_FlowCompUNI

SYSTEM_TESTS_RM       = FlowComp.rm

ESMF_FlowCompSTest.o : $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod core *velocity.* SIE.* $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_FlowComp:
	-@rm -f testlog stdlog.* logfile.* ESMF_FlowCompSTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_FlowCompSTest >$(ESMC_TESTDIR)/ESMF_FlowCompSTest.stdout

RUN_FlowCompUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_FlowCompSTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_FlowCompSTest >$(ESMC_TESTDIR)/ESMF_FlowCompSTest.stdout

FlowComp.rm:
	echo "FlowComp.rm target called here."



