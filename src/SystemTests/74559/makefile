# $Id: makefile,v 1.10 2003/05/22 20:35:35 flanigan Exp $ 

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
LOCDIR	  = src/SystemTests/74559


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_SysTest74559

# Object files other than ESMF_SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = FlowArraysMod.o InjectArraysMod.o \
                        FlowSolverMod.o InjectorMod.o CouplerMod.o 

SYSTEM_TESTS_RUN      = RUN_ESMF_SysTest74559  

SYSTEM_TESTS_RUN_UNI  = RUN_ESMF_SysTest74559UNI

SYSTEM_TESTS_RM       = ESMF_SysTest74559.rm

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_ESMF_SysTest74559:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest74559.tmps
	${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_SysTest74559 >$(ESMC_TESTDIR)/ESMF_SysTest74559.stdout

RUN_ESMF_SysTest74559UNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest74559.tmps
	${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_SysTest74559 >$(ESMC_TESTDIR)/ESMF_SysTest74559.stdout

ESMF_SysTest74559.rm:
	echo "ESMF_SysTest74559.rm target called here."



