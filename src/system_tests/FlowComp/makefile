# $Id: makefile,v 1.1 2003/09/18 21:26:47 cdeluca Exp $ 

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
LOCDIR	  = src/SystemTests/74558


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_SysTest74558

# Object files other than ESMF_SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = ArraysGlobalMod.o FlowMod.o

SYSTEM_TESTS_RUN      = RUN_ESMF_SysTest74558  

SYSTEM_TESTS_RUN_UNI  = RUN_ESMF_SysTest74558UNI

SYSTEM_TESTS_RM       = ESMF_SysTest74558.rm

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod core *velocity.* SIE.* $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_ESMF_SysTest74558:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest74558.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_SysTest74558 >$(ESMC_TESTDIR)/ESMF_SysTest74558.stdout

RUN_ESMF_SysTest74558UNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest74558.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_SysTest74558 >$(ESMC_TESTDIR)/ESMF_SysTest74558.stdout

ESMF_SysTest74558.rm:
	echo "ESMF_SysTest74558.rm target called here."



