# $Id: makefile,v 1.1 2003/07/22 21:12:21 nscollins Exp $ 

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
LOCDIR	  = src/SystemTests/79497


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_SysTest79497

# Object files other than ESMF_SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_model1.o user_model2.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_ESMF_SysTest79497  

SYSTEM_TESTS_RUN_UNI  = RUN_ESMF_SysTest79497UNI

SYSTEM_TESTS_RM       = ESMF_SysTest79497.rm

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod
CLOBBERDIRS =


RUN_ESMF_SysTest79497:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest79497.tmps
	-${MPIRUN} -np 8 $(ESMC_TESTDIR)/ESMF_SysTest79497 >$(ESMC_TESTDIR)/ESMF_SysTest79497.stdout

RUN_ESMF_SysTest79497UNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest79497.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_SysTest79497 >$(ESMC_TESTDIR)/ESMF_SysTest79497.stdout

ESMF_SysTest79497.rm:
	echo "ESMF_SysTest79497.rm target called here."



