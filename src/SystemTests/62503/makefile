# $Id: makefile,v 1.6 2003/05/23 20:18:04 jwolfe Exp $ 

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
LOCDIR	  = src/SystemTests/62503


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_SysTest62503

# Object files other than ESMF_SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_model1.o user_model2.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_ESMF_SysTest62503  

SYSTEM_TESTS_RUN_UNI  = RUN_ESMF_SysTest62503UNI

SYSTEM_TESTS_RM       = ESMF_SysTest62503.rm

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod
CLOBBERDIRS =


RUN_ESMF_SysTest62503:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest62503.tmps
	-${MPIRUN} -np 8 $(ESMC_TESTDIR)/ESMF_SysTest62503 >$(ESMC_TESTDIR)/ESMF_SysTest62503.stdout

RUN_ESMF_SysTest62503UNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest62503.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_SysTest62503 >$(ESMC_TESTDIR)/ESMF_SysTest62503.stdout

ESMF_SysTest62503.rm:
	echo "ESMF_SysTest62503.rm target called here."



