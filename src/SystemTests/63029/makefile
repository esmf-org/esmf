# $Id: makefile,v 1.22 2003/05/23 20:18:36 jwolfe Exp $ 

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
LOCDIR	  = src/SystemTests/63029


SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_SysTest63029

# Object files other than ESMF_SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     = user_model.o

SYSTEM_TESTS_RUN     = RUN_ESMF_SysTest63029

SYSTEM_TESTS_RUN_UNI = RUN_ESMF_SysTest63029UNI

SYSTEM_TESTS_RM      = ESMF_SysTest63029.rm

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt core $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_ESMF_SysTest63029:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest63029.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_SysTest63029 > $(ESMC_TESTDIR)/ESMF_SysTest63029.stdout

RUN_ESMF_SysTest63029UNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest63029.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_SysTest63029 > $(ESMC_TESTDIR)/ESMF_SysTest63029.stdout

ESMF_SysTest63029.rm:
	echo "ESMF_SysTest63029.rm target called here."



