# $Id: makefile,v 1.15 2003/04/18 17:18:34 flanigan Exp $ 

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
LOCDIR	  = src/SystemTests/70385


SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_SysTest70385

# Object files other than ESMF_SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     =

SYSTEM_TESTS_RUN     = RUN_ESMF_SysTest70385  

SYSTEM_TESTS_RUN_UNI = RUN_ESMF_SysTest70385UNI

SYSTEM_TESTS_RM      = ESMF_SysTest70385.rm

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod
CLOBBERDIRS =


RUN_ESMF_SysTest70385:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest70385.tmps
	${MPIRUN} -np 12 $(ESMC_TESTDIR)/ESMF_SysTest70385 > $(ESMC_TESTDIR)/ESMF_SysTest70385.stdout

RUN_ESMF_SysTest70385UNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest70385.tmps
	${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_SysTest70385 > $(ESMC_TESTDIR)/ESMF_SysTest70385.stdout

ESMF_SysTest70385.rm:
	echo "ESMF_SysTest70385.rm target called here."


