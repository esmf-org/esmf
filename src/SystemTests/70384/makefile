# $Id: makefile,v 1.11 2003/05/22 20:35:35 flanigan Exp $ 

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
LOCDIR	  = src/SystemTests/70384

SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_SysTest70384

# Object files other than ESMF_SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     =

SYSTEM_TESTS_RUN     = RUN_ESMF_SysTest70384  

SYSTEM_TESTS_RUN_UNI = RUN_ESMF_SysTest70384UNI

SYSTEM_TESTS_RM      = ESMF_SysTest70384.rm

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_ESMF_SysTest70384:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest70384.tmps
	${MPIRUN} -np 6 $(ESMC_TESTDIR)/ESMF_SysTest70384 > $(ESMC_TESTDIR)/ESMF_SysTest70384.stdout

RUN_ESMF_SysTest70384UNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest70384.tmps
	${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_SysTest70384 > $(ESMC_TESTDIR)/ESMF_SysTest70384.stdout

ESMF_SysTest70384.rm:
	echo "ESMF_SysTest70384.rm target called here."



