# $Id: makefile,v 1.12 2003/05/23 15:42:10 flanigan Exp $ 

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
LOCDIR	  = src/SystemTests/69527

SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_SysTest69527

# Object files other than ESMF_SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     =

SYSTEM_TESTS_RUN     = RUN_ESMF_SysTest69527  

SYSTEM_TESTS_RUN_UNI = RUN_ESMF_SysTest69527UNI

SYSTEM_TESTS_RM      = ESMF_SysTest69527.rm

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt core $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_ESMF_SysTest69527:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest69527.tmps
	${MPIRUN} -np 2 $(ESMC_TESTDIR)/ESMF_SysTest69527 > $(ESMC_TESTDIR)/ESMF_SysTest69527.stdout

RUN_ESMF_SysTest69527UNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest69527.tmps
	${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_SysTest69527 > $(ESMC_TESTDIR)/ESMF_SysTest69527.stdout

ESMF_SysTest69527.rm:
	echo "ESMF_SysTest69527.rm target called here."



