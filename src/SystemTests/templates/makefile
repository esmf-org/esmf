# $Id: makefile,v 1.5 2003/05/23 15:44:33 flanigan Exp $ 

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
LOCDIR	  = src/SystemTests/NNNNN

SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_SysTestNNNNN

SYSTEM_TESTS_RUN      = RUN_ESMF_SysTestNNNNN

SYSTEM_TESTS_RUN_UNI  = RUN_ESMF_SysTestNNNNNUNI

SYSTEM_TESTS_RM       = ESMF_SysTestNNNNN.rm

include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt core $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =

RUN_ESMF_SysTestNNNNN:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTestNNNNN.tmps
	${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_SysTestNNNNN > $(ESMC_TESTDIR)/ESMF_SysTestNNNNN.stdout


RUN_ESMF_SysTestNNNNNUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTestNNNNN.tmps
	${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_SysTestNNNNN > $(ESMC_TESTDIR)/ESMF_SysTestNNNNN.stdout


ESMF_SysTestNNNNN.rm:
	echo "ESMF_SysTestNNNNN.rm target called here."



