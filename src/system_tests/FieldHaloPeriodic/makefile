# $Id: makefile,v 1.1 2003/09/18 21:26:47 cdeluca Exp $ 

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
LOCDIR	  = src/SystemTests/82899


SYSTEM_TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_SysTest82899

# Object files other than ESMF_SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ     =

SYSTEM_TESTS_RUN     = RUN_ESMF_SysTest82899  

SYSTEM_TESTS_RUN_UNI = RUN_ESMF_SysTest82899UNI

SYSTEM_TESTS_RM      = ESMF_SysTest82899.rm

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt core $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_ESMF_SysTest82899:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest82899.tmps
	-${MPIRUN} -np 12 $(ESMC_TESTDIR)/ESMF_SysTest82899 > $(ESMC_TESTDIR)/ESMF_SysTest82899.stdout

RUN_ESMF_SysTest82899UNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest82899.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_SysTest82899 > $(ESMC_TESTDIR)/ESMF_SysTest82899.stdout

ESMF_SysTest82899.rm:
	echo "ESMF_SysTest82899.rm target called here."


