# $Id: makefile,v 1.10 2003/05/23 20:16:50 jwolfe Exp $ 

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
LOCDIR	  = src/SystemTests/62501

SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_SysTest62501  

# Object files other than ESMF_SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = 

SYSTEM_TESTS_RUN      = RUN_ESMF_SysTest62501

SYSTEM_TESTS_RUN_UNI  = RUN_ESMF_SysTest62501UNI

SYSTEM_TESTS_RM       = ESMF_SysTest62501.rm


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod
CLOBBERDIRS =


RUN_ESMF_SysTest62501:  
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest62501.tmps
	-${MPIRUN} -np 4 $(EMSC_TESTDIR)/ESMF_SysTest62501 > $(ESMC_TESTDIR)/ESMF_SysTest62501.stdout

RUN_ESMF_SysTest62501UNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest62501.tmps
	-${MPIRUN} -np 1 $(EMSC_TESTDIR)/ESMF_SysTest62501 > $(ESMC_TESTDIR)/ESMF_SysTest62501.stdout

ESMF_SysTest62501.rm:  
	echo "ESMF_SysTest62501.rm target called here."



