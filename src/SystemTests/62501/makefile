# $Id: makefile,v 1.12 2003/05/30 21:55:39 flanigan Exp $ 

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

SYSTEM_TESTS_BUILD    = warning $(ESMC_TESTDIR)/ESMF_SysTest62501  

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

warning: 
	@echo "*******************************************************************"
	@echo ""
	@echo "Warning: This system test is incompatible with the current ESMF"
	@echo "version, and is not expected to build or run.  Please see the"
	@echo "Test and Validation page on the Development link of the ESMF"
	@echo "website for details on the status of system tests."
	@echo ""
	@echo "*******************************************************************"


RUN_ESMF_SysTest62501:  
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest62501.tmps
	-${MPIRUN} -np 4 $(EMSC_TESTDIR)/ESMF_SysTest62501 > $(ESMC_TESTDIR)/ESMF_SysTest62501.stdout

RUN_ESMF_SysTest62501UNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest62501.tmps
	-${MPIRUN} -np 1 $(EMSC_TESTDIR)/ESMF_SysTest62501 > $(ESMC_TESTDIR)/ESMF_SysTest62501.stdout

ESMF_SysTest62501.rm:  
	echo "ESMF_SysTest62501.rm target called here."



