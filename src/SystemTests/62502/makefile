# $Id: makefile,v 1.14 2003/05/22 20:35:34 flanigan Exp $ 

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
LOCDIR	  = src/SystemTests/62502


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_SysTest62502

# Object files other than ESMF_SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_model1.o user_model2.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_ESMF_SysTest62502  

SYSTEM_TESTS_RUN_UNI  = RUN_ESMF_SysTest62502UNI

SYSTEM_TESTS_RM       = ESMF_SysTest62502.rm

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_ESMF_SysTest62502:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest62502.tmps
	${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_SysTest62502 >$(ESMC_TESTDIR)/ESMF_SysTest62502.stdout

RUN_ESMF_SysTest62502UNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_SysTest62502.tmps
	${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_SysTest62502 >$(ESMC_TESTDIR)/ESMF_SysTest62502.stdout

ESMF_SysTest62502.rm:
	echo "ESMF_SysTest62502.rm target called here."



