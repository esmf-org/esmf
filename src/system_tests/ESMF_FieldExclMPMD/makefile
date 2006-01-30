# $Id: makefile,v 1.2 2006/01/30 18:11:25 nscollins Exp $ 

ALL: tree_build_system_tests

run: tree_run_system_tests

CFLAGS    = 
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/system_tests/ESMF_FieldExclMPMD


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_FieldExclMPMDSTestA $(ESMC_TESTDIR)/ESMF_FieldExclMPMDSTestB MPMDCLEANUP

# Object files other than *STest.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ_A     = user_model1.o user_coupler.o
SYSTEM_TESTS_OBJ_B     = user_model2.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_FieldExclMPMD  

SYSTEM_TESTS_RUN_UNI  = RUN_FieldExclUNI

include $(ESMF_DIR)/makefile

# The Darwin (Mac OS X), xlf 8.1 compiler cannot compile user_model1.F90
# with the -g flag (produces an internal compiler error).  removing the -g
# allows the file to build.
ifeq ($(ESMF_ARCH),Darwin)
ifeq ($(ESMF_COMPILER),xlf)
ifeq ($(ESMF_BOPT),g)
G_FFLAGS =
endif
endif
endif

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_FieldExclMPMD:
	-${MPIMPMDRUN} -np 1 $(ESMC_TESTDIR)/ESMF_FieldExclMPMDSTestA : -np 1 $(ESMC_TESTDIR)/ESMF_FieldExclMPMDSTestB : -np 1 $(ESMC_TESTDIR)/ESMF_FieldExclMPMDSTestA : -np 1 $(ESMC_TESTDIR)/ESMF_FieldExclMPMDSTestB : -np 1 $(ESMC_TESTDIR)/ESMF_FieldExclMPMDSTestA : -np 1 $(ESMC_TESTDIR)/ESMF_FieldExclMPMDSTestB : -np 2 $(ESMC_TESTDIR)/ESMF_FieldExclMPMDSTestA >$(ESMC_TESTDIR)/ESMF_FieldExclMPMDSTest.stdout

RUN_FieldExclUNI:
	$(MAKE) TNAME=FieldExcl NP=1 stest



