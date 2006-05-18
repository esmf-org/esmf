# $Id: makefile,v 1.1 2006/05/18 18:32:44 theurich Exp $ 

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
LOCDIR	  = src/system_tests/ESMF_ArraySparseMatMul


SYSTEM_TESTS_BUILD    = $(ESMC_TESTDIR)/ESMF_ArraySparseMatMulSTest

# Object files other than SysTest%.o that the
# system tests executable will depend on.
# List objects files in the order that they
# are to be compiled/created.
SYSTEM_TESTS_OBJ      = user_model1.o user_model2.o user_coupler.o

SYSTEM_TESTS_RUN      = RUN_ArraySparseMatMul

SYSTEM_TESTS_RUN_UNI  = RUN_ArraySparseMatMulUNI


ESMF_ArraySparseMatMulSTest.o : $(SYSTEM_TESTS_OBJ)

include $(ESMF_DIR)/makefile

#todo: check if this is still an issue:
# The Darwin (Mac OS X), xlf 8.1 compiler cannot compile user_coupler.F90
# with the -g flag (produces an internal compiler error).  removing the -g
# allows the file to build.
#ifeq ($(ESMF_ARCH),Darwin)
#ifeq ($(ESMF_COMPILER),xlf)
#ifeq ($(ESMF_BOPT),g)
#G_FFLAGS =
#endif
#endif
#endif

DIRS = 

CLEANDIRS   =
CLEANFILES  = $(SYSTEM_TESTS_BUILD)
CLOBBERDIRS =


RUN_ArraySparseMatMul:
	$(MAKE) TNAME=ArraySparseMatMul NP=6 stest

RUN_ArraySparseMatMulUNI:
	$(MAKE) TNAME=ArraySparseMatMul NP=1 stest
