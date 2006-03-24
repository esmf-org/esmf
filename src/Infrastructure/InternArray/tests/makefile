# $Id: makefile,v 1.1 2006/03/24 16:33:28 theurich Exp $ 

ALL:  build_unit_tests
run:  run_unit_tests

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Infrastructure/InternArray/tests


TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_InternArrayDataMapUTest \
                $(ESMF_TESTDIR)/ESMF_InternArrayUTest \
		$(ESMF_TESTDIR)/ESMF_InternArrayGatherUTest

TESTS_RUN     = RUN_ESMF_InternArrayDataMapUTest \
                RUN_ESMF_InternArrayUTest \
		RUN_ESMF_InternArrayGatherUTest

TESTS_RUN_UNI = RUN_ESMF_InternArrayDataMapUTestUNI \
                RUN_ESMF_InternArrayUTestUNI \
		RUN_ESMF_InternArrayGatherUTestUNI



include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD) 
CLOBBERDIRS =


#
# Fortran InternArrayDataMap unit test
#
RUN_ESMF_InternArrayDataMapUTest:
	$(MAKE) TNAME=InternArrayDataMap NP=4 ftest

RUN_ESMF_InternArrayDataMapUTestUNI:
	$(MAKE) TNAME=InternArrayDataMap NP=1 ftest



#
# C++ InternArrayDataMap Unit tests
#

RUN_ESMC_InternArrayDataMapUTest:
	$(MAKE) TNAME=InternArrayDataMap NP=4 ctest

RUN_ESMC_InternArrayDataMapUTestUNI:
	$(MAKE) TNAME=InternArrayDataMap NP=1 ctest



#
#  ESMF_InternArrayUTest
#
RUN_ESMF_InternArrayUTest:
	$(MAKE) TNAME=InternArray NP=4 ftest

RUN_ESMF_InternArrayUTestUNI:
	$(MAKE) TNAME=InternArray NP=1 ftest



#
#  ESMF_InternArrayGatherUTest
#
RUN_ESMF_InternArrayGatherUTest:
	$(MAKE) TNAME=InternArrayGather NP=4 ftest

RUN_ESMF_InternArrayGatherUTestUNI:
	$(MAKE) TNAME=InternArrayGather NP=1 ftest




