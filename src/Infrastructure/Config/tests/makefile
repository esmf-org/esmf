# $Id: makefile,v 1.11 2005/10/12 19:06:16 nscollins Exp $

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
LOCDIR	  =  src/Infrastructure/Config/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_ConfigUTest
#	        $(ESMF_TESTDIR)/ESMC_ConfigUTest 

TESTS_RUN     = RUN_ESMF_ConfigUTest
#		RUN_ESMC_ConfigUTest 

TESTS_RUN_UNI = RUN_ESMF_ConfigUTestUNI
#		RUN_ESMC_ConfigUTestUNI 




include ${ESMF_DIR}/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

#
# ESMF_ConfigUTest
#
RUN_ESMF_ConfigUTest:
	cp -f ESMF_Resource_File_Sample.rc $(ESMF_TESTDIR)
	$(MAKE) TNAME=Config NP=4 ftest

RUN_ESMF_ConfigUTestUNI:
	cp -f ESMF_Resource_File_Sample.rc $(ESMF_TESTDIR)
	$(MAKE) TNAME=Config NP=1 ftest


#
# ESMC_ConfigUTest
#
RUN_ESMC_ConfigUTest:
	cp -f ESMF_Resource_File_Sample.rc $(ESMF_TESTDIR)
	$(MAKE) TNAME=Config NP=4 ctest

RUN_ESMC_ConfigUTestUNI:
	cp -f ESMF_Resource_File_Sample.rc $(ESMF_TESTDIR)
	$(MAKE) TNAME=Config NP=1 ctest



