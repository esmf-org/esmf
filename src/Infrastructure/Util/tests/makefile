# $Id: makefile,v 1.7 2007/01/17 20:04:41 theurich Exp $ 

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
LOCDIR	  = src/Infrastructure/Util/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_UtilUTest \
                $(ESMF_TESTDIR)/ESMF_FortranWordsizeUTest \
                $(ESMF_TESTDIR)/ESMF_InitMacrosUTest 

TESTS_RUN     = RUN_ESMF_UtilUTest \
                RUN_ESMF_FortranWordsizeUTest \
                RUN_ESMF_InitMacrosUTest 

TESTS_RUN_UNI = RUN_ESMF_UtilUTestUNI \
                RUN_ESMF_FortranWordsizeUTestUNI \
                RUN_ESMF_InitMacrosUTestUNI


include ${ESMF_DIR}/makefile

DIRS        =

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
#  Util class
#

RUN_ESMF_UtilUTest:
	$(MAKE) TNAME=Util NP=4 ftest

RUN_ESMF_UtilUTestUNI:
	$(MAKE) TNAME=Util NP=1 ftest

#
# FortranWordsize
#

RUN_ESMF_FortranWordsizeUTest:
	$(MAKE) TNAME=FortranWordsize NP=4 ftest

RUN_ESMF_FortranWordsizeUTestUNI:
	$(MAKE) TNAME=FortranWordsize NP=1 ftest

#
# InitMacrosTest
#
UTEST_InitMacros_OBJS = ESMF_InitMacrosTestTypes.o
ESMF_InitMacrosUTest.o: ESMF_InitMacrosTestTypes.o

RUN_ESMF_InitMacrosUTest:
	$(MAKE) TNAME=InitMacros NP=4 ftest

RUN_ESMF_InitMacrosUTestUNI:
	$(MAKE) TNAME=InitMacros NP=1 ftest



