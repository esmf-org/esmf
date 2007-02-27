# $Id: makefile,v 1.8 2007/02/27 20:50:47 rosalind Exp $ 

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
                $(ESMF_TESTDIR)/ESMF_TypeKindGetUTest \
                $(ESMF_TESTDIR)/ESMF_InitMacrosUTest 

TESTS_RUN     = RUN_ESMF_UtilUTest \
                RUN_ESMF_FortranWordsizeUTest \
                RUN_ESMF_TypeKindGetUTest \
                RUN_ESMF_InitMacrosUTest 

TESTS_RUN_UNI = RUN_ESMF_UtilUTestUNI \
                RUN_ESMF_FortranWordsizeUTestUNI \
                RUN_ESMF_TypeKindGetUTestUNI \
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
# TypeKindGet
#

RUN_ESMF_TypeKindGetUTest:
	$(MAKE) TNAME=TypeKindGet NP=4 ftest

RUN_ESMF_TypeKindGetUTestUNI:
	$(MAKE) TNAME=TypeKindGet NP=1 ftest

#
# InitMacrosTest
#
UTEST_InitMacros_OBJS = ESMF_InitMacrosTestTypes.o
ESMF_InitMacrosUTest.o: ESMF_InitMacrosTestTypes.o

RUN_ESMF_InitMacrosUTest:
	$(MAKE) TNAME=InitMacros NP=4 ftest

RUN_ESMF_InitMacrosUTestUNI:
	$(MAKE) TNAME=InitMacros NP=1 ftest



