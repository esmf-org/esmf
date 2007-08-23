# $Id: makefile,v 1.43 2007/08/23 17:16:06 cdeluca Exp $ 

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
LOCDIR	  = src/Superstructure/Component/tests

# TODO: these are very old tests which predate the current unit test
#  conventions for file names and for using the test macros.
#  when someone has the time, these files should be renamed
#  to have UTest in the names, and should have the standard test macros
#  handle the testing and results.  until then, i have removed them from
#  the build list because the results are not tabulated and the files are
#  simply left around and not cleaned out when unit tests are cleaned.
#
#TESTS_BUILD   =
#		$(ESMF_TESTDIR)/ESMC_CompCreate \
#		$(ESMF_TESTDIR)/ESMF_CompRun \
#		$(ESMF_TESTDIR)/ESMC_CompRun \
#TESTS_RUN     =
#		RUN_ESMC_CompCreate \
#		RUN_ESMF_CompRun \
#		RUN_ESMC_CompRun
#TESTS_RUN_UNI =
#		RUN_ESMC_CompCreateUNI \
#		RUN_ESMF_CompRunUNI \
#		RUN_ESMC_CompRunUNI
#

TESTS_BUILD   =	$(ESMF_TESTDIR)/ESMF_ComponentUTest \
		$(ESMF_TESTDIR)/ESMF_GridCompCreateUTest \
		$(ESMF_TESTDIR)/ESMF_CompSetServUTest \
		$(ESMF_TESTDIR)/ESMF_CplCompCreateUTest

TESTS_RUN     =	RUN_ESMF_ComponentUTest \
		RUN_ESMF_GridCompCreateUTest \
		RUN_ESMF_CompSetServUTest \
		RUN_ESMF_CplCompCreateUTest

TESTS_RUN_UNI =	RUN_ESMF_ComponentUTestUNI \
		RUN_ESMF_GridCompCreateUTestUNI \
		RUN_ESMF_CompSetServUTestUNI \
		RUN_ESMF_CplCompCreateUTestUNI


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

# 
ESMF_CompSetServUTest.o: ESMF_SetServCode.o

UTEST_CompSetServ_OBJS = ESMF_SetServCode.o


#
# Components
#
RUN_ESMF_ComponentUTest:
	cp -f grid.rc $(ESMF_TESTDIR)
	$(MAKE) TNAME=Component NP=4 ftest

RUN_ESMF_ComponentUTestUNI:
	cp -f grid.rc $(ESMF_TESTDIR)
	$(MAKE) TNAME=Component NP=1 ftest


RUN_ESMF_GridCompCreateUTest:
	cp -f grid.rc $(ESMF_TESTDIR)
	$(MAKE) TNAME=GridCompCreate NP=4 ftest

RUN_ESMF_GridCompCreateUTestUNI:
	cp -f grid.rc $(ESMF_TESTDIR)
	$(MAKE) TNAME=GridCompCreate NP=1 ftest


RUN_ESMF_CplCompCreateUTest:
	cp -f grid.rc $(ESMF_TESTDIR)
	$(MAKE) TNAME=CplCompCreate NP=4 ftest

RUN_ESMF_CplCompCreateUTestUNI:
	cp -f grid.rc $(ESMF_TESTDIR)
	$(MAKE) TNAME=CplCompCreate NP=1 ftest


RUN_ESMF_CompSetServUTest:
	cp -f grid.rc $(ESMF_TESTDIR)
	$(MAKE) TNAME=CompSetServ NP=4 ftest

RUN_ESMF_CompSetServUTestUNI:
	cp -f grid.rc $(ESMF_TESTDIR)
	$(MAKE) TNAME=CompSetServ NP=1 ftest

#
# older tests
#

$(ESMC_TESTDIR)/ESMC_CompCreate: ESMC_CompCreate.o
	-$(CLINKER) $(LINKOPTS) -o $(ESMC_TESTDIR)/ESMC_CompCreate \
         ESMC_CompCreate.o $(CLINKLIBS)
	${ESMF_RM} -f ESMC_CompCreate.o

RUN_ESMC_CompCreate:
	cp -f grid.rc $(ESMF_TESTDIR)
	-${ESMF_MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMC_CompCreate > $(ESMC_TESTDIR)/ESMC_CompCreate.stdout

RUN_ESMC_CompCreateUNI:
	cp -f grid.rc $(ESMF_TESTDIR)
	-${ESMF_MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMC_CompCreate > $(ESMC_TESTDIR)/ESMC_CompCreate.stdout


#
#
#
ESMF_UserRun.o: ESMF_UserGComp.o 

$(ESMC_TESTDIR)/ESMF_CompRun: ESMF_UserGComp.o ESMF_CompRun.o
	-$(FLINKER) $(LINKOPTS) -o $(ESMC_TESTDIR)/ESMF_CompRun ESMF_CompRun.o \
         ESMF_UserGComp.o $(FLINKLIBS)
	${ESMF_RM} -f ESMF_CompRun.o

RUN_ESMF_CompRun:
	cp -f grid.rc $(ESMF_TESTDIR)
	-${ESMF_MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_CompRun > $(ESMC_TESTDIR)/ESMF_CompRun.stdout

RUN_ESMF_CompRunUNI:
	cp -f grid.rc $(ESMF_TESTDIR)
	-${ESMF_MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_CompRun > $(ESMC_TESTDIR)/ESMF_CompRun.stdout



#
#
#
ESMC_UserRun.o: ESMF_UserGComp.o ESMF_UserCComp.o ESMF_UserGComp_F.o
ESMF_UserGComp_F.o : ESMF_UserGComp.o

$(ESMC_TESTDIR)/ESMC_CompRun: ESMF_UserGComp.o ESMF_UserGComp_F.o ESMC_CompRun.o
	-$(CLINKER) $(LINKOPTS) -o $(ESMC_TESTDIR)/ESMC_CompRun \
         ESMC_CompRun.o ESMF_UserGComp.o ESMF_UserGComp_F.o $(CLINKLIBS)
	${ESMF_RM} -f ESMC_CompRun.o ESMF_UserGComp_F.o ESMF_UserGComp.o

RUN_ESMC_CompRun:
	cp -f grid.rc $(ESMF_TESTDIR)
	-${ESMF_MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMC_CompRun > $(ESMC_TESTDIR)/ESMC_CompRun.stdout

RUN_ESMC_CompRunUNI:
	cp -f grid.rc $(ESMF_TESTDIR)
	-${ESMF_MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMC_CompRun > $(ESMC_TESTDIR)/ESMC_CompRun.stdout


