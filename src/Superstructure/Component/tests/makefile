# $Id: makefile,v 1.35 2005/06/28 19:56:14 svasquez Exp $ 

ALL:  build_unit_tests

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
		$(ESMF_TESTDIR)/ESMF_CplCompCreateUTest

TESTS_RUN     =	RUN_ESMF_ComponentUTest \
		RUN_ESMF_GridCompCreateUTest \
		RUN_ESMF_CplCompCreateUTest

TESTS_RUN_UNI =	RUN_ESMF_ComponentUTestUNI \
		RUN_ESMF_GridCompCreateUTestUNI \
		RUN_ESMF_CplCompCreateUTestUNI


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

#
# Components
#
RUN_ESMF_ComponentUTest:
	-${RM} *ComponentUTest.Log
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_ComponentUTest > $(ESMC_TESTDIR)/ESMF_ComponentUTest.stdout
	-cat *ComponentUTest.Log > $(ESMC_TESTDIR)/ESMF_ComponentUTest.Log
	-${RM} *ComponentUTest.Log

RUN_ESMF_ComponentUTestUNI:
	-${RM} *ComponentUTest.Log
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_ComponentUTest > $(ESMC_TESTDIR)/ESMF_ComponentUTest.stdout
	-cat *ComponentUTest.Log > $(ESMC_TESTDIR)/ESMF_ComponentUTest.Log
	-${RM} *ComponentUTest.Log


RUN_ESMF_GridCompCreateUTest:
	-${RM} *GridCompCreateUTest.Log
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_GridCompCreateUTest > $(ESMC_TESTDIR)/ESMF_GridCompCreateUTest.stdout
	-cat *GridCompCreateUTest.Log > $(ESMC_TESTDIR)/ESMF_GridCompCreateUTest.Log
	-${RM} *GridCompCreateUTest.Log

RUN_ESMF_GridCompCreateUTestUNI:
	-${RM} *GridCompCreateUTest.Log
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_GridCompCreateUTest > $(ESMC_TESTDIR)/ESMF_GridCompCreateUTest.stdout
	-cat *GridCompCreateUTest.Log > $(ESMC_TESTDIR)/ESMF_GridCompCreateUTest.Log
	-${RM} *GridCompCreateUTest.Log



$(ESMC_TESTDIR)/ESMC_CompCreate: ESMC_CompCreate.o
	-$(CLINKER) $(LINKOPTS) -o $(ESMC_TESTDIR)/ESMC_CompCreate \
         ESMC_CompCreate.o $(CLINKLIBS)
	${RM} -f ESMC_CompCreate.o

RUN_ESMC_CompCreate:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMC_CompCreate > $(ESMC_TESTDIR)/ESMC_CompCreate.stdout

RUN_ESMC_CompCreateUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMC_CompCreate > $(ESMC_TESTDIR)/ESMC_CompCreate.stdout


#
#
#
ESMF_UserRun.o: ESMF_UserGComp.o 

$(ESMC_TESTDIR)/ESMF_CompRun: ESMF_UserGComp.o ESMF_CompRun.o
	-$(FLINKER) $(LINKOPTS) -o $(ESMC_TESTDIR)/ESMF_CompRun ESMF_CompRun.o \
         ESMF_UserGComp.o $(FLINKLIBS)
	${RM} -f ESMF_CompRun.o

RUN_ESMF_CompRun:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_CompRun > $(ESMC_TESTDIR)/ESMF_CompRun.stdout

RUN_ESMF_CompRunUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_CompRun > $(ESMC_TESTDIR)/ESMF_CompRun.stdout



#
#
#
ESMC_UserRun.o: ESMF_UserGComp.o ESMF_UserCComp.o ESMF_UserGComp_F.o
ESMF_UserGComp_F.o : ESMF_UserGComp.o

$(ESMC_TESTDIR)/ESMC_CompRun: ESMF_UserGComp.o ESMF_UserGComp_F.o ESMC_CompRun.o
	-$(CLINKER) $(LINKOPTS) -o $(ESMC_TESTDIR)/ESMC_CompRun \
         ESMC_CompRun.o ESMF_UserGComp.o ESMF_UserGComp_F.o $(CLINKLIBS)
	${RM} -f ESMC_CompRun.o ESMF_UserGComp_F.o ESMF_UserGComp.o

RUN_ESMC_CompRun:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMC_CompRun > $(ESMC_TESTDIR)/ESMC_CompRun.stdout

RUN_ESMC_CompRunUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMC_CompRun > $(ESMC_TESTDIR)/ESMC_CompRun.stdout


#
#
#

RUN_ESMF_CplCompCreateUTest:
	-${RM} *CplCompCreateUTest.Log
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_CplCompCreateUTest > $(ESMC_TESTDIR)/ESMF_CplCompCreateUTest.stdout
	-cat *CplCompCreateUTest.Log > $(ESMC_TESTDIR)/ESMF_CplCompCreateUTest.Log
	-${RM} *CplCompCreateUTest.Log

RUN_ESMF_CplCompCreateUTestUNI:
	-${RM} *CplCompCreateUTest.Log
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_CplCompCreateUTest > $(ESMC_TESTDIR)/ESMF_CplCompCreateUTest.stdout
	-cat *CplCompCreateUTest.Log > $(ESMC_TESTDIR)/ESMF_CplCompCreateUTest.Log
	-${RM} *CplCompCreateUTest.Log


