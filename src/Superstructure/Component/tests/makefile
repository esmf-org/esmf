# $Id: makefile,v 1.31 2005/04/12 16:52:57 nscollins Exp $ 

ALL:  build_unit_unit_tests

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

TESTS_BUILD   =	$(ESMC_TESTDIR)/ESMF_ComponentUTest \
		$(ESMC_TESTDIR)/ESMF_GridCompCreateUTest \
		$(ESMC_TESTDIR)/ESMC_CompCreate \
		$(ESMC_TESTDIR)/ESMF_CompRun \
		$(ESMC_TESTDIR)/ESMC_CompRun \
		$(ESMC_TESTDIR)/ESMF_CplCompCreateUTest

TESTS_RUN     =	RUN_ESMF_ComponentUTest \
		RUN_ESMF_GridCompCreateUTest \
		RUN_ESMF_CplCompCreateUTest \
		RUN_ESMC_CompCreate \
		RUN_ESMF_CompRun \
		RUN_ESMC_CompRun

TESTS_RUN_UNI =	RUN_ESMF_ComponentUTestUNI \
		RUN_ESMF_GridCompCreateUTestUNI \
		RUN_ESMF_CplCompCreateUTestUNI \
		RUN_ESMC_CompCreateUNI \
		RUN_ESMF_CompRunUNI \
		RUN_ESMC_CompRunUNI


include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =

#
# Components
#
RUN_ESMF_ComponentUTest:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_ComponentUTest > $(ESMC_TESTDIR)/ESMF_ComponentUTest.stdout
	-cat *ComponentUTest.Log > $(ESMC_TESTDIR)/ESMF_ComponentUTest.Log
	-rm *ComponentUTest.Log

RUN_ESMF_ComponentUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_ComponentUTest > $(ESMC_TESTDIR)/ESMF_ComponentUTest.stdout
	-cat *ComponentUTest.Log > $(ESMC_TESTDIR)/ESMF_ComponentUTest.Log
	-rm *ComponentUTest.Log


RUN_ESMF_GridCompCreateUTest:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_GridCompCreateUTest > $(ESMC_TESTDIR)/ESMF_GridCompCreateUTest.stdout
	-cat *GridCompCreateUTest.Log > $(ESMC_TESTDIR)/ESMF_GridCompCreateUTest.Log
	-rm *GridCompCreateUTest.Log

RUN_ESMF_GridCompCreateUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_GridCompCreateUTest > $(ESMC_TESTDIR)/ESMF_GridCompCreateUTest.stdout
	-cat *GridCompCreateUTest.Log > $(ESMC_TESTDIR)/ESMF_GridCompCreateUTest.Log
	-rm *GridCompCreateUTest.Log



$(ESMC_TESTDIR)/ESMC_CompCreate: ESMC_CompCreate.o
	-$(CLINKER) $(LINKOPTS) -o $(ESMC_TESTDIR)/ESMC_CompCreate ESMC_CompCreate.o -lesmf ${MPI_LIB} ${EXTRA_LIBS} ${CXXF90LIBS}
	${RM} -f ESMC_CompCreate.o

RUN_ESMC_CompCreate:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMC_CompCreate > $(ESMC_TESTDIR)/ESMC_CompCreate.stdout
	-cat *CompCreate.Log > $(ESMC_TESTDIR)/ESMC_CompCreate.Log
	-rm *CompCreate.Log

RUN_ESMC_CompCreateUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMC_CompCreate > $(ESMC_TESTDIR)/ESMC_CompCreate.stdout
	-cat *CompCreate.Log > $(ESMC_TESTDIR)/ESMC_CompCreate.Log
	-rm *CompCreate.Log


#
#
#
ESMF_UserRun.o: ESMF_UserGComp.o 

$(ESMC_TESTDIR)/ESMF_CompRun: ESMF_UserGComp.o ESMF_CompRun.o
	-$(FLINKER) $(LINKOPTS) -o $(ESMC_TESTDIR)/ESMF_CompRun ESMF_CompRun.o \
         ESMF_UserGComp.o -lesmf ${MPI_LIB} ${EXTRA_LIBS} ${F90CXXLIBS}
	${RM} -f ESMF_CompRun.o

RUN_ESMF_CompRun:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_CompRun > $(ESMC_TESTDIR)/ESMF_CompRun.stdout
	-cat *CompRun.Log > $(ESMC_TESTDIR)/ESMF_CompRun.Log
	-rm *CompRun.Log

RUN_ESMF_CompRunUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_CompRun > $(ESMC_TESTDIR)/ESMF_CompRun.stdout
	-cat *CompRun.Log > $(ESMC_TESTDIR)/ESMF_CompRun.Log
	-rm *CompRun.Log



#
#
#
ESMC_UserRun.o: ESMF_UserGComp.o ESMF_UserCComp.o ESMF_UserGComp_F.o
ESMF_UserGComp_F.o : ESMF_UserGComp.o

$(ESMC_TESTDIR)/ESMC_CompRun: ESMF_UserGComp.o ESMF_UserGComp_F.o ESMC_CompRun.o
	-$(CLINKER) $(LINKOPTS) -o $(ESMC_TESTDIR)/ESMC_CompRun \
         ESMC_CompRun.o ESMF_UserGComp.o ESMF_UserGComp_F.o -lesmf \
         ${MPI_LIB} ${EXTRA_LIBS} ${CXXF90LIBS}
	${RM} -f ESMC_CompRun.o ESMF_UserGComp_F.o ESMF_UserGComp.o

RUN_ESMC_CompRun:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMC_CompRun > $(ESMC_TESTDIR)/ESMC_CompRun.stdout
	-cat *CompRun.Log > $(ESMC_TESTDIR)/ESMC_CompRun.Log
	-rm *CompRun.Log

RUN_ESMC_CompRunUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMC_CompRun > $(ESMC_TESTDIR)/ESMC_CompRun.stdout
	-cat *CompRun.Log > $(ESMC_TESTDIR)/ESMC_CompRun.Log
	-rm *CompRun.Log


#
#
#

RUN_ESMF_CplCompCreateUTest:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_CplCompCreateUTest > $(ESMC_TESTDIR)/ESMF_CplCompCreateUTest.stdout
	-cat *CplCompCreateUTest.Log > $(ESMC_TESTDIR)/ESMF_CplCompCreateUTest.Log
	-rm *CplCompCreateUTest.Log

RUN_ESMF_CplCompCreateUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_CplCompCreateUTest > $(ESMC_TESTDIR)/ESMF_CplCompCreateUTest.stdout
	-cat *CplCompCreateUTest.Log > $(ESMC_TESTDIR)/ESMF_CplCompCreateUTest.Log
	-rm *CplCompCreateUTest.Log


