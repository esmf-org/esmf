# $Id: makefile,v 1.27 2005/03/15 17:22:37 nscollins Exp $ 

ALL:  build_tests

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

RUN_ESMF_ComponentUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_ComponentUTest > $(ESMC_TESTDIR)/ESMF_ComponentUTest.stdout


RUN_ESMF_GridCompCreateUTest:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_GridCompCreateUTest > $(ESMC_TESTDIR)/ESMF_GridCompCreateUTest.stdout

RUN_ESMF_GridCompCreateUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_GridCompCreateUTest > $(ESMC_TESTDIR)/ESMF_GridCompCreateUTest.stdout



$(ESMC_TESTDIR)/ESMC_CompCreate: ESMC_CompCreate.o
	-$(SL_C_LINKER) -o $(ESMC_TESTDIR)/ESMC_CompCreate ESMC_CompCreate.o -lesmf ${MPI_LIB} ${MP_LIB} ${NETCDF_LIB} ${THREAD_LIB} ${PCL_LIB} ${CXXF90LIBS} $(SL_LINKOPTS)
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
	-$(SL_F_LINKER) -o $(ESMC_TESTDIR)/ESMF_CompRun ESMF_CompRun.o \
         ESMF_UserGComp.o -lesmf ${MPI_LIB} ${MP_LIB} ${NETCDF_LIB} \
        ${THREAD_LIB} ${PCL_LIB} ${F90CXXLIBS} $(SL_LINKOPTS)
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
	-$(SL_C_LINKER) -o $(ESMC_TESTDIR)/ESMC_CompRun ESMC_CompRun.o \
         ESMF_UserGComp.o ESMF_UserGComp_F.o -lesmf ${MPI_LIB} ${MP_LIB} \
        ${NETCDF_LIB} ${THREAD_LIB} ${PCL_LIB} ${CXXF90LIBS} $(SL_LINKOPTS)
	${RM} -f ESMC_CompRun.o ESMF_UserGComp_F.o ESMF_UserGComp.o

RUN_ESMC_CompRun:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMC_CompRun > $(ESMC_TESTDIR)/ESMC_CompRun.stdout

RUN_ESMC_CompRunUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMC_CompRun > $(ESMC_TESTDIR)/ESMC_CompRun.stdout


#
#
#

RUN_ESMF_CplCompCreateUTest:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_CplCompCreateUTest > $(ESMC_TESTDIR)/ESMF_CplCompCreateUTest.stdout

RUN_ESMF_CplCompCreateUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_CplCompCreateUTest > $(ESMC_TESTDIR)/ESMF_CplCompCreateUTest.stdout


