# $Id: makefile,v 1.17 2004/05/14 22:34:27 slswift Exp $ 

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

TESTS_BUILD   =	$(ESMC_TESTDIR)/ESMF_CompCreate \
		$(ESMC_TESTDIR)/ESMF_CompCreateUTest \
		$(ESMC_TESTDIR)/ESMC_CompCreate \
		$(ESMC_TESTDIR)/ESMF_CompRun \
		$(ESMC_TESTDIR)/ESMC_CompRun 

TESTS_RUN     =	RUN_ESMF_CompCreate \
		RUN_ESMF_CompCreateUTest \
		RUN_ESMC_CompCreate \
		RUN_ESMF_CompRun \
		RUN_ESMC_CompRun 

TESTS_RUN_UNI =	RUN_ESMF_CompCreateUNI \
		RUN_ESMF_CompCreateUTestUNI \
		RUN_ESMC_CompCreateUNI \
		RUN_ESMF_CompRunUNI \
		RUN_ESMC_CompRunUNI 

TESTS_RM      =	ESMF_CompCreate.rm \
		ESMF_CompCreateUTest.rm \
		ESMC_CompCreate.rm \
		ESMF_CompRun.rm \
		ESMC_CompRun.rm 

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod 
CLOBBERDIRS =

#
# Components
#
$(ESMC_TESTDIR)/ESMF_CompCreate: ESMF_CompCreate.o
	-$(SL_F_LINKER) -o $(ESMC_TESTDIR)/ESMF_CompCreate ESMF_CompCreate.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} ${NETCDF_LIB} $(SL_LINKOPTS)
	${RM} -f ESMF_CompCreate.o

RUN_ESMF_CompCreate:
	-@rm -f testlog stdlog.* logfile.* ESMF_CompCreate.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_CompCreate > $(ESMC_TESTDIR)/ESMF_CompCreate.stdout

RUN_ESMF_CompCreateUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_CompCreate.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_CompCreate > $(ESMC_TESTDIR)/ESMF_CompCreate.stdout

ESMF_CompCreate.rm:
	echo "ESMF_CompCreate.rm target called here."

$(ESMC_TESTDIR)/ESMF_CompCreateUTest: ESMF_CompCreateUTest.o
	-$(SL_F_LINKER) -o $(ESMC_TESTDIR)/ESMF_CompCreateUTest ESMF_CompCreateUTest.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} ${NETCDF_LIB} $(SL_LINKOPTS)
	${RM} -f ESMF_CompCreateUTest.o

RUN_ESMF_CompCreateUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_CompCreateUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_CompCreateUTest > $(ESMC_TESTDIR)/ESMF_CompCreateUTest.stdout

RUN_ESMF_CompCreateUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_CompCreateUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_CompCreateUTest > $(ESMC_TESTDIR)/ESMF_CompCreateUTest.stdout

ESMF_CompCreateUTest.rm:
	echo "ESMF_CompCreate.rm target called here."



$(ESMC_TESTDIR)/ESMC_CompCreate: ESMC_CompCreate.o
	-$(SL_C_LINKER) -o $(ESMC_TESTDIR)/ESMC_CompCreate ESMC_CompCreate.o -lesmf ${CXXF90LIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} ${NETCDF_LIB} $(SL_LINKOPTS)
	${RM} -f ESMC_CompCreate.o

RUN_ESMC_CompCreate:
	-@rm -f testlog stdlog.* logfile.* ESMC_CompCreate.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMC_CompCreate > $(ESMC_TESTDIR)/ESMC_CompCreate.stdout

RUN_ESMC_CompCreateUNI:
	-@rm -f testlog stdlog.* logfile.* ESMC_CompCreate.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMC_CompCreate > $(ESMC_TESTDIR)/ESMC_CompCreate.stdout

ESMC_CompCreate.rm:
	echo "ESMC_CompCreate.rm target called here."

#
#
#
ESMF_UserRun.o: ESMF_UserGComp.o 

$(ESMC_TESTDIR)/ESMF_CompRun: ESMF_UserGComp.o ESMF_CompRun.o
	-$(SL_F_LINKER) -o $(ESMC_TESTDIR)/ESMF_CompRun ESMF_CompRun.o \
         ESMF_UserGComp.o -lesmf ${F90CXXLIBS} ${MPI_LIB} \
         ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} ${NETCDF_LIB} $(SL_LINKOPTS)
	${RM} -f ESMF_CompRun.o

RUN_ESMF_CompRun:
	-@rm -f testlog stdlog.* logfile.* ESMF_CompRun.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_CompRun > $(ESMC_TESTDIR)/ESMF_CompRun.stdout

RUN_ESMF_CompRunUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_CompRun.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_CompRun > $(ESMC_TESTDIR)/ESMF_CompRun.stdout

ESMF_CompRun.rm:
	echo "ESMF_CompRun.rm target called here."


#
#
#
ESMC_UserRun.o: ESMF_UserGComp.o ESMF_UserCComp.o ESMF_UserGComp_F.o
ESMF_UserGComp_F.o : ESMF_UserGComp.o

$(ESMC_TESTDIR)/ESMC_CompRun: ESMF_UserGComp.o ESMF_UserGComp_F.o ESMC_CompRun.o
	-$(SL_C_LINKER) -o $(ESMC_TESTDIR)/ESMC_CompRun ESMC_CompRun.o \
         ESMF_UserGComp.o ESMF_UserGComp_F.o -lesmf ${F90CXXLIBS} ${MPI_LIB} \
         ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} ${NETCDF_LIB} $(SL_LINKOPTS)
	${RM} -f ESMC_CompRun.o ESMF_UserGComp_F.o ESMF_UserGComp.o

RUN_ESMC_CompRun:
	-@rm -f testlog stdlog.* logfile.* ESMC_CompRun.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMC_CompRun > $(ESMC_TESTDIR)/ESMC_CompRun.stdout

RUN_ESMC_CompRunUNI:
	-@rm -f testlog stdlog.* logfile.* ESMC_CompRun.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMC_CompRun > $(ESMC_TESTDIR)/ESMC_CompRun.stdout

ESMC_CompRun.rm:
	echo "ESMC_CompRun.rm target called here."

