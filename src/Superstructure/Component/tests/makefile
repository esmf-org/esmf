# $Id: makefile,v 1.9 2003/04/09 23:48:44 flanigan Exp $ 

ALL:  tests

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
		$(ESMC_TESTDIR)/ESMF_CompRun \
		$(ESMC_TESTDIR)/ESMF_StateCreate \
		$(ESMC_TESTDIR)/ESMF_StateUTest

TESTS_RUN     =	RUN_ESMF_CompCreate \
		RUN_ESMF_CompRun \
		RUN_ESMF_StateCreate \
		RUN_ESMF_StateUTest

TESTS_RUN_UNI =	RUN_ESMF_CompCreateUNI \
		RUN_ESMF_CompRunUNI \
		RUN_ESMF_StateCreateUNI \
		RUN_ESMF_StateUTestUNI

TESTS_RM      =	ESMF_CompCreate.rm \
		ESMF_CompRun.rm \
		ESMF_StateCreate.rm \
		ESMF_StateUTest.rm

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod 
CLOBBERDIRS =

#
# Components
#
$(ESMC_TESTDIR)/ESMF_CompCreate: ESMF_CompCreate.o
	$(SL_F_LINKER) -o $(ESMC_TESTDIR)/ESMF_CompCreate ESMF_CompCreate.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	${RM} ESMF_CompCreate.o

RUN_ESMF_CompCreate:
	-@rm -f testlog stdlog.* logfile.* ESMF_CompCreate.tmps
	${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_CompCreate > $(ESMC_TESTDIR)/ESMF_CompCreate.stdout

RUN_ESMF_CompCreateUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_CompCreate.tmps
	${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_CompCreate > $(ESMC_TESTDIR)/ESMF_CompCreate.stdout

ESMF_CompCreate.rm:
	echo "ESMF_CompCreate.rm target called here."

#
#
#
ESMF_UserRun.o: ESMF_UserGComp.o ESMF_UserCComp.o

$(ESMC_TESTDIR)/ESMF_CompRun: ESMF_UserGComp.o ESMF_UserCComp.o ESMF_CompRun.o
	-$(SL_F_LINKER) -o $(ESMC_TESTDIR)/ESMF_CompRun ESMF_CompRun.o \
         ESMF_UserGComp.o ESMF_UserCComp.o -lesmf ${F90CXXLIBS} ${MPI_LIB} \
         ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	${RM} ESMF_CompRun.o ESMF_UserComp.o

RUN_ESMF_CompRun:
	-@rm -f testlog stdlog.* logfile.* ESMF_CompRun.tmps
	${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_CompRun > $(ESMC_TESTDIR)/ESMF_CompRun.stdout

RUN_ESMF_CompRunUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_CompRun.tmps
	${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_CompRun > $(ESMC_TESTDIR)/ESMF_CompRun.stdout

ESMF_CompRun.rm:
	echo "ESMF_CompRun.rm target called here."


#       
# State Create
#

$(ESMC_TESTDIR)/ESMF_StateCreate: ESMF_StateCreate.o
	$(SL_F_LINKER) -o $(ESMC_TESTDIR)/ESMF_StateCreate ESMF_StateCreate.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	${RM} ESMF_StateCreate.o

RUN_ESMF_StateCreate:
	-@rm -f testlog stdlog.* logfile.* ESMF_StateCreate.tmps
	${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_StateCreate > $(ESMC_TESTDIR)/ESMF_StateCreate.stdout

RUN_ESMF_StateCreateUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_StateCreate.tmps
	${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_StateCreate > $(ESMC_TESTDIR)/ESMF_StateCreate.stdout

ESMF_StateCreate.rm:
	echo "ESMF_StateCreate.rm target called here."




#       
# State Unit Test
#

$(ESMC_TESTDIR)/ESMF_StateUTest: ESMF_StateUTest.o
	$(SL_F_LINKER) -o $(ESMC_TESTDIR)/ESMF_StateUTest ESMF_StateUTest.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	${RM} ESMF_StateUTest.o

RUN_ESMF_StateUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_StateUTest.tmps
	${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_StateUTest > $(ESMC_TESTDIR)/ESMF_StateUTest.stdout

RUN_ESMF_StateUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_StateUTest.tmps
	${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_StateUTest > $(ESMC_TESTDIR)/ESMF_StateUTest.stdout

ESMF_StateUTest.rm:
	echo "ESMF_StateUTest.rm target called here."




