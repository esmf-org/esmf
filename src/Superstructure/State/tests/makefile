# $Id: makefile,v 1.3 2004/05/14 22:34:28 slswift Exp $ 

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
LOCDIR	  = src/Superstructure/State/tests

TESTS_BUILD   =	$(ESMC_TESTDIR)/ESMF_StateCreate \
		$(ESMC_TESTDIR)/ESMF_StateUTest

TESTS_RUN     =	RUN_ESMF_StateCreate \
		RUN_ESMF_StateUTest

TESTS_RUN_UNI =	RUN_ESMF_StateCreateUNI \
		RUN_ESMF_StateUTestUNI

TESTS_RM      =	ESMF_StateCreate.rm \
		ESMF_StateUTest.rm

include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod 
CLOBBERDIRS =

#       
# State Create
#

$(ESMC_TESTDIR)/ESMF_StateCreate: ESMF_StateCreate.o
	-$(SL_F_LINKER) -o $(ESMC_TESTDIR)/ESMF_StateCreate ESMF_StateCreate.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} ${NETCDF_LIB} $(SL_LINKOPTS)
	${RM} -f ESMF_StateCreate.o

RUN_ESMF_StateCreate:
	-@rm -f testlog stdlog.* logfile.* ESMF_StateCreate.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_StateCreate > $(ESMC_TESTDIR)/ESMF_StateCreate.stdout

RUN_ESMF_StateCreateUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_StateCreate.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_StateCreate > $(ESMC_TESTDIR)/ESMF_StateCreate.stdout

ESMF_StateCreate.rm:
	echo "ESMF_StateCreate.rm target called here."


#       
# State Unit Test
#

$(ESMC_TESTDIR)/ESMF_StateUTest: ESMF_StateUTest.o
	-$(SL_F_LINKER) -o $(ESMC_TESTDIR)/ESMF_StateUTest ESMF_StateUTest.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} ${NETCDF_LIB} $(SL_LINKOPTS)
	${RM} -f ESMF_StateUTest.o

RUN_ESMF_StateUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_StateUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_StateUTest > $(ESMC_TESTDIR)/ESMF_StateUTest.stdout

RUN_ESMF_StateUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_StateUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_StateUTest > $(ESMC_TESTDIR)/ESMF_StateUTest.stdout

ESMF_StateUTest.rm:
	echo "ESMF_StateUTest.rm target called here."




