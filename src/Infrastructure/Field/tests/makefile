\# $Id: makefile,v 1.14 2003/04/10 22:50:58 svasquez Exp $ 

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
DIRS	  = 
LOCDIR	  = src/Infrastructure/Field/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_FieldUTest \
		$(ESMC_TESTDIR)/ESMF_FRouteUTest \
		$(ESMC_TESTDIR)/ESMF_BundleUTest

TESTS_RUN     = RUN_ESMF_FieldUTest \
		RUN_ESMF_FRouteUTest \
		RUN_ESMF_BundleUTest 

TESTS_RUN_UNI = RUN_ESMF_FieldUTestUNI \
		RUN_ESMF_FRouteUTestUNI \
		RUN_ESMF_BundleUTestUNI 

TESTS_RM      = ESMF_FieldUTest.rm \
		ESMF_FRouteUTest.rm \
		ESMF_BundleUTest.rm 

include ${ESMF_DIR}/makefile

CLEANDIRS   =
CLEANFILES  = *.o *.mod 
CLOBBERDIRS =


#
#  Bundles
#
$(ESMC_TESTDIR)/ESMF_BundleUTest: ESMF_BundleUTest.o
	-$(SL_F_LINKER) -o $(ESMC_TESTDIR)/ESMF_BundleUTest ESMF_BundleUTest.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	${RM} ESMF_BundleUTest.o

RUN_ESMF_BundleUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_BundleUTest.tmps
	${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_BundleUTest > $(ESMC_TESTDIR)/ESMF_BundleUTest.stdout

RUN_ESMF_BundleUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_BundleUTest.tmps
	${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_BundleUTest > $(ESMC_TESTDIR)/ESMF_BundleUTest.stdout

ESMF_BundleUTest.rm:
	echo "ESMF_BundleUTest.rm target called here."

#
# Field
#
$(ESMC_TESTDIR)/ESMF_FieldUTest: ESMF_FieldUTest.o
	-$(SL_F_LINKER) -o $(ESMC_TESTDIR)/ESMF_FieldUTest ESMF_FieldUTest.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	${RM} ESMF_FieldUTest.o

RUN_ESMF_FieldUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldUTest.tmps
	${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_FieldUTest > $(ESMC_TESTDIR)/ESMF_FieldUTest.stdout

RUN_ESMF_FieldUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldUTest.tmps
	${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_FieldUTest > $(ESMC_TESTDIR)/ESMF_FieldUTest.stdout

ESMF_FieldUTest.rm:
	echo "ESMF_FieldUTest.rm target called here."




#
# Field Route function
#
$(ESMC_TESTDIR)/ESMF_FRouteUTest: ESMF_FRouteUTest.o
	-$(SL_F_LINKER) -o $(ESMC_TESTDIR)/ESMF_FRouteUTest ESMF_FRouteUTest.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	${RM} ESMF_FRouteUTest.o

RUN_ESMF_FRouteUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_FRouteUTest.tmps
	${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_FRouteUTest > $(ESMC_TESTDIR)/ESMF_FRouteUTest.stdout

RUN_ESMF_FRouteUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_FRouteUTest.tmps
	${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_FRouteUTest > $(ESMC_TESTDIR)/ESMF_FRouteUTest.stdout

ESMF_FRouteUTest.rm:
	echo "ESMF_FRouteUTest.rm target called here."



