# $Id: makefile,v 1.1 2003/03/10 21:54:22 cdeluca Exp $ 

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
LOCDIR	  = src/Infrastructure/Fields/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_FieldUTest \
		$(ESMC_TESTDIR)/ESMF_FieldsTest \
		$(ESMC_TESTDIR)/ESMF_BundleUTest

TESTS_RUN     = RUN_ESMF_FieldUTest \
		RUN_ESMF_FieldsTest \
		RUN_ESMF_BundleUTest 

TESTS_RUN_UNI = RUN_ESMF_FieldUTestUNI \
		RUN_ESMF_FieldsTestUNI \
		RUN_ESMF_BundleUTestUNI 

TESTS_RM      = ESMF_FieldUTest.rm \
		ESMF_FieldsTest.rm \
		ESMF_BundleUTest.rm 

include ${ESMF_DIR}/makefile

#
#  Fields
#
$(ESMC_TESTDIR)/ESMF_FieldUTest: ESMF_FieldUTest.o
	-${F90CXXLD} -o $(ESMC_TESTDIR)/ESMF_FieldUTest ESMF_FieldUTest.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMF_FieldUTest.o

$(ESMC_TESTDIR)/ESMF_FieldsTest: ESMF_FieldsTest.o
	-${F90CXXLD} -o $(ESMC_TESTDIR)/ESMF_FieldsTest ESMF_FieldsTest.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMF_FieldsTest.o

RUN_ESMF_FieldUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldUTest.tmps
	${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_FieldUTest > $(ESMC_TESTDIR)/ESMF_FieldUTest.stdout

RUN_ESMF_FieldsTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldsTest.tmps
	${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_FieldsTest > $(ESMC_TESTDIR)/ESMF_FieldsTest.stdout

RUN_ESMF_FieldUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldUTest.tmps
	${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_FieldUTest > $(ESMC_TESTDIR)/ESMF_FieldUTest.stdout

RUN_ESMF_FieldsTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldsTest.tmps
	${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_FieldsTest > $(ESMC_TESTDIR)/ESMF_FieldsTest.stdout

ESMF_FieldUTest.rm:
	echo "ESMF_FieldUTest.rm target called here."

ESMF_FieldsTest.rm:
	echo "ESMF_FieldsTest.rm target called here."

#
#  Bundles
#
$(ESMC_TESTDIR)/ESMF_BundleUTest: ESMF_BundleUTest.o
	-${F90CXXLD} -o $(ESMC_TESTDIR)/ESMF_BundleUTest ESMF_BundleUTest.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
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
# Fields
#
$(ESMC_TESTDIR)/ESMF_FieldsTest: ESMF_FieldsTest.o
	-${F90CXXLD} -o $(ESMC_TESTDIR)/ESMF_FieldsTest ESMF_FieldsTest.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMF_FieldsTest.o

RUN_ESMF_FieldsTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldsTest.tmps
	${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_FieldsTest > $(ESMC_TESTDIR)/ESMF_FieldsTest.stdout

RUN_ESMF_FieldsTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldsTest.tmps
	${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_FieldsTest > $(ESMC_TESTDIR)/ESMF_FieldsTest.stdout

ESMF_FieldsTest.rm:
	echo "ESMF_FieldsTest.rm target called here."

