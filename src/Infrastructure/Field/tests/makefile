# $Id: makefile,v 1.2 2003/03/10 22:32:40 cdeluca Exp $ 

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
		$(ESMC_TESTDIR)/ESMF_FieldTest \
		$(ESMC_TESTDIR)/ESMF_BundleUTest

TESTS_RUN     = RUN_ESMF_FieldUTest \
		RUN_ESMF_FieldTest \
		RUN_ESMF_BundleUTest 

TESTS_RUN_UNI = RUN_ESMF_FieldUTestUNI \
		RUN_ESMF_FieldTestUNI \
		RUN_ESMF_BundleUTestUNI 

TESTS_RM      = ESMF_FieldUTest.rm \
		ESMF_FieldTest.rm \
		ESMF_BundleUTest.rm 

include ${ESMF_DIR}/makefile

#
#  Fields
#
$(ESMC_TESTDIR)/ESMF_FieldUTest: ESMF_FieldUTest.o
	-${F90CXXLD} -o $(ESMC_TESTDIR)/ESMF_FieldUTest ESMF_FieldUTest.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMF_FieldUTest.o

$(ESMC_TESTDIR)/ESMF_FieldTest: ESMF_FieldTest.o
	-${F90CXXLD} -o $(ESMC_TESTDIR)/ESMF_FieldTest ESMF_FieldTest.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMF_FieldTest.o

RUN_ESMF_FieldUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldUTest.tmps
	${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_FieldUTest > $(ESMC_TESTDIR)/ESMF_FieldUTest.stdout

RUN_ESMF_FieldTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldTest.tmps
	${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_FieldTest > $(ESMC_TESTDIR)/ESMF_FieldTest.stdout

RUN_ESMF_FieldUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldUTest.tmps
	${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_FieldUTest > $(ESMC_TESTDIR)/ESMF_FieldUTest.stdout

RUN_ESMF_FieldTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldTest.tmps
	${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_FieldTest > $(ESMC_TESTDIR)/ESMF_FieldTest.stdout

ESMF_FieldUTest.rm:
	echo "ESMF_FieldUTest.rm target called here."

ESMF_FieldTest.rm:
	echo "ESMF_FieldTest.rm target called here."

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
# Field
#
$(ESMC_TESTDIR)/ESMF_FieldTest: ESMF_FieldTest.o
	-${F90CXXLD} -o $(ESMC_TESTDIR)/ESMF_FieldTest ESMF_FieldTest.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMF_FieldTest.o

RUN_ESMF_FieldTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldTest.tmps
	${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_FieldTest > $(ESMC_TESTDIR)/ESMF_FieldTest.stdout

RUN_ESMF_FieldTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_FieldTest.tmps
	${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_FieldTest > $(ESMC_TESTDIR)/ESMF_FieldTest.stdout

ESMF_FieldTest.rm:
	echo "ESMF_FieldTest.rm target called here."



