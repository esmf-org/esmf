# $Id: makefile,v 1.6 2003/04/09 23:48:44 flanigan Exp $

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
LOCDIR	  = src/Infrastructure/Regrid/tests


TESTS_BUILD   = $(ESMC_TESTDIR)/ESMC_RegridUTest \
		$(ESMC_TESTDIR)/ESMF_RegridUTest 

TESTS_RUN     = RUN_ESMC_RegridUTest \
		RUN_ESMF_RegridUTest 

TESTS_RUN_UNI = RUN_ESMC_RegridUTestUNI \
		RUN_ESMF_RegridUTestUNI 

TESTS_RM      = 

include $(ESMF_DIR)/makefile


DIRS =

CLEANDIRS   =
CLEANFILES  = *.o *.mod
CLOBBERDIRS =

#
#
#
$(ESMF_TESTDIR)/ESMC_RegridUTest: chkopts ESMC_RegridUTest.o
	-$(SL_C_LINKER) -o $(ESMF_TESTDIR)/ESMC_RegridUTest ESMC_RegridUTest.o -lesmf ${CXXF90LIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	${RM} ESMC_RegridTest.o

RUN_ESMC_RegridUTest:
	${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMC_RegridUTest > $(ESMF_TESTDIR)/ESMC_RegridUTest.stdout

RUN_ESMC_RegridUTestUNI:
	${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMC_RegridUTest > $(ESMF_TESTDIR)/ESMC_RegridUTest.stdout

ESMC_RegridUTest.rm:
	echo "ESMC_RegridTest.rm target called here."


#
#
#
$(ESMF_TESTDIR)/ESMF_RegridUTest: chkopts ESMF_RegridUTest.o
	-$(SL_C_LINKER) -o $(ESMF_TESTDIR)/ESMF_RegridUTest ESMF_RegridUTest.o -lesmf ${CXXF90LIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	${RM} ESMF_RegridTest.o

RUN_ESMF_RegridUTest:
	${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_RegridUTest > $(ESMF_TESTDIR)/ESMF_RegridUTest.stdout

RUN_ESMF_RegridUTestUNI:
	${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_RegridUTest > $(ESMF_TESTDIR)/ESMF_RegridUTest.stdout

ESMF_RegridUTest.rm:
	echo "ESMF_RegridTest.rm target called here."
