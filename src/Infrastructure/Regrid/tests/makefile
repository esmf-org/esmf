# $Id: makefile,v 1.9 2003/12/03 23:19:23 jwolfe Exp $

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


TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_RegridUTest 

TESTS_RUN     = RUN_ESMF_RegridUTest 

TESTS_RUN_UNI = RUN_ESMF_RegridUTestUNI 

TESTS_RM      = 

include $(ESMF_DIR)/makefile


DIRS =

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt NULL core $(TESTS_BUILD)
CLOBBERDIRS =

#
#
#
$(ESMF_TESTDIR)/ESMF_RegridUTest: chkopts ESMF_RegridUTest.o
	-$(SL_C_LINKER) -o $(ESMF_TESTDIR)/ESMF_RegridUTest ESMF_RegridUTest.o -lesmf ${CXXF90LIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} $(SL_LINKOPTS)
	${RM} ESMF_RegridTest.o

RUN_ESMF_RegridUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_RegridUTest > $(ESMF_TESTDIR)/ESMF_RegridUTest.stdout

RUN_ESMF_RegridUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_RegridUTest > $(ESMF_TESTDIR)/ESMF_RegridUTest.stdout

ESMF_RegridUTest.rm:
	echo "ESMF_RegridTest.rm target called here."
