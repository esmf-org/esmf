# $Id: makefile,v 1.2 2003/03/15 00:13:32 nscollins Exp $

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
LOCDIR	  = src/Infrastructure/Regrid/tests

TESTEXAMPLES_3  =
TESTEXAMPLES_9  = 

#TESTEXAMPLES_3  = \
#	ESMC_RegridTest RUN_ESMC_RegridTest ESMC_RegridTest.rm
#
#TESTEXAMPLES_9  = \
#	ESMC_RegridTest RUN_ESMC_RegridTestUNI ESMC_RegridTest.rm

EXAMPLESF       = ESMF_Regrid.F90
EXAMPLESC       = ESMC_Regrid.F90

include ${ESMF_DIR}/makefile

tests test:
#tests test: ESMF_RegridTest ESMC_RegridTest

ESMF_RegridCreateDir:
	mkdir -p ${ESMC_TESTDIR}

ESMC_RegridTest: chkopts ESMC_RegridTest.o
	-${CXXF90LD} -o ESMC_RegridTest ESMC_RegridTest.o -lesmf ${CXXF90LIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMC_RegridTest.o

RUN_ESMC_RegridTest:
	-@rm -f testlog stdlog.* logfile.* ESMC_RegridTest.tmps
	${MPIRUN} -np 4 ESMC_RegridTest > ESMC_RegridTest.stdout

RUN_ESMC_RegridTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMC_RegridTest.tmps
	${MPIRUN} -np 1 ESMC_RegridTest > ESMC_RegridTest.stdout

ESMC_RegridTest.rm:
	echo "ESMC_RegridTest.rm target called here."
