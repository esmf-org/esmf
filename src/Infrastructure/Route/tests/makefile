# $Id: makefile,v 1.1 2003/03/05 17:04:54 nscollins Exp $

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
LOCDIR	  = src/Infrastructure/Route/tests

TESTEXAMPLES_3  = \
	ESMF_XPacketTest RUN_ESMF_XPacketTest ESMF_XPacketTest.rm \
	ESMC_RouteTest RUN_ESMC_RouteTest ESMC_RouteTest.rm

TESTEXAMPLES_9  = \
	ESMF_XPacketTest RUN_ESMF_XPacketTestUNI ESMF_XPacketTest.rm \
	ESMC_RouteTest RUN_ESMC_RouteTestUNI ESMC_RouteTest.rm

EXAMPLESF       = ESMF_XPacketTest.F90 ESMF_Route.F90
EXAMPLESC       = ESMC_XPacketTest.C ESMC_Route.F90

include ${ESMF_DIR}/makefile

tests test: ESMF_XPacketTest ESMC_XPacketTest ESMF_RouteTest ESMC_RouteTest

ESMF_RouteCreateDir:
	mkdir -p ${ESMC_TESTDIR}

ESMF_XPacketTest: chkopts ESMF_XPacketTest.o
	-${F90CXXLD} -o ESMF_XPacketTest ESMF_XPacketTest.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMF_XPacketTest.o

RUN_ESMF_XPacketTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_XPacketTest.tmps
	${MPIRUN} -np 4 ESMF_XPacketTest > ESMF_XPacketTest.stdout

RUN_ESMF_XPacketTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_XPacketTest.tmps
	${MPIRUN} -np 1 ESMF_XPacketTest > ESMF_XPacketTest.stdout

ESMF_XPacketTest.rm:
	echo "ESMF_XPacketTest.rm target called here."

ESMC_RouteTest: chkopts ESMC_RouteTest.o
	-${CXXF90LD} -o ESMC_RouteTest ESMC_RouteTest.o -lesmf ${CXXF90LIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMC_RouteTest.o

RUN_ESMC_RouteTest:
	-@rm -f testlog stdlog.* logfile.* ESMC_RouteTest.tmps
	${MPIRUN} -np 4 ESMC_RouteTest > ESMC_RouteTest.stdout

RUN_ESMC_RouteTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMC_RouteTest.tmps
	${MPIRUN} -np 1 ESMC_RouteTest > ESMC_RouteTest.stdout

ESMC_RouteTest.rm:
	echo "ESMC_RouteTest.rm target called here."
