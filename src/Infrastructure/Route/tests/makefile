# $Id: makefile,v 1.7 2003/03/28 17:37:47 svasquez Exp $

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
LOCDIR	  = src/Infrastructure/Route/tests

TESTEXAMPLES_3  = 
TESTEXAMPLES_9  = 

include ${ESMF_DIR}/makefile

DIRS = 

CLEANDIRS   =
CLEANFILES  = *.o *.mod *.tmps
CLOBBERDIRS =


tests test: ESMF_RouteTest ESMC_RouteTest

#tests test: ESMF_XPacketTest ESMC_XPacketTest \
#            ESMF_RTableTest ESMC_RTableTest \
#            ESMF_CommTableTest ESMC_CommTableTest \
#            ESMF_RouteTest ESMC_RouteTest

ESMF_RouteCreateDir:
	mkdir -p ${ESMC_TESTDIR}

ESMF_XPacketTest: chkopts ESMF_XPacketTest.o
	${F90CXXLD} -o ESMF_XPacketTest ESMF_XPacketTest.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMF_XPacketTest.o

RUN_ESMF_XPacketTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_XPacketTest.tmps
	${MPIRUN} -np 4 ESMF_XPacketTest > ESMF_XPacketTest.stdout

RUN_ESMF_XPacketTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_XPacketTest.tmps
	${MPIRUN} -np 1 ESMF_XPacketTest > ESMF_XPacketTest.stdout

ESMF_XPacketTest.rm:
	echo "ESMF_XPacketTest.rm target called here."

ESMC_XPacketTest: chkopts ESMC_XPacketTest.o
	${CXXF90LD} -o ESMC_XPacketTest ESMC_XPacketTest.o -lesmf ${CXXF90LIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMC_XPacketTest.o

RUN_ESMC_XPacketTest:
	-@rm -f testlog stdlog.* logfile.* ESMC_XPacketTest.tmps
	${MPIRUN} -np 4 ESMC_XPacketTest > ESMC_XPacketTest.stdout

RUN_ESMC_XPacketTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMC_XPacketTest.tmps
	${MPIRUN} -np 1 ESMC_XPacketTest > ESMC_XPacketTest.stdout

ESMC_XPacketTest.rm:
	echo "ESMC_XPacketTest.rm target called here."

ESMF_RouteTest: chkopts ESMF_RouteTest.o
	${F90CXXLD} -o ESMF_RouteTest ESMF_RouteTest.o -lesmf ${F90CXXLIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMF_RouteTest.o

RUN_ESMF_RouteTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_RouteTest.tmps
	${MPIRUN} -np 4 ESMF_RouteTest > ESMF_RouteTest.stdout

RUN_ESMF_RouteTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_RouteTest.tmps
	${MPIRUN} -np 1 ESMF_RouteTest > ESMF_RouteTest.stdout

ESMF_RouteTest.rm:
	echo "ESMF_RouteTest.rm target called here."

ESMC_RouteTest: chkopts ESMC_RouteTest.o
	${CXXF90LD} -o ESMC_RouteTest ESMC_RouteTest.o -lesmf ${CXXF90LIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB}
	${RM} ESMC_RouteTest.o

RUN_ESMC_RouteTest:
	-@rm -f testlog stdlog.* logfile.* ESMC_RouteTest.tmps
	${MPIRUN} -np 4 ESMC_RouteTest > ESMC_RouteTest.stdout

RUN_ESMC_RouteTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMC_RouteTest.tmps
	${MPIRUN} -np 1 ESMC_RouteTest > ESMC_RouteTest.stdout

ESMC_RouteTest.rm:
	echo "ESMC_RouteTest.rm target called here."
