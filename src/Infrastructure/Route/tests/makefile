# $Id: makefile,v 1.15 2004/08/03 19:33:50 nscollins Exp $

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
LOCDIR	  = src/Infrastructure/Route/tests


TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_RouteUTest 

TESTS_RUN     = RUN_ESMF_RouteUTest

TESTS_RUN_UNI = 

TESTS_RM      = 


include ${ESMF_DIR}/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.tmps *.txt $(TESTS_BUILD) NULL core \
              ESMF_LogFile stdoutfiles
CLOBBERDIRS =


#
# F Packet 
#
RUN_ESMF_XPacketUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_XPacketUTest > $(ESMF_TESTDIR)/ESMF_XPacketUTest.stdout

RUN_ESMF_XPacketUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_XPacketUTest > $(ESMF_TESTDIR)/ESMF_XPacketUTest.stdout

ESMF_XPacketUTest.rm:
	echo "ESMF_XPacketUTest.rm target called here."

#
#  C++ Packet
#
$(ESMF_TESTDIR)/ESMC_XPacketUTest: chkopts ESMC_XPacketUTest.o
	-$(SL_C_LINKER) -o $(ESMF_TESTDIR)/ESMC_XPacketUTest ESMC_XPacketUTest.o -lesmf ${CXXF90LIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} ${NETCDF_LIB} $(SL_LINKOPTS)
	${RM} ESMC_XPacketUTest.o

RUN_ESMC_XPacketUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMC_XPacketUTest > $(ESMF_TESTDIR)/ESMC_XPacketUTest.stdout

RUN_ESMC_XPacketUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMC_XPacketUTest > $(ESMF_TESTDIR)/ESMC_XPacketUTest.stdout

ESMC_XPacketUTest.rm:
	echo "ESMC_XPacketUTest.rm target called here."


#
#  F Route
#
RUN_ESMF_RouteUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_RouteUTest > $(ESMF_TESTDIR)/ESMF_RouteUTest.stdout

RUN_ESMF_RouteUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_RouteUTest > $(ESMF_TESTDIR)/ESMF_RouteUTest.stdout

ESMF_RouteUTest.rm:
	echo "ESMF_RouteUTest.rm target called here."


#
# C++ Route
#
$(ESMF_TESTDIR)/ESMC_RouteUTest: chkopts ESMC_RouteUTest.o
	-$(SL_C_LINKER) -o $(ESMF_TESTDIR)/ESMC_RouteUTest ESMC_RouteUTest.o -lesmf ${CXXF90LIBS} ${MPI_LIB} ${MP_LIB} ${THREAD_LIB} ${PCL_LIB} ${NETCDF_LIB} $(SL_LINKOPTS)
	${RM} ESMC_RouteUTest.o

RUN_ESMC_RouteUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMC_RouteUTest > $(ESMF_TESTDIR)/ESMC_RouteUTest.stdout

RUN_ESMC_RouteUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMC_RouteUTest > $(ESMF_TESTDIR)/ESMC_RouteUTest.stdout

ESMC_RouteUTest.rm:
	echo "ESMC_RouteUTest.rm target called here."

