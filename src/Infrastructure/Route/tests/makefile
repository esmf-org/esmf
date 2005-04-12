# $Id: makefile,v 1.21 2005/04/12 16:52:47 nscollins Exp $

ALL:  build_unit_tests

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


TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_RouteUTest \
                $(ESMF_TESTDIR)/ESMF_CommTableUTest 

TESTS_RUN     = RUN_ESMF_RouteUTest \
                RUN_ESMF_CommTableUTest

TESTS_RUN_UNI = RUN_ESMF_RouteUTestUNI \
                RUN_ESMF_CommTableUTestUNI




include ${ESMF_DIR}/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
# F90 Exchange Packet 
#
RUN_ESMF_XPacketUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_XPacketUTest > $(ESMF_TESTDIR)/ESMF_XPacketUTest.stdout
	-cat *XPacketUTest.Log > $(ESMC_TESTDIR)/ESMF_XPacketUTest.Log
	-rm *XPacketUTest.Log

RUN_ESMF_XPacketUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_XPacketUTest > $(ESMF_TESTDIR)/ESMF_XPacketUTest.stdout
	-cat *XPacketUTest.Log > $(ESMC_TESTDIR)/ESMF_XPacketUTest.Log
	-rm *XPacketUTest.Log

#
#  C++ Exchange Packets
#
RUN_ESMC_XPacketUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMC_XPacketUTest > $(ESMF_TESTDIR)/ESMC_XPacketUTest.stdout
	-cat *XPacketUTest.Log > $(ESMC_TESTDIR)/ESMC_XPacketUTest.Log
	-rm *XPacketUTest.Log

RUN_ESMC_XPacketUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMC_XPacketUTest > $(ESMF_TESTDIR)/ESMC_XPacketUTest.stdout
	-cat *XPacketUTest.Log > $(ESMC_TESTDIR)/ESMC_XPacketUTest.Log
	-rm *XPacketUTest.Log



#
#  F90 Route
#
RUN_ESMF_RouteUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_RouteUTest > $(ESMF_TESTDIR)/ESMF_RouteUTest.stdout
	-cat *RouteUTest.Log > $(ESMC_TESTDIR)/ESMF_RouteUTest.Log
	-rm *RouteUTest.Log

RUN_ESMF_RouteUTestUNI:
	echo "PASS: ESMF_RouteUTest not set up to run on one processor." > $(ESMF_TESTDIR)/ESMF_RouteUTest.stdout

#
# C++ Route
#
RUN_ESMC_RouteUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMC_RouteUTest > $(ESMF_TESTDIR)/ESMC_RouteUTest.stdout
	-cat *RouteUTest.Log > $(ESMC_TESTDIR)/ESMC_RouteUTest.Log
	-rm *RouteUTest.Log

RUN_ESMC_RouteUTestUNI:
	echo "PASS: ESMC_RouteUTest not set up to run on one processor." > $(ESMF_TESTDIR)/ESMC_RouteUTest.stdout



#
#  F90 CommTable
#
RUN_ESMF_CommTableUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_CommTableUTest > $(ESMF_TESTDIR)/ESMF_CommTableUTest.stdout
	-cat *CommTableUTest.Log > $(ESMC_TESTDIR)/ESMF_CommTableUTest.Log
	-rm *CommTableUTest.Log

RUN_ESMF_CommTableUTestUNI:
	echo "PASS: ESMF_CommTableUTest not set up to run on one processor." > $(ESMF_TESTDIR)/ESMF_CommTableUTest.stdout


#
# C++ CommTable
#
RUN_ESMC_CommTableUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMC_CommTableUTest > $(ESMF_TESTDIR)/ESMC_CommTableUTest.stdout
	-cat *CommTableUTest.Log > $(ESMC_TESTDIR)/ESMC_CommTableUTest.Log
	-rm *CommTableUTest.Log

RUN_ESMC_CommTableUTestUNI:
	echo "PASS: ESMC_CommTableUTest not set up to run on one processor." > $(ESMF_TESTDIR)/ESMC_CommTableUTest.stdout



#
#  F90 RTable
#
RUN_ESMF_RTableUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_RTableUTest > $(ESMF_TESTDIR)/ESMF_RTableUTest.stdout
	-cat *RTableUTest.Log > $(ESMC_TESTDIR)/ESMF_RTableUTest.Log
	-rm *CommTableUTest.Log

RUN_ESMF_RTableUTestUNI:
	echo "PASS: ESMF_RTableUTest not set up to run on one processor." > $(ESMF_TESTDIR)/ESMF_RTableUTest.stdout


#
# C++ RTable
#
RUN_ESMC_RTableUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMC_RTableUTest > $(ESMF_TESTDIR)/ESMC_RTableUTest.stdout
	-cat *RTableUTest.Log > $(ESMC_TESTDIR)/ESMC_RTableUTest.Log
	-rm *CommTableUTest.Log

RUN_ESMC_RTableUTestUNI:
	echo "PASS: ESMC_RTableUTest not set up to run on one processor." > $(ESMF_TESTDIR)/ESMC_RTableUTest.stdout


