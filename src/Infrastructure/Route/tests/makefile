# $Id: makefile,v 1.17 2005/02/28 16:34:57 nscollins Exp $

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


TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_RouteUTest \
                $(ESMF_TESTDIR)/ESMF_CommTableUTest 

TESTS_RUN     = RUN_ESMF_RouteUTest \
                RUN_ESMF_CommTableUTest

TESTS_RUN_UNI = RUN_ESMF_RouteUTestUNI \
                RUN_ESMF_CommTableUTestUNI




include ${ESMF_DIR}/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.tmps *.txt $(TESTS_BUILD) NULL core \
              ESMF_LogFile stdoutfiles
CLOBBERDIRS =


#
# F90 Exchange Packet 
#
RUN_ESMF_XPacketUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_XPacketUTest > $(ESMF_TESTDIR)/ESMF_XPacketUTest.stdout

RUN_ESMF_XPacketUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_XPacketUTest > $(ESMF_TESTDIR)/ESMF_XPacketUTest.stdout

#
#  C++ Exchange Packets
#
RUN_ESMC_XPacketUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMC_XPacketUTest > $(ESMF_TESTDIR)/ESMC_XPacketUTest.stdout

RUN_ESMC_XPacketUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMC_XPacketUTest > $(ESMF_TESTDIR)/ESMC_XPacketUTest.stdout



#
#  F90 Route
#
RUN_ESMF_RouteUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_RouteUTest > $(ESMF_TESTDIR)/ESMF_RouteUTest.stdout

RUN_ESMF_RouteUTestUNI:
	echo "PASS: ESMF_RouteUTest not set up to run on one processor." > /Users/nancy/esmf/esmf_head/examples/examplesg/Darwin.absoft.32.default/ESMF_RouteUTest.stdout

#
# C++ Route
#
RUN_ESMC_RouteUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMC_RouteUTest > $(ESMF_TESTDIR)/ESMC_RouteUTest.stdout

RUN_ESMC_RouteUTestUNI:
	echo "PASS: ESMC_RouteUTest not set up to run on one processor." > /Users/nancy/esmf/esmf_head/examples/examplesg/Darwin.absoft.32.default/ESMC_RouteUTest.stdout



#
#  F90 CommTable
#
RUN_ESMF_CommTableUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_CommTableUTest > $(ESMF_TESTDIR)/ESMF_CommTableUTest.stdout

RUN_ESMF_CommTableUTestUNI:
	echo "PASS: ESMF_CommTableUTest not set up to run on one processor." > /Users/nancy/esmf/esmf_head/examples/examplesg/Darwin.absoft.32.default/ESMF_CommTableUTest.stdout


#
# C++ CommTable
#
RUN_ESMC_CommTableUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMC_CommTableUTest > $(ESMF_TESTDIR)/ESMC_CommTableUTest.stdout

RUN_ESMC_CommTableUTestUNI:
	echo "PASS: ESMC_CommTableUTest not set up to run on one processor." > /Users/nancy/esmf/esmf_head/examples/examplesg/Darwin.absoft.32.default/ESMC_CommTableUTest.stdout



#
#  F90 RTable
#
RUN_ESMF_RTableUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_RTableUTest > $(ESMF_TESTDIR)/ESMF_RTableUTest.stdout

RUN_ESMF_RTableUTestUNI:
	echo "PASS: ESMF_RTableUTest not set up to run on one processor." > /Users/nancy/esmf/esmf_head/examples/examplesg/Darwin.absoft.32.default/ESMF_RTableUTest.stdout


#
# C++ RTable
#
RUN_ESMC_RTableUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMC_RTableUTest > $(ESMF_TESTDIR)/ESMC_RTableUTest.stdout

RUN_ESMC_RTableUTestUNI:
	echo "PASS: ESMC_RTableUTest not set up to run on one processor." > /Users/nancy/esmf/esmf_head/examples/examplesg/Darwin.absoft.32.default/ESMC_RTableUTest.stdout


