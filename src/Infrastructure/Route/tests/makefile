# $Id: makefile,v 1.27 2005/11/22 00:27:38 nscollins Exp $

ALL:  build_unit_tests
run:  run_unit_tests

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
                $(ESMF_TESTDIR)/ESMF_CommTableUTest \
                $(ESMF_TESTDIR)/ESMF_HaloUTest \
                $(ESMF_TESTDIR)/ESMF_RedistUTest

TESTS_RUN     = RUN_ESMF_RouteUTest \
                RUN_ESMF_CommTableUTest \
                RUN_ESMF_RedistUTest \
                RUN_ESMF_HaloUTest 

TESTS_RUN_UNI = RUN_ESMF_RouteUTestUNI \
                RUN_ESMF_CommTableUTestUNI \
                RUN_ESMF_RedistUTestUNI \
                RUN_ESMF_HaloUTestUNI




include ${ESMF_DIR}/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
# F90 Exchange Packet 
#
RUN_ESMF_XPacketUTest:
	$(MAKE) TNAME=XPacket NP=4 ftest

RUN_ESMF_XPacketUTestUNI:
	$(MAKE) TNAME=XPacket NP=1 ftest

#
#  C++ Exchange Packets
#
RUN_ESMC_XPacketUTest:
	$(MAKE) TNAME=XPacket NP=4 ctest

RUN_ESMC_XPacketUTestUNI:
	$(MAKE) TNAME=XPacket NP=1 ctest



#
#  F90 Route
#
RUN_ESMF_RouteUTest:
	$(MAKE) TNAME=Route NP=4 ftest

RUN_ESMF_RouteUTestUNI:
	echo "PASS: ESMF_RouteUTest not set up to run on one processor." > $(ESMF_TESTDIR)/ESMF_RouteUTest.stdout


#
# C++ Route
#
RUN_ESMC_RouteUTest:
	$(MAKE) TNAME=Route NP=4 ctest

RUN_ESMC_RouteUTestUNI:
	echo "PASS: ESMC_RouteUTest not set up to run on one processor." > $(ESMF_TESTDIR)/ESMC_RouteUTest.stdout



#
#  F90 CommTable
#
RUN_ESMF_CommTableUTest:
	$(MAKE) TNAME=CommTable NP=4 ftest

RUN_ESMF_CommTableUTestUNI:
	echo "PASS: ESMF_CommTableUTest not set up to run on one processor." > $(ESMF_TESTDIR)/ESMF_CommTableUTest.stdout


#
# C++ CommTable
#
RUN_ESMC_CommTableUTest:
	$(MAKE) TNAME=CommTable NP=4 ctest

RUN_ESMC_CommTableUTestUNI:
	echo "PASS: ESMC_CommTableUTest not set up to run on one processor." > $(ESMF_TESTDIR)/ESMC_CommTableUTest.stdout



#
#  F90 RTable
#
RUN_ESMF_RTableUTest:
	$(MAKE) TNAME=RTable NP=4 ftest

RUN_ESMF_RTableUTestUNI:
	echo "PASS: ESMF_RTableUTest not set up to run on one processor." > $(ESMF_TESTDIR)/ESMF_RTableUTest.stdout


#
# C++ RTable
#
RUN_ESMC_RTableUTest:
	$(MAKE) TNAME=RTable NP=4 ctest

RUN_ESMC_RTableUTestUNI:
	echo "PASS: ESMC_RTableUTest not set up to run on one processor." > $(ESMF_TESTDIR)/ESMC_RTableUTest.stdout



#
#  F90 Redist
#
UTEST_Redist_OBJS =  ESMF_RedistHelpers.o

ESMF_RedistUTest.o: ESMF_RedistHelpers.o

RUN_ESMF_RedistUTest:
	$(MAKE) TNAME=Redist NP=4 ftest

RUN_ESMF_RedistUTestUNI:
	$(MAKE) TNAME=Redist NP=1 ftest


#
#  F90 Halo
#
UTEST_Halo_OBJS =  ESMF_HaloHelpers.o

ESMF_HaloUTest.o: ESMF_HaloHelpers.o

RUN_ESMF_HaloUTest:
	$(MAKE) TNAME=Halo NP=4 ftest

RUN_ESMF_HaloUTestUNI:
	$(MAKE) TNAME=Halo NP=1 ftest


