# $Id: makefile,v 1.16 2004/12/15 22:44:55 nscollins Exp $

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


#
#  C++ Packet
#
RUN_ESMC_XPacketUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMC_XPacketUTest > $(ESMF_TESTDIR)/ESMC_XPacketUTest.stdout

RUN_ESMC_XPacketUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMC_XPacketUTest > $(ESMF_TESTDIR)/ESMC_XPacketUTest.stdout



#
#  F Route
#
RUN_ESMF_RouteUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_RouteUTest > $(ESMF_TESTDIR)/ESMF_RouteUTest.stdout

RUN_ESMF_RouteUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_RouteUTest > $(ESMF_TESTDIR)/ESMF_RouteUTest.stdout



#
# C++ Route
#
RUN_ESMC_RouteUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMC_RouteUTest > $(ESMF_TESTDIR)/ESMC_RouteUTest.stdout

RUN_ESMC_RouteUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMC_RouteUTest > $(ESMF_TESTDIR)/ESMC_RouteUTest.stdout


