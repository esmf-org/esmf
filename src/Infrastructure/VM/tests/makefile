# $Id: makefile,v 1.12 2005/02/28 16:34:58 nscollins Exp $ 

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
DIRS	  = 
LOCDIR	  = src/Infrastructure/VM/tests

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_VMUTest \
		$(ESMC_TESTDIR)/ESMF_VMSendVMRecvUTest \
		$(ESMC_TESTDIR)/ESMF_VMSendRecvUTest \
		$(ESMC_TESTDIR)/ESMF_VMScatterUTest \
		$(ESMC_TESTDIR)/ESMF_VMGatherUTest \
		$(ESMC_TESTDIR)/ESMF_VMBroadcastUTest \
		$(ESMC_TESTDIR)/ESMF_VMBarrierUTest

TESTS_RUN     = RUN_ESMF_VMUTest \
                RUN_ESMF_VMSendVMRecvUTest \
                RUN_ESMF_VMSendRecvUTest \
                RUN_ESMF_VMScatterUTest \
                RUN_ESMF_VMGatherUTest \
		RUN_ESMF_VMBroadcastUTest \
		RUN_ESMF_VMBarrierUTest

TESTS_RUN_UNI = RUN_ESMF_VMUTestUNI \
                RUN_ESMF_VMSendVMRecvUTestUNI \
                RUN_ESMF_VMSendRecvUTestUNI \
                RUN_ESMF_VMScatterUTestUNI \
                RUN_ESMF_VMGatherUTestUNI \
                RUN_ESMF_VMBroadcastUTestUNI \
                RUN_ESMF_VMBarrierUTestUNI


include ${ESMF_DIR}/makefile

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod NULL *.txt stdoutfiles core ESMF_LogFile $(TESTS_BUILD)
CLOBBERDIRS =


#
# VM
#
RUN_ESMF_VMUTest:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_VMUTest > $(ESMC_TESTDIR)/ESMF_VMUTest.stdout

RUN_ESMF_VMUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_VMUTest > $(ESMC_TESTDIR)/ESMF_VMUTest.stdout

#
# VM Send Receive
#
RUN_ESMF_VMSendVMRecvUTest:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_VMSendVMRecvUTest > $(ESMC_TESTDIR)/ESMF_VMSendVMRecvUTest.stdout

RUN_ESMF_VMSendVMRecvUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_VMSendVMRecvUTest > $(ESMC_TESTDIR)/ESMF_VMSendVMRecvUTest.stdout

#
# VM Sendreceive
#
RUN_ESMF_VMSendRecvUTest:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_VMSendRecvUTest > $(ESMC_TESTDIR)/ESMF_VMSendRecvUTest.stdout

RUN_ESMF_VMSendRecvUTestUNI:
	echo "PASS: ESMF_VMSendRecvUTest not set up to run on one processor." > /Users/nancy/esmf/esmf_head/examples/examplesg/Darwin.absoft.32.default/ESMF_VMSendRecvUTest.stdout

#
# VM Scatter 
#
RUN_ESMF_VMScatterUTest:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_VMScatterUTest > $(ESMC_TESTDIR)/ESMF_VMScatterUTest.stdout

RUN_ESMF_VMScatterUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_VMScatterUTest > $(ESMC_TESTDIR)/ESMF_VMScatterUTest.stdout

#
# VM Gather
#
RUN_ESMF_VMGatherUTest:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_VMGatherUTest > $(ESMC_TESTDIR)/ESMF_VMGatherUTest.stdout

RUN_ESMF_VMGatherUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_VMGatherUTest > $(ESMC_TESTDIR)/ESMF_VMGatherUTest.stdout

#
# VM Broadcast
#
RUN_ESMF_VMBroadcastUTest:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_VMBroadcastUTest > $(ESMC_TESTDIR)/ESMF_VMBroadcastUTest.stdout

RUN_ESMF_VMBroadcastUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_VMBroadcastUTest > $(ESMC_TESTDIR)/ESMF_VMBroadcastUTest.stdout

#
# VM Barrier
#
RUN_ESMF_VMBarrierUTest:
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_VMBarrierUTest > $(ESMC_TESTDIR)/ESMF_VMBarrierUTest.stdout

RUN_ESMF_VMBarrierUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_VMBarrierUTest > $(ESMC_TESTDIR)/ESMF_VMBarrierUTest.stdout

