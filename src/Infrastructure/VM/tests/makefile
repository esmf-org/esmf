# $Id: makefile,v 1.33.2.1 2008/09/09 17:08:44 theurich Exp $ 

ALL:      build_unit_tests
run:      run_unit_tests
run_uni:  run_unit_tests_uni

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

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_VMUTest \
		$(ESMF_TESTDIR)/ESMF_VMUserMpiInitUTest \
		$(ESMF_TESTDIR)/ESMF_VMSendVMRecvUTest \
		$(ESMF_TESTDIR)/ESMF_VMSendNbVMRecvNbUTest \
		$(ESMF_TESTDIR)/ESMF_VMSendRecvUTest \
		$(ESMF_TESTDIR)/ESMF_VMSendRecvNbUTest \
		$(ESMF_TESTDIR)/ESMF_VMScatterUTest \
		$(ESMF_TESTDIR)/ESMF_VMGatherUTest \
		$(ESMF_TESTDIR)/ESMF_VMAllGatherUTest \
		$(ESMF_TESTDIR)/ESMF_VMAllGatherVUTest \
		$(ESMF_TESTDIR)/ESMF_VMBroadcastUTest \
		$(ESMF_TESTDIR)/ESMF_VMAllToAllVUTest \
		$(ESMF_TESTDIR)/ESMF_VMBarrierUTest \
		$(ESMF_TESTDIR)/ESMF_VMComponentUTest

TESTS_RUN     = RUN_ESMF_VMUTest \
                RUN_ESMF_VMUserMpiInitUTest \
                RUN_ESMF_VMSendVMRecvUTest \
                RUN_ESMF_VMSendNbVMRecvNbUTest \
                RUN_ESMF_VMSendRecvUTest \
                RUN_ESMF_VMSendRecvNbUTest \
                RUN_ESMF_VMScatterUTest \
                RUN_ESMF_VMGatherUTest \
                RUN_ESMF_VMAllGatherUTest \
                RUN_ESMF_VMAllGatherVUTest \
		RUN_ESMF_VMBroadcastUTest \
                RUN_ESMF_VMAllToAllVUTest \
		RUN_ESMF_VMBarrierUTest \
		RUN_ESMF_VMComponentUTest 

TESTS_RUN_UNI = RUN_ESMF_VMUTestUNI \
                RUN_ESMF_VMUserMpiInitUTestUNI \
                RUN_ESMF_VMSendNbVMRecvNbUTestUNI \
                RUN_ESMF_VMSendRecvUTestUNI \
                RUN_ESMF_VMSendRecvNbUTestUNI \
                RUN_ESMF_VMScatterUTestUNI \
                RUN_ESMF_VMGatherUTestUNI \
                RUN_ESMF_VMAllGatherUTestUNI \
                RUN_ESMF_VMAllGatherVUTestUNI \
                RUN_ESMF_VMBroadcastUTestUNI \
                RUN_ESMF_VMAllToAllVUTestUNI \
                RUN_ESMF_VMBarrierUTestUNI \
                RUN_ESMF_VMComponentUTestUNI


include ${ESMF_DIR}/makefile

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
# VM
#
RUN_ESMF_VMUTest:
	$(MAKE) TNAME=VM NP=4 ftest

RUN_ESMF_VMUTestUNI:
	$(MAKE) TNAME=VM NP=1 ftest

#
# VM user mpi init
#
RUN_ESMF_VMUserMpiInitUTest:
	$(MAKE) TNAME=VMUserMpiInit NP=4 ftest

RUN_ESMF_VMUserMpiInitUTestUNI:
	$(MAKE) TNAME=VMUserMpiInit NP=1 ftest

#
# VM Send VM Recv
#
RUN_ESMF_VMSendVMRecvUTest:
	$(MAKE) TNAME=VMSendVMRecv NP=4 ftest

#
# VM SendNb VM RecvNb
#
RUN_ESMF_VMSendNbVMRecvNbUTest:
	$(MAKE) TNAME=VMSendNbVMRecvNb NP=4 ftest

RUN_ESMF_VMSendNbVMRecvNbUTestUNI:
	$(MAKE) TNAME=VMSendNbVMRecvNb NP=1 ftest

#
# VM SendRecv
#
RUN_ESMF_VMSendRecvUTest:
	$(MAKE) TNAME=VMSendRecv NP=4 ftest

RUN_ESMF_VMSendRecvUTestUNI:
	$(MAKE) TNAME=VMSendRecv NP=1 ftest

#
# VM SendRecvNb
#
RUN_ESMF_VMSendRecvNbUTest:
	$(MAKE) TNAME=VMSendRecvNb NP=4 ftest

RUN_ESMF_VMSendRecvNbUTestUNI:
	$(MAKE) TNAME=VMSendRecvNb NP=1 ftest

#
# VM Scatter 
#
RUN_ESMF_VMScatterUTest:
	$(MAKE) TNAME=VMScatter NP=4 ftest

RUN_ESMF_VMScatterUTestUNI:
	$(MAKE) TNAME=VMScatter NP=1 ftest

#
# VM Gather
#
RUN_ESMF_VMGatherUTest:
	$(MAKE) TNAME=VMGather NP=4 ftest

RUN_ESMF_VMGatherUTestUNI:
	$(MAKE) TNAME=VMGather NP=1 ftest


#
# VM AllGather
#
RUN_ESMF_VMAllGatherUTest:
	$(MAKE) TNAME=VMAllGather NP=4 ftest

RUN_ESMF_VMAllGatherUTestUNI:
	$(MAKE) TNAME=VMAllGather NP=1 ftest


#
# VM AllGatherV
#
RUN_ESMF_VMAllGatherVUTest:
	$(MAKE) TNAME=VMAllGatherV NP=4 ftest

RUN_ESMF_VMAllGatherVUTestUNI:
	$(MAKE) TNAME=VMAllGatherV NP=1 ftest

#
#
# VM Broadcast
#
RUN_ESMF_VMBroadcastUTest:
	$(MAKE) TNAME=VMBroadcast NP=4 ftest

RUN_ESMF_VMBroadcastUTestUNI:
	$(MAKE) TNAME=VMBroadcast NP=1 ftest

#
# VM AllToAllV
#
RUN_ESMF_VMAllToAllVUTest:
	$(MAKE) TNAME=VMAllToAllV NP=4 ftest

RUN_ESMF_VMAllToAllVUTestUNI:
	$(MAKE) TNAME=VMAllToAllV NP=1 ftest

#
# VM Barrier
#
RUN_ESMF_VMBarrierUTest:
	$(MAKE) TNAME=VMBarrier NP=4 ftest

RUN_ESMF_VMBarrierUTestUNI:
	$(MAKE) TNAME=VMBarrier NP=1 ftest

#
# VM Component
#
RUN_ESMF_VMComponentUTest:
	env MPI_COMM_MAX=5000 MPI_GROUP_MAX=5000 $(MAKE) TNAME=VMComponent NP=4 ftest

RUN_ESMF_VMComponentUTestUNI:
	env MPI_COMM_MAX=5000 MPI_GROUP_MAX=5000 $(MAKE) TNAME=VMComponent NP=1 ftest


