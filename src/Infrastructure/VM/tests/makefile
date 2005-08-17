# $Id: makefile,v 1.20 2005/08/17 22:38:37 theurich Exp $ 

ALL:  build_unit_tests
run:  tree_run_unit_tests

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
		$(ESMF_TESTDIR)/ESMF_VMSendRecvUTest \
		$(ESMF_TESTDIR)/ESMF_VMScatterUTest \
		$(ESMF_TESTDIR)/ESMF_VMGatherUTest \
		$(ESMF_TESTDIR)/ESMF_VMBroadcastUTest \
		$(ESMF_TESTDIR)/ESMF_VMBarrierUTest

TESTS_RUN     = RUN_ESMF_VMUTest \
                RUN_ESMF_VMUserMpiInitUTest \
                RUN_ESMF_VMSendVMRecvUTest \
                RUN_ESMF_VMSendRecvUTest \
                RUN_ESMF_VMScatterUTest \
                RUN_ESMF_VMGatherUTest \
		RUN_ESMF_VMBroadcastUTest \
		RUN_ESMF_VMBarrierUTest

TESTS_RUN_UNI = RUN_ESMF_VMUTestUNI \
                RUN_ESMF_VMUserMpiInitUTestUNI \
                RUN_ESMF_VMSendVMRecvUTestUNI \
                RUN_ESMF_VMSendRecvUTestUNI \
                RUN_ESMF_VMScatterUTestUNI \
                RUN_ESMF_VMGatherUTestUNI \
                RUN_ESMF_VMBroadcastUTestUNI \
                RUN_ESMF_VMBarrierUTestUNI


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

RUN_ESMF_VMUTestUNI:
	$(MAKE) TNAME=VMUserMpiInit NP=1 ftest

#
# VM Send Receive
#
RUN_ESMF_VMSendVMRecvUTest:
	$(MAKE) TNAME=VMSendVMRecv NP=4 ftest

RUN_ESMF_VMSendVMRecvUTestUNI:
	$(MAKE) TNAME=VMSendVMRecv NP=1 ftest

#
# VM Sendreceive
#
RUN_ESMF_VMSendRecvUTest:
	$(MAKE) TNAME=VMSendRecv NP=4 ftest

RUN_ESMF_VMSendRecvUTestUNI:
	$(MAKE) TNAME=VMSendRecv NP=1 ftest

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
# VM Broadcast
#
RUN_ESMF_VMBroadcastUTest:
	$(MAKE) TNAME=VMBroadcast NP=4 ftest

RUN_ESMF_VMBroadcastUTestUNI:
	$(MAKE) TNAME=VMBroadcast NP=1 ftest

#
# VM Barrier
#
RUN_ESMF_VMBarrierUTest:
	$(MAKE) TNAME=VMBarrier NP=4 ftest

RUN_ESMF_VMBarrierUTestUNI:
	$(MAKE) TNAME=VMBarrier NP=1 ftest

