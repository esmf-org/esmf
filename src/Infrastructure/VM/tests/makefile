# $Id: makefile,v 1.18 2005/06/28 21:02:04 svasquez Exp $ 

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
DIRS	  = 
LOCDIR	  = src/Infrastructure/VM/tests

TESTS_BUILD   = $(ESMF_TESTDIR)/ESMF_VMUTest \
		$(ESMF_TESTDIR)/ESMF_VMSendVMRecvUTest \
		$(ESMF_TESTDIR)/ESMF_VMSendRecvUTest \
		$(ESMF_TESTDIR)/ESMF_VMScatterUTest \
		$(ESMF_TESTDIR)/ESMF_VMGatherUTest \
		$(ESMF_TESTDIR)/ESMF_VMBroadcastUTest \
		$(ESMF_TESTDIR)/ESMF_VMBarrierUTest

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

CLEANDIRS   = 
CLEANFILES  = $(TESTS_BUILD)
CLOBBERDIRS =


#
# VM
#
RUN_ESMF_VMUTest:
	-$(RM) *VMUTest.Log
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_VMUTest > $(ESMF_TESTDIR)/ESMF_VMUTest.stdout
	-cat *VMUTest.Log > $(ESMC_TESTDIR)/ESMF_VMUTest.Log
	-$(RM) *VMUTest.Log

RUN_ESMF_VMUTestUNI:
	-$(RM) *VMUTest.Log
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_VMUTest > $(ESMF_TESTDIR)/ESMF_VMUTest.stdout
	-cat *VMUTest.Log > $(ESMC_TESTDIR)/ESMF_VMUTest.Log
	-$(RM) *VMUTest.Log

#
# VM Send Receive
#
RUN_ESMF_VMSendVMRecvUTest:
	-$(RM) *VMSendVMRecvUTest.Log
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_VMSendVMRecvUTest > $(ESMF_TESTDIR)/ESMF_VMSendVMRecvUTest.stdout
	-cat *VMSendVMRecvUTest.Log > $(ESMC_TESTDIR)/ESMF_VMSendVMRecvUTest.Log
	-$(RM) *VMSendVMRecvUTest.Log

RUN_ESMF_VMSendVMRecvUTestUNI:
	-$(RM) *VMSendVMRecvUTest.Log
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_VMSendVMRecvUTest > $(ESMF_TESTDIR)/ESMF_VMSendVMRecvUTest.stdout
	-cat *VMSendVMRecvUTest.Log > $(ESMC_TESTDIR)/ESMF_VMSendVMRecvUTest.Log
	-$(RM) *VMSendVMRecvUTest.Log

#
# VM Sendreceive
#
RUN_ESMF_VMSendRecvUTest:
	-$(RM) *VMSendRecvUTest.Log
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_VMSendRecvUTest > $(ESMF_TESTDIR)/ESMF_VMSendRecvUTest.stdout
	-cat *VMSendRecvUTest.Log > $(ESMC_TESTDIR)/ESMF_VMSendRecvUTest.Log
	-$(RM) *VMSendRecvUTest.Log

RUN_ESMF_VMSendRecvUTestUNI:
	-$(RM) *VMSendRecvUTest.Log
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_VMSendRecvUTest > $(ESMC_TESTDIR)/ESMF_VMSendRecvUTest.stdout
	-cat *VMSendRecvUTest.Log > $(ESMC_TESTDIR)/ESMF_VMSendRecvUTest.Log
	-$(RM) *VMSendRecvUTest.Log

#
# VM Scatter 
#
RUN_ESMF_VMScatterUTest:
	-$(RM) *VMScatterUTest.Log
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_VMScatterUTest > $(ESMF_TESTDIR)/ESMF_VMScatterUTest.stdout
	-cat *VMScatterUTest.Log > $(ESMC_TESTDIR)/ESMF_VMScatterUTest.Log
	-$(RM) *VMScatterUTest.Log

RUN_ESMF_VMScatterUTestUNI:
	-$(RM) *VMScatterUTest.Log
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_VMScatterUTest > $(ESMF_TESTDIR)/ESMF_VMScatterUTest.stdout
	-cat *VMScatterUTest.Log > $(ESMC_TESTDIR)/ESMF_VMScatterUTest.Log
	-$(RM) *VMScatterUTest.Log

#
# VM Gather
#
RUN_ESMF_VMGatherUTest:
	-$(RM) *VMGatherUTest.Log
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_VMGatherUTest > $(ESMF_TESTDIR)/ESMF_VMGatherUTest.stdout
	-cat *VMGatherUTest.Log > $(ESMC_TESTDIR)/ESMF_VMGatherUTest.Log
	-$(RM) *VMGatherUTest.Log

RUN_ESMF_VMGatherUTestUNI:
	-$(RM) *VMGatherUTest.Log
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_VMGatherUTest > $(ESMF_TESTDIR)/ESMF_VMGatherUTest.stdout
	-cat *VMGatherUTest.Log > $(ESMC_TESTDIR)/ESMF_VMGatherUTest.Log
	-$(RM) *VMGatherUTest.Log

#
# VM Broadcast
#
RUN_ESMF_VMBroadcastUTest:
	-$(RM) *VMBroadcastUTest.Log
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_VMBroadcastUTest > $(ESMF_TESTDIR)/ESMF_VMBroadcastUTest.stdout
	-cat *VMBroadcastUTest.Log > $(ESMC_TESTDIR)/ESMF_VMBroadcastUTest.Log
	-$(RM) *VMBroadcastUTest.Log

RUN_ESMF_VMBroadcastUTestUNI:
	-$(RM) *VMBroadcastUTest.Log
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_VMBroadcastUTest > $(ESMF_TESTDIR)/ESMF_VMBroadcastUTest.stdout
	-cat *VMBroadcastUTest.Log > $(ESMC_TESTDIR)/ESMF_VMBroadcastUTest.Log
	-$(RM) *VMBroadcastUTest.Log

#
# VM Barrier
#
RUN_ESMF_VMBarrierUTest:
	-$(RM) *VMBarrierUTest.Log
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_VMBarrierUTest > $(ESMF_TESTDIR)/ESMF_VMBarrierUTest.stdout
	-cat *VMBarrierUTest.Log > $(ESMC_TESTDIR)/ESMF_VMBarrierUTest.Log
	-$(RM) *VMBarrierUTest.Log

RUN_ESMF_VMBarrierUTestUNI:
	-$(RM) *VMBarrierUTest.Log
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_VMBarrierUTest > $(ESMF_TESTDIR)/ESMF_VMBarrierUTest.stdout
	-cat *VMBarrierUTest.Log > $(ESMC_TESTDIR)/ESMF_VMBarrierUTest.Log
	-$(RM) *VMBarrierUTest.Log

