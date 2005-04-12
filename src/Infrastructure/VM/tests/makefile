# $Id: makefile,v 1.17 2005/04/12 16:52:47 nscollins Exp $ 

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
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_VMUTest > $(ESMF_TESTDIR)/ESMF_VMUTest.stdout
	-cat *VMUTest.Log > $(ESMC_TESTDIR)/ESMF_VMUTest.Log
	-rm *VMUTest.Log

RUN_ESMF_VMUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_VMUTest > $(ESMF_TESTDIR)/ESMF_VMUTest.stdout
	-cat *VMUTest.Log > $(ESMC_TESTDIR)/ESMF_VMUTest.Log
	-rm *VMUTest.Log

#
# VM Send Receive
#
RUN_ESMF_VMSendVMRecvUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_VMSendVMRecvUTest > $(ESMF_TESTDIR)/ESMF_VMSendVMRecvUTest.stdout
	-cat *VMSendVMRecvUTest.Log > $(ESMC_TESTDIR)/ESMF_VMSendVMRecvUTest.Log
	-rm *VMSendVMRecvUTest.Log

RUN_ESMF_VMSendVMRecvUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_VMSendVMRecvUTest > $(ESMF_TESTDIR)/ESMF_VMSendVMRecvUTest.stdout
	-cat *VMSendVMRecvUTest.Log > $(ESMC_TESTDIR)/ESMF_VMSendVMRecvUTest.Log
	-rm *VMSendVMRecvUTest.Log

#
# VM Sendreceive
#
RUN_ESMF_VMSendRecvUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_VMSendRecvUTest > $(ESMF_TESTDIR)/ESMF_VMSendRecvUTest.stdout
	-cat *VMSendRecvUTest.Log > $(ESMC_TESTDIR)/ESMF_VMSendRecvUTest.Log
	-rm *VMSendRecvUTest.Log

RUN_ESMF_VMSendRecvUTestUNI:
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_VMSendRecvUTest > $(ESMC_TESTDIR)/ESMF_VMSendRecvUTest.stdout
	-cat *VMSendRecvUTest.Log > $(ESMC_TESTDIR)/ESMF_VMSendRecvUTest.Log
	-rm *VMSendRecvUTest.Log

#
# VM Scatter 
#
RUN_ESMF_VMScatterUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_VMScatterUTest > $(ESMF_TESTDIR)/ESMF_VMScatterUTest.stdout
	-cat *VMScatterUTest.Log > $(ESMC_TESTDIR)/ESMF_VMScatterUTest.Log
	-rm *VMScatterUTest.Log

RUN_ESMF_VMScatterUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_VMScatterUTest > $(ESMF_TESTDIR)/ESMF_VMScatterUTest.stdout
	-cat *VMScatterUTest.Log > $(ESMC_TESTDIR)/ESMF_VMScatterUTest.Log
	-rm *VMScatterUTest.Log

#
# VM Gather
#
RUN_ESMF_VMGatherUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_VMGatherUTest > $(ESMF_TESTDIR)/ESMF_VMGatherUTest.stdout
	-cat *VMGatherUTest.Log > $(ESMC_TESTDIR)/ESMF_VMGatherUTest.Log
	-rm *VMGatherUTest.Log

RUN_ESMF_VMGatherUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_VMGatherUTest > $(ESMF_TESTDIR)/ESMF_VMGatherUTest.stdout
	-cat *VMGatherUTest.Log > $(ESMC_TESTDIR)/ESMF_VMGatherUTest.Log
	-rm *VMGatherUTest.Log

#
# VM Broadcast
#
RUN_ESMF_VMBroadcastUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_VMBroadcastUTest > $(ESMF_TESTDIR)/ESMF_VMBroadcastUTest.stdout
	-cat *VMBroadcastUTest.Log > $(ESMC_TESTDIR)/ESMF_VMBroadcastUTest.Log
	-rm *VMBroadcastUTest.Log

RUN_ESMF_VMBroadcastUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_VMBroadcastUTest > $(ESMF_TESTDIR)/ESMF_VMBroadcastUTest.stdout
	-cat *VMBroadcastUTest.Log > $(ESMC_TESTDIR)/ESMF_VMBroadcastUTest.Log
	-rm *VMBroadcastUTest.Log

#
# VM Barrier
#
RUN_ESMF_VMBarrierUTest:
	-${MPIRUN} -np 4 $(ESMF_TESTDIR)/ESMF_VMBarrierUTest > $(ESMF_TESTDIR)/ESMF_VMBarrierUTest.stdout
	-cat *VMBarrierUTest.Log > $(ESMC_TESTDIR)/ESMF_VMBarrierUTest.Log
	-rm *VMBarrierUTest.Log

RUN_ESMF_VMBarrierUTestUNI:
	-${MPIRUN} -np 1 $(ESMF_TESTDIR)/ESMF_VMBarrierUTest > $(ESMF_TESTDIR)/ESMF_VMBarrierUTest.stdout
	-cat *VMBarrierUTest.Log > $(ESMC_TESTDIR)/ESMF_VMBarrierUTest.Log
	-rm *VMBarrierUTest.Log

