# $Id: makefile,v 1.10 2005/01/28 17:41:20 theurich Exp $ 

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
                RUN_ESMF_VMSendVMRecvUTestUNI

TESTS_RM      =

include ${ESMF_DIR}/makefile

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod NULL *.txt stdoutfiles core ESMF_LogFile $(TESTS_BUILD)
CLOBBERDIRS =


#
# VM
#
RUN_ESMF_VMUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_VMUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_VMUTest > $(ESMC_TESTDIR)/ESMF_VMUTest.stdout

RUN_ESMF_VMUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_VMUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_VMUTest > $(ESMC_TESTDIR)/ESMF_VMUTest.stdout

#
# VM Send Receive
#
RUN_ESMF_VMSendVMRecvUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_VMSendVMRecvUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_VMSendVMRecvUTest > $(ESMC_TESTDIR)/ESMF_VMSendVMRecvUTest.stdout

RUN_ESMF_VMSendVMRecvUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_VMSendVMRecvUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_VMSendVMRecvUTest > $(ESMC_TESTDIR)/ESMF_VMSendVMRecvUTest.stdout

#
# VM Sendreceive
#
RUN_ESMF_VMSendRecvUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_VMSendRecvUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_VMSendRecvUTest > $(ESMC_TESTDIR)/ESMF_VMSendRecvUTest.stdout

#
# VM Scatter 
#
RUN_ESMF_VMScatterUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_VMScatterUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_VMScatterUTest > $(ESMC_TESTDIR)/ESMF_VMScatterUTest.stdout

#
# VM Gather
#
RUN_ESMF_VMGatherUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_VMGatherUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_VMGatherUTest > $(ESMC_TESTDIR)/ESMF_VMGatherUTest.stdout

#
# VM Broadcast
#
RUN_ESMF_VMBroadcastUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_VMBroadcastUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_VMBroadcastUTest > $(ESMC_TESTDIR)/ESMF_VMBroadcastUTest.stdout

#
# VM Barrier
#
RUN_ESMF_VMBarrierUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_VMBarrierUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_VMBarrierUTest > $(ESMC_TESTDIR)/ESMF_VMBarrierUTest.stdout

#
# VM ThreadBarrier
#
RUN_ESMF_VMThreadBarrierUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_VMThreadBarrierUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_VMThreadBarrierUTest > $(ESMC_TESTDIR)/ESMF_VMThreadBarrierUTest.stdout
