# $Id: makefile,v 1.8 2005/01/25 18:44:04 jwolfe Exp $ 

ALL:  build_tests

CFLAGS    =
FFLAGS    =
# FFLAGS   += -omp
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

#		$(ESMC_TESTDIR)/ESMF_VMThreadBarrierUTest

TESTS_RUN     = RUN_ESMF_VMUTest \
                RUN_ESMF_VMSendVMRecvUTest \
                RUN_ESMF_VMSendRecvUTest \
                RUN_ESMF_VMScatterUTest \
                RUN_ESMF_VMGatherUTest \
		RUN_ESMF_VMBroadcastUTest \
		RUN_ESMF_VMBarrierUTest

#		RUN_ESMF_VMThreadBarrierUTest

TESTS_RUN_UNI = RUN_ESMF_VMUTestUNI \
                RUN_ESMF_VMSendVMRecvUTestUNI \
                RUN_ESMF_VMScatterUTestUNI \
                RUN_ESMF_VMGatherUTestUNI \
		RUN_ESMF_VMBroadcastUTestUNI \
		RUN_ESMF_VMBarrierUTestUNI

#		RUN_ESMF_VMThreadBarrierUTestUNI

TESTS_RM      = ESMF_VMUTest.rm

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
ESMF_VMUTest.rm:
	echo "ESMF_VMUTest.rm target called here."

#
# VM Send Receive
#
RUN_ESMF_VMSendVMRecvUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_VMSendVMRecvUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_VMSendVMRecvUTest > $(ESMC_TESTDIR)/ESMF_VMSendVMRecvUTest.stdout

ESMF_VMSendVMRecvUTest.rm:
	echo "ESMF_VMSendVMRecvUTest.rm target called here."

#
# VM Sendreceive
#
RUN_ESMF_VMSendRecvUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_VMSendRecvUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_VMSendRecvUTest > $(ESMC_TESTDIR)/ESMF_VMSendRecvUTest.stdout

ESMF_VMSendRecvUTest.rm:
	echo "ESMF_VMSendRecvUTest.rm target called here."

RUN_ESMF_VMSendVMRecvUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_VMSendVMRecvUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_VMSendVMRecvUTest > $(ESMC_TESTDIR)/ESMF_VMSendVMRecvUTest.stdout

#
# VM Scatter 
#
RUN_ESMF_VMScatterUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_VMScatterUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_VMScatterUTest > $(ESMC_TESTDIR)/ESMF_VMScatterUTest.stdout

ESMF_VMScatterUTest.rm:
	echo "ESMF_VMScatterUTest.rm target called here."

#
# VM Gather
#
RUN_ESMF_VMGatherUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_VMGatherUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_VMGatherUTest > $(ESMC_TESTDIR)/ESMF_VMGatherUTest.stdout

ESMF_VMGatherUTest.rm:
	echo "ESMF_VMGatherUTest.rm target called here."
#
# VM Broadcast
#
RUN_ESMF_VMBroadcastUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_VMBroadcastUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_VMBroadcastUTest > $(ESMC_TESTDIR)/ESMF_VMBroadcastUTest.stdout

ESMF_VMBroadcastUTest.rm:
	echo "ESMF_VMBroadcastUTest.rm target called here."

#
# VM Barrier
#
RUN_ESMF_VMBarrierUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_VMBarrierUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_VMBarrierUTest > $(ESMC_TESTDIR)/ESMF_VMBarrierUTest.stdout

ESMF_VMBarrierUTest.rm:
	echo "ESMF_VMBarrierUTest.rm target called here."

#
# VM ThreadBarrier
#
RUN_ESMF_VMThreadBarrierUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_VMThreadBarrierUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_VMThreadBarrierUTest > $(ESMC_TESTDIR)/ESMF_VMThreadBarrierUTest.stdout

ESMF_VMThreadBarrierUTest.rm:
	echo "ESMF_VMThreadBarrierUTest.rm target called here."


