# $Id: makefile,v 1.6 2004/12/21 16:11:28 rfaincht Exp $ 

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

TESTS_BUILD   = $(ESMC_TESTDIR)/ESMF_VMUTest\
		$(ESMC_TESTDIR)/ESMF_VMSendVMRecvUTest\
		$(ESMC_TESTDIR)/ESMF_VMSendRecvUTest\
		$(ESMC_TESTDIR)/ESMF_VMScatterVMGatherUTest

TESTS_RUN     = RUN_ESMF_VMUTest\
                RUN_ESMF_VMSendVMRecvUTest\
                RUN_ESMF_VMSendRecvUTest\
                RUN_ESMF_VMScatterVMGatherUTest

TESTS_RUN_UNI = RUN_ESMF_VMUTestUNI \
                RUN_ESMF_VMSendVMRecvUTestUNI\
                RUN_ESMF_VMScatterVMGatherUTestUNI

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
# VM Scatter Gather
#
RUN_ESMF_VMScatterVMGatherUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_VMScatterVMGatherUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_VMScatterVMGatherUTest > $(ESMC_TESTDIR)/ESMF_VMScatterVMGatherUTest.stdout

ESMF_VMScatterVMGatherUTest.rm:
	echo "ESMF_VMScatterVMGatherUTest.rm target called here."

