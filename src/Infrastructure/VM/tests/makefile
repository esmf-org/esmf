# $Id: makefile,v 1.4 2004/12/16 01:11:36 theurich Exp $ 

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
		$(ESMC_TESTDIR)/ESMF_VMScatterVMGatherUTest

TESTS_RUN     = RUN_ESMF_VMUTest\
                RUN_ESMF_VMSendVMRecvUTest\
                RUN_ESMF_VMScatterVMGatherUTest

TESTS_RUN_UNI = RUN_ESMF_VMUTestUNI \
                RUN_ESMF_VMSendVMRecvUTestUNI\
                RUN_ESMF_VMScatterVMGatherUTestUNI

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
# VM Scatter Gather
#
RUN_ESMF_VMScatterVMGatherUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_VMScatterVMGatherUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_VMScatterVMGatherUTest > $(ESMC_TESTDIR)/ESMF_VMScatterVMGatherUTest.stdout

RUN_ESMF_VMScatterVMGatherUTestUNI:
	-@rm -f testlog stdlog.* logfile.* ESMF_VMScatterVMGatherUTest.tmps
	-${MPIRUN} -np 1 $(ESMC_TESTDIR)/ESMF_VMScatterVMGatherUTest > $(ESMC_TESTDIR)/ESMF_VMScatterVMGatherUTest.stdout
