# $Id: makefile,v 1.5 2004/12/20 17:29:59 rfaincht Exp $ 

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
		$(ESMC_TESTDIR)/ESMF_VMSendrecvUTest\
		$(ESMC_TESTDIR)/ESMF_VMScatterVMGatherUTest

TESTS_RUN     = RUN_ESMF_VMUTest\
                RUN_ESMF_VMSendVMRecvUTest\
                RUN_ESMF_VMSendrecvUTest\
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
RUN_ESMF_VMSendrecvUTest:
	-@rm -f testlog stdlog.* logfile.* ESMF_VMSendrecvUTest.tmps
	-${MPIRUN} -np 4 $(ESMC_TESTDIR)/ESMF_VMSendrecvUTest > $(ESMC_TESTDIR)/ESMF_VMSendrecvUTest.stdout

ESMF_VMSendrecvUTest.rm:
	echo "ESMF_VMSendrecvUTest.rm target called here."

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

