# $Id: makefile,v 1.23 2009/03/03 17:23:24 theurich Exp $

ALL:      build_examples
run:      run_examples
run_uni:  run_examples_uni

LOCDIR	  = src/Infrastructure/VM/examples


EXAMPLES_BUILD    = $(ESMF_EXDIR)/ESMF_VMDefaultBasicsEx \
                    $(ESMF_EXDIR)/ESMF_VMGetMPICommunicatorEx \
                    $(ESMF_EXDIR)/ESMF_VMUserMpiEx \
                    $(ESMF_EXDIR)/ESMF_VMUserMpiCommEx \
                    $(ESMF_EXDIR)/ESMF_VMSendVMRecvEx \
                    $(ESMF_EXDIR)/ESMF_VMScatterVMGatherEx \
                    $(ESMF_EXDIR)/ESMF_VMAllFullReduceEx \
                    $(ESMF_EXDIR)/ESMF_VMComponentEx

EXAMPLES_RUN      = run_ESMF_VMDefaultBasicsEx \
                    run_ESMF_VMGetMPICommunicatorEx \
                    run_ESMF_VMUserMpiEx \
                    run_ESMF_VMUserMpiCommEx \
                    run_ESMF_VMSendVMRecvEx \
                    run_ESMF_VMScatterVMGatherEx \
                    run_ESMF_VMAllFullReduceEx \
                    run_ESMF_VMComponentEx

EXAMPLES_RUN_UNI  = run_ESMF_VMDefaultBasicsEx_uni \
                    run_ESMF_VMGetMPICommunicatorEx_uni \
                    run_ESMF_VMUserMpiEx_uni \
                    run_ESMF_VMUserMpiCommEx_uni \
                    run_ESMF_VMSendVMRecvEx_uni \
                    run_ESMF_VMScatterVMGatherEx_uni \
                    run_ESMF_VMAllFullReduceEx_uni \
                    run_ESMF_VMComponentEx_uni


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

#
#  Run targets
#
run_ESMF_VMDefaultBasicsEx:
	$(MAKE) EXNAME=VMDefaultBasics NP=4 exfrun

run_ESMF_VMDefaultBasicsEx_uni:
	$(MAKE) EXNAME=VMDefaultBasics NP=1 exfrun

run_ESMF_VMGetMPICommunicatorEx:
	$(MAKE) EXNAME=VMGetMPICommunicator NP=4 exfrun

run_ESMF_VMGetMPICommunicatorEx_uni:
	$(MAKE) EXNAME=VMGetMPICommunicator NP=1 exfrun

run_ESMF_VMUserMpiEx:
	$(MAKE) EXNAME=VMUserMpi NP=4 exfrun

run_ESMF_VMUserMpiEx_uni:
	$(MAKE) EXNAME=VMUserMpi NP=1 exfrun

run_ESMF_VMUserMpiCommEx:
	$(MAKE) EXNAME=VMUserMpiComm NP=4 exfrun

run_ESMF_VMUserMpiCommEx_uni:
	$(MAKE) EXNAME=VMUserMpiComm NP=1 exfrun

run_ESMF_VMSendVMRecvEx:
	$(MAKE) EXNAME=VMSendVMRecv NP=4 exfrun

run_ESMF_VMSendVMRecvEx_uni:
	$(MAKE) EXNAME=VMSendVMRecv NP=1 exfrun

run_ESMF_VMScatterVMGatherEx:
	$(MAKE) EXNAME=VMScatterVMGather NP=4 exfrun

run_ESMF_VMScatterVMGatherEx_uni:
	$(MAKE) EXNAME=VMScatterVMGather NP=1 exfrun

run_ESMF_VMAllFullReduceEx:
	$(MAKE) EXNAME=VMAllFullReduce NP=4 exfrun

run_ESMF_VMAllFullReduceEx_uni:
	$(MAKE) EXNAME=VMAllFullReduce NP=1 exfrun

run_ESMF_VMComponentEx:
	$(MAKE) EXNAME=VMComponent NP=4 exfrun

run_ESMF_VMComponentEx_uni:
	$(MAKE) EXNAME=VMComponent NP=1 exfrun


