# $Id: makefile,v 1.19 2009/10/21 05:54:17 theurich Exp $

ALL:      build_examples
run:      run_examples
run_uni:  run_examples_uni

LOCDIR	  = src/Infrastructure/Array/examples


EXAMPLES_BUILD    = $(ESMF_EXDIR)/ESMF_ArrayFarrayEx \
                    $(ESMF_EXDIR)/ESMF_ArrayFarrayHaloEx \
                    $(ESMF_EXDIR)/ESMF_ArrayLarrayEx \
                    $(ESMF_EXDIR)/ESMF_ArrayEx \
                    $(ESMF_EXDIR)/ESMF_ArrayScatterGatherEx \
                    $(ESMF_EXDIR)/ESMF_ArrayRedistEx \
                    $(ESMF_EXDIR)/ESMF_ArraySparseMatMulEx \
                    $(ESMF_EXDIR)/ESMF_ArrayScatterGatherArbEx

EXAMPLES_RUN      = run_ESMF_ArrayFarrayEx \
                    run_ESMF_ArrayFarrayHaloEx \
                    run_ESMF_ArrayLarrayEx \
                    run_ESMF_ArrayEx \
                    run_ESMF_ArrayScatterGatherEx \
                    run_ESMF_ArrayRedistEx \
                    run_ESMF_ArraySparseMatMulEx \
                    run_ESMF_ArrayScatterGatherArbEx

EXAMPLES_RUN_UNI  = 


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

#
#  Run targets
#
run_ESMF_ArrayLarrayEx:
	$(MAKE) EXNAME=ArrayLarray NP=4 exfrun


run_ESMF_ArrayFarrayHaloEx:
	$(MAKE) EXNAME=ArrayFarrayHalo NP=4 exfrun


run_ESMF_ArrayFarrayEx:
	$(MAKE) EXNAME=ArrayFarray NP=4 exfrun


run_ESMF_ArrayEx:
	$(MAKE) EXNAME=Array NP=4 exfrun


run_ESMF_ArrayScatterGatherEx:
	$(MAKE) EXNAME=ArrayScatterGather NP=4 exfrun


run_ESMF_ArrayRedistEx:
	$(MAKE) EXNAME=ArrayRedist NP=4 exfrun


run_ESMF_ArraySparseMatMulEx:
	$(MAKE) EXNAME=ArraySparseMatMul NP=4 exfrun


run_ESMF_ArrayScatterGatherArbEx:
	$(MAKE) EXNAME=ArrayScatterGatherArb NP=4 exfrun

