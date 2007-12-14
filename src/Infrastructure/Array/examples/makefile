# $Id: makefile,v 1.16.2.2 2007/12/14 20:10:47 theurich Exp $

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
                    $(ESMF_EXDIR)/ESMF_ArraySparseMatMulEx

EXAMPLES_RUN      = run_ESMF_ArrayFarrayEx \
                    run_ESMF_ArrayFarrayHaloEx \
                    run_ESMF_ArrayLarrayEx \
                    run_ESMF_ArrayEx \
                    run_ESMF_ArrayScatterGatherEx \
                    run_ESMF_ArrayRedistEx \
                    run_ESMF_ArraySparseMatMulEx

EXAMPLES_RUN_UNI  = run_ESMF_ArrayFarrayEx_uni \
                    run_ESMF_ArrayFarrayHaloEx_uni \
                    run_ESMF_ArrayLarrayEx_uni \
                    run_ESMF_ArrayEx_uni \
                    run_ESMF_ArrayScatterGatherEx_uni \
                    run_ESMF_ArrayRedistEx_uni \
                    run_ESMF_ArraySparseMatMulEx_uni


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

run_ESMF_ArrayLarrayEx_uni:
	$(MAKE) EXNAME=ArrayLarray NP=1 exfrun

run_ESMF_ArrayFarrayHaloEx:
	$(MAKE) EXNAME=ArrayFarrayHalo NP=4 exfrun

run_ESMF_ArrayFarrayHaloEx_uni:
	$(MAKE) EXNAME=ArrayFarrayHalo NP=1 exfrun

run_ESMF_ArrayFarrayEx:
	$(MAKE) EXNAME=ArrayFarray NP=4 exfrun

run_ESMF_ArrayFarrayEx_uni:
	$(MAKE) EXNAME=ArrayFarray NP=1 exfrun

run_ESMF_ArrayEx:
	$(MAKE) EXNAME=Array NP=4 exfrun

run_ESMF_ArrayEx_uni:
	$(MAKE) EXNAME=Array NP=1 exfrun

run_ESMF_ArrayScatterGatherEx:
	$(MAKE) EXNAME=ArrayScatterGather NP=4 exfrun

run_ESMF_ArrayScatterGatherEx_uni:
	$(MAKE) EXNAME=ArrayScatterGather NP=1 exfrun

run_ESMF_ArrayRedistEx:
	env P4_GLOBMEMSIZE=41943040 $(MAKE) EXNAME=ArrayRedist NP=4 exfrun

run_ESMF_ArrayRedistEx_uni:
	$(MAKE) EXNAME=ArrayRedist NP=1 exfrun

run_ESMF_ArraySparseMatMulEx:
	$(MAKE) EXNAME=ArraySparseMatMul NP=4 exfrun

run_ESMF_ArraySparseMatMulEx_uni:
	$(MAKE) EXNAME=ArraySparseMatMul NP=1 exfrun

