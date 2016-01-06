# $Id$

ALL:      build_examples
run:      run_examples
run_uni:  run_examples_uni

LOCDIR	  = src/Infrastructure/ArrayBundle/examples


.NOTPARALLEL:
EXAMPLES_BUILD    = $(ESMF_EXDIR)/ESMF_ArrayBundleEx \
                    $(ESMF_EXDIR)/ESMF_ArrayBundleHaloEx

EXAMPLES_RUN      = run_ESMF_ArrayBundleEx \
                    run_ESMF_ArrayBundleHaloEx

EXAMPLES_RUN_UNI  = 


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

#
#  Run targets
#
run_ESMF_ArrayBundleEx:
	$(MAKE) EXNAME=ArrayBundle NP=4 exfrun

#
run_ESMF_ArrayBundleHaloEx:
	$(MAKE) EXNAME=ArrayBundleHalo NP=4 exfrun

