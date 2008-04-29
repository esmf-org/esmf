# $Id: makefile,v 1.1.2.1 2008/04/29 18:51:21 theurich Exp $

ALL:      build_examples
run:      run_examples
run_uni:  run_examples_uni

LOCDIR	  = src/Infrastructure/ArrayBundle/examples


EXAMPLES_BUILD    = $(ESMF_EXDIR)/ESMF_ArrayBundleEx

EXAMPLES_RUN      = run_ESMF_ArrayBundleEx

EXAMPLES_RUN_UNI  = run_ESMF_ArrayBundleEx_uni


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

run_ESMF_ArrayBundleEx_uni:
	$(MAKE) EXNAME=ArrayBundle NP=1 exfrun

