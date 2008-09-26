# $Id: makefile,v 1.16 2008/09/26 04:16:48 cdeluca Exp $

ALL: build_examples
run: run_examples
run_uni: run_examples_uni

LOCDIR	  = src/Infrastructure/DistGrid/examples


EXAMPLES_BUILD    = $(ESMF_EXDIR)/ESMF_DistGridEx

EXAMPLES_RUN      = run_ESMF_DistGridEx

EXAMPLES_RUN_UNI  = run_ESMF_DistGridEx_uni


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

#
#  Run targets
#
run_ESMF_DistGridEx:
	$(MAKE) EXNAME=DistGrid NP=4 exfrun

run_ESMF_DistGridEx_uni:
	$(MAKE) EXNAME=DistGrid NP=1 exfrun
