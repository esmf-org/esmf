# $Id$

ALL:      build_examples
run:      run_examples
run_uni:  run_examples_uni

LOCDIR	  = src/Infrastructure/Base/examples

.NOTPARALLEL:
EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_InfoTutorialEx \
                 $(ESMF_EXDIR)/ESMF_InfoGetFromHostEx

EXAMPLES_RUN   = run_ESMF_InfoTutorialEx \
                 run_ESMF_InfoGetFromHostEx

EXAMPLES_RUN_UNI = run_ESMF_InfoTutorialEx_uni \
                   run_ESMF_InfoGetFromHostEx_uni

include $(ESMF_DIR)/makefile

CLEANDIRS   =
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

DIRS =

# =============================================================================

run_ESMF_InfoTutorialEx:
	$(MAKE) EXNAME=InfoTutorial NP=4 exfrun

run_ESMF_InfoTutorialEx_uni:
	$(MAKE) EXNAME=InfoTutorial NP=1 exfrun

run_ESMF_InfoGetFromHostEx:
	$(MAKE) EXNAME=InfoGetFromHost NP=4 exfrun

run_ESMF_InfoGetFromHostEx_uni:
	$(MAKE) EXNAME=InfoGetFromHost NP=1 exfrun
