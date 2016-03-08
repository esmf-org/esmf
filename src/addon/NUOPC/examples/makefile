# $Id$ 

ALL:      build_examples
run:      run_examples
run_uni:  run_examples_uni

LOCDIR	  = src/addon/NUOPC/examples

.NOTPARALLEL:
EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_NUOPCBasicModelEx $(ESMF_EXDIR)/ESMF_NUOPCAtmModelEx

EXAMPLES_RUN   = run_ESMF_NUOPCBasicModelEx run_ESMF_NUOPCAtmModelEx

EXAMPLES_RUN_UNI = run_ESMF_NUOPCBasicModelEx_uni run_ESMF_NUOPCAtmModelEx_uni

include $(ESMF_DIR)/makefile

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

DIRS = 

#
# ESMF_NUOPCBasicModelEx
#
run_ESMF_NUOPCBasicModelEx:
	$(MAKE) EXNAME=NUOPCBasicModel NP=4 exfrun

run_ESMF_NUOPCBasicModelEx_uni:
	$(MAKE) EXNAME=NUOPCBasicModel NP=1 exfrun

#
# ESMF_NUOPCAtmModelEx
#
run_ESMF_NUOPCAtmModelEx:
	$(MAKE) EXNAME=NUOPCAtmModel NP=4 exfrun

run_ESMF_NUOPCAtmModelEx_uni:
	$(MAKE) EXNAME=NUOPCAtmModel NP=1 exfrun


