# $Id$ 

ALL: build_examples

run: run_examples

LOCDIR    = src/Infrastructure/FieldBundle/examples

.NOTPARALLEL:
EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_FieldBundleCreateEx \
                 $(ESMF_EXDIR)/ESMF_FieldBundleRedistEx \
                 $(ESMF_EXDIR)/ESMF_FieldBundleSMMEx \
                 $(ESMF_EXDIR)/ESMF_FieldBundleHaloEx 

EXAMPLES_RUN = run_ESMF_FieldBundleCreateEx \
               run_ESMF_FieldBundleRedistEx \
               run_ESMF_FieldBundleSMMEx \
               run_ESMF_FieldBundleHaloEx

EXAMPLES_RUN_UNI = run_ESMF_FieldBundleCreateEx_uni \

include $(ESMF_DIR)/makefile


DIRS      = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =


#
# ESMF_FieldBundleCreateEx
#
run_ESMF_FieldBundleCreateEx:
	$(MAKE) EXNAME=FieldBundleCreate NP=4 exfrun

run_ESMF_FieldBundleCreateEx_uni:
	$(MAKE) EXNAME=FieldBundleCreate NP=1 exfrun

run_ESMF_FieldBundleRedistEx:
	$(MAKE) EXNAME=FieldBundleRedist NP=4 exfrun

run_ESMF_FieldBundleSMMEx:
	$(MAKE) EXNAME=FieldBundleSMM NP=4 exfrun

run_ESMF_FieldBundleHaloEx:
	$(MAKE) EXNAME=FieldBundleHalo NP=4 exfrun
