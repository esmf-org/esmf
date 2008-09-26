# $Id: makefile,v 1.4 2008/09/26 04:16:50 cdeluca Exp $ 

ALL: build_examples

run: run_examples

LOCDIR    = src/Infrastructure/FieldBundle/examples

EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_FieldBundleCreateEx \
                 $(ESMF_EXDIR)/ESMF_FieldBundleRedistEx \
                 $(ESMF_EXDIR)/ESMF_FieldBundleSMMEx 

EXAMPLES_RUN = run_ESMF_FieldBundleCreateEx \
               run_ESMF_FieldBundleRedistEx \
               run_ESMF_FieldBundleSMMEx 

EXAMPLES_RUN_UNI = run_ESMF_FieldBundleCreateEx_uni \
                   run_ESMF_FieldBundleRedistEx_uni \
                   run_ESMF_FieldBundleSMMEx_uni 

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

run_ESMF_FieldBundleRedistEx_uni:
	echo "PASS: ESMF_FieldBundleRedistEx not set up to run on one processor." > $(ESMF_EXDIR)/ESMF_FieldBundleRedistEx.stdout

run_ESMF_FieldBundleSMMEx:
	$(MAKE) EXNAME=FieldBundleSMM NP=4 exfrun

run_ESMF_FieldBundleSMMEx_uni:
	echo "PASS: ESMF_FieldBundleSMMEx not set up to run on one processor." > $(ESMF_EXDIR)/ESMF_FieldBundleSMMEx.stdout

