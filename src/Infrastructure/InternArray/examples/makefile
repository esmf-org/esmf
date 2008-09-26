# $Id: makefile,v 1.2 2008/09/26 04:16:56 cdeluca Exp $ 

ALL:  build_examples

run:  run_examples

LOCDIR	  = src/Infrastructure/InternArray/examples



EXAMPLES_BUILD   =  $(ESMF_EXDIR)/ESMF_InternArrayDataMapEx \
                    $(ESMF_EXDIR)/ESMF_InternArrayCreateEx \
                    $(ESMF_EXDIR)/ESMF_InternArrayGetEx 


EXAMPLES_RUN     =  run_ESMF_InternArrayDataMapEx \
                    run_ESMF_InternArrayCreateEx \
                    run_ESMF_InternArrayGetEx


EXAMPLES_RUN_UNI =  run_ESMF_InternArrayDataMapEx_uni \
                    run_ESMF_InternArrayCreateEx_uni \
                    run_ESMF_InternArrayGetEx_uni 



include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =


#
# ESMF_InternArrayDataMapEx
#
run_ESMF_InternArrayDataMapEx:
	$(MAKE) EXNAME=InternArrayDataMap NP=4 exfrun

run_ESMF_InternArrayDataMapEx_uni:
	$(MAKE) EXNAME=InternArrayDataMap NP=1 exfrun

#
# ESMF_InternArrayCreateEx
#
run_ESMF_InternArrayCreateEx:
	$(MAKE) EXNAME=InternArrayCreate NP=4 exfrun

run_ESMF_InternArrayCreateEx_uni:
	$(MAKE) EXNAME=InternArrayCreate NP=1 exfrun


#
# ESMF_InternArrayGetEx
#
run_ESMF_InternArrayGetEx:
	$(MAKE) EXNAME=InternArrayGet NP=4 exfrun

run_ESMF_InternArrayGetEx_uni:
	$(MAKE) EXNAME=InternArrayGet NP=1 exfrun

