# $Id: makefile,v 1.33 2009/03/27 05:33:32 theurich Exp $ 

ALL:      build_examples
run:      run_examples
run_uni:  run_examples_uni

LOCDIR	  = src/Superstructure/Component/examples


EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_AppMainEx \
                 $(ESMF_EXDIR)/ESMF_CplEx \
                 $(ESMF_EXDIR)/ESMF_GCompEx \
                 $(ESMF_EXDIR)/ESMF_InternalStateEx \
                 $(ESMF_EXDIR)/ESMF_InternalStateModEx \

EXAMPLES_RUN   = run_ESMF_AppMainEx \
                 run_ESMF_CplEx \
                 run_ESMF_GCompEx \
                 run_ESMF_InternalStateEx \
                 run_ESMF_InternalStateModEx
                 
EXAMPLES_RUN_UNI = run_ESMF_AppMainEx_uni \
                   run_ESMF_CplEx_uni \
                   run_ESMF_GCompEx_uni \
                   run_ESMF_InternalStateEx_uni \
                   run_ESMF_InternalStateModEx_uni
                   
include $(ESMF_DIR)/makefile

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

DIRS = 

#
# ESMF_AppMainEx
#
run_ESMF_AppMainEx:
	cp -f setup.rc $(ESMF_EXDIR)
	$(MAKE) EXNAME=AppMain NP=4 exfrun

run_ESMF_AppMainEx_uni:
	cp -f setup.rc $(ESMF_EXDIR)
	$(MAKE) EXNAME=AppMain NP=1 exfrun

#
# ESMF_CplEx
#
run_ESMF_CplEx:
	$(MAKE) EXNAME=Cpl NP=4 exfrun

run_ESMF_CplEx_uni:
	$(MAKE) EXNAME=Cpl NP=1 exfrun

#
# ESMF_GCompEx
#
run_ESMF_GCompEx:
	$(MAKE) EXNAME=GComp NP=4 exfrun

run_ESMF_GCompEx_uni:
	$(MAKE) EXNAME=GComp NP=1 exfrun

#
# ESMF_InternalStateEx
#
run_ESMF_InternalStateEx:
	$(MAKE) EXNAME=InternalState NP=4 exfrun

run_ESMF_InternalStateEx_uni:
	$(MAKE) EXNAME=InternalState NP=1 exfrun

#
# ESMF_InternalStateModEx
#
run_ESMF_InternalStateModEx:
	$(MAKE) EXNAME=InternalStateMod NP=4 exfrun

run_ESMF_InternalStateModEx_uni:
	$(MAKE) EXNAME=InternalStateMod NP=1 exfrun

