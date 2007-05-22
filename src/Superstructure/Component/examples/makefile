# $Id: makefile,v 1.29 2007/05/22 18:18:43 theurich Exp $ 

ALL:      build_examples
run:      run_examples
run_uni:  run_examples_uni

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Superstructure/Component/examples


EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_AppMainEx \
                 $(ESMF_EXDIR)/ESMF_CplEx \
                 $(ESMF_EXDIR)/ESMF_GCompEx \
                 $(ESMF_EXDIR)/ESMF_InternalStateEx \
                 $(ESMF_EXDIR)/ESMC_AppMainEx \
                 $(ESMF_EXDIR)/ESMC_CplEx \
                 $(ESMF_EXDIR)/ESMC_GCompEx

EXAMPLES_RUN   = run_ESMF_AppMainEx \
                 run_ESMF_CplEx \
                 run_ESMF_GCompEx \
                 run_ESMF_InternalStateEx \
                 run_ESMC_AppMainEx \
                 run_ESMC_CplEx \
                 run_ESMC_GCompEx
                 
EXAMPLES_RUN_UNI = run_ESMF_AppMainEx_uni \
                   run_ESMF_CplEx_uni \
                   run_ESMF_GCompEx_uni \
                   run_ESMF_InternalStateEx_uni \
                   run_ESMC_AppMainEx_uni \
                   run_ESMC_CplEx_uni \
                   run_ESMC_GCompEx_uni
                   
include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =


#
# ESMC_AppMainEx
#
run_ESMC_AppMainEx:
	cp -f setup.rc $(ESMF_EXDIR)
	$(MAKE) EXNAME=AppMain NP=4 excrun

run_ESMC_AppMainEx_uni:
	cp -f setup.rc $(ESMF_EXDIR)
	$(MAKE) EXNAME=AppMain NP=1 excrun

#
# ESMC_CplEx
#
run_ESMC_CplEx:
	$(MAKE) EXNAME=Cpl NP=4 excrun

run_ESMC_CplEx_uni:
	$(MAKE) EXNAME=Cpl NP=1 excrun

#
# ESMC_GCompEx
#
run_ESMC_GCompEx:
	$(MAKE) EXNAME=GComp NP=4 excrun

run_ESMC_GCompEx_uni:
	$(MAKE) EXNAME=GComp NP=1 excrun

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


