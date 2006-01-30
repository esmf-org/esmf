# $Id: makefile,v 1.10 2006/01/30 20:10:10 nscollins Exp $ 

ALL:  build_examples

run: run_examples

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Superstructure/Xform/examples


EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_XformEx

# EXAMPLES_RUN   = run_ESMF_XformEx  commented out until it does more
EXAMPLES_RUN   =


# EXAMPLES_RUN_UNI = run_ESMF_XformEx_uni  commented out until it does more
EXAMPLES_RUN_UNI =

include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =


#
# ESMC_XformEx
#
run_ESMC_XformEx:
	$(MAKE) EXNAME=Xform NP=4 excrun

run_ESMC_XformEx_uni:
	$(MAKE) EXNAME=Xform NP=1 excrun


#
# ESMF_XformEx
#
run_ESMF_XformEx:
	$(MAKE) EXNAME=Xform NP=4 exfrun

run_ESMF_XformEx_uni:
	$(MAKE) EXNAME=Xform NP=1 exfrun
