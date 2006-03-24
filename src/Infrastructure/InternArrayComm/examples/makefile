# $Id: makefile,v 1.1 2006/03/24 16:36:36 theurich Exp $ 

ALL:  build_examples

run:  run_examples

CFLAGS    =
FFLAGS    =
SOURCEC	  = 
SOURCEF	  =

SOURCEH	  = 
OBJSC	  =  
OBJSF	  = 
MANSEC	  = 
LIBBASE	  = 
LOCDIR	  = src/Infrastructure/InternArrayComm/examples



EXAMPLES_BUILD   =  $(ESMF_EXDIR)/ESMF_InternArrayCommEx 


EXAMPLES_RUN     =  run_ESMF_InternArrayCommEx


EXAMPLES_RUN_UNI =  run_ESMF_InternArrayCommEx_uni 



include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =



#
# ESMF_InternArrayCommEx
#
run_ESMF_InternArrayCommEx:
	$(MAKE) EXNAME=InternArrayComm NP=4 exfrun

run_ESMF_InternArrayCommEx_uni:
	$(MAKE) EXNAME=InternArrayComm NP=1 exfrun

