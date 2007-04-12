# $Id: makefile,v 1.24 2007/04/12 20:41:36 theurich Exp $ 

ALL: build_examples

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
LOCDIR	  = src/Infrastructure/LogErr/examples

EXAMPLES_BUILD = $(ESMF_EXDIR)/ESMF_LogErrEx

EXAMPLES_RUN = run_ESMF_LogErrEx

EXAMPLES_RUN_UNI = run_ESMF_LogErrEx_uni


include $(ESMF_DIR)/makefile

DIRS	  = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =


#
#  ESMF_LogErrEx
#
run_ESMF_LogErrEx:
	$(MAKE) EXNAME=LogErr NP=4 exfrun

run_ESMF_LogErrEx_uni:
	$(MAKE) EXNAME=LogErr NP=1 exfrun

