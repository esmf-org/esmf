# $Id: makefile,v 1.23 2006/01/30 20:10:08 nscollins Exp $ 

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
#		 $(ESMF_EXDIR)/ESMC_LogErrEx

EXAMPLES_RUN = run_ESMF_LogErrEx
#	       run_ESMC_LogErrEx  


EXAMPLES_RUN_UNI = run_ESMF_LogErrEx_uni
#                   run_ESMC_LogErrEx_uni 


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

#
#  ESMC_LogErrEx
#
run_ESMC_LogErrEx:
	$(MAKE) EXNAME=LogErr NP=4 excrun

run_ESMC_LogErrEx_uni:
	$(MAKE) EXNAME=LogErr NP=1 excrun




