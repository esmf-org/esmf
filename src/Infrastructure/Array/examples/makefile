# $Id: makefile,v 1.9 2006/01/30 20:10:05 nscollins Exp $ 

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
LOCDIR	  = src/Infrastructure/Array/examples



EXAMPLES_BUILD   =  $(ESMF_EXDIR)/ESMF_ArrayCreateEx \
                    $(ESMF_EXDIR)/ESMF_ArrayGetEx 


EXAMPLES_RUN     =  run_ESMF_ArrayCreateEx \
                    run_ESMF_ArrayGetEx


EXAMPLES_RUN_UNI =  run_ESMF_ArrayCreateEx_uni \
                    run_ESMF_ArrayGetEx_uni 



include $(ESMF_DIR)/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =



#
# ESMF_ArrayCreateEx
#
run_ESMF_ArrayCreateEx:
	$(MAKE) EXNAME=ArrayCreate NP=4 exfrun

run_ESMF_ArrayCreateEx_uni:
	$(MAKE) EXNAME=ArrayCreate NP=1 exfrun


#
# ESMF_ArrayGetEx
#
run_ESMF_ArrayGetEx:
	$(MAKE) EXNAME=ArrayGet NP=4 exfrun

run_ESMF_ArrayGetEx_uni:
	$(MAKE) EXNAME=ArrayGet NP=1 exfrun

