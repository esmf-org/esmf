# $Id: makefile,v 1.17 2009/02/27 22:52:43 svasquez Exp $

ALL:  build_examples

run: run_examples

LOCDIR	  = src/Infrastructure/FieldComm/examples


EXAMPLES_BUILD  = $(ESMF_EXDIR)/ESMF_FieldCommEx \
		  $(ESMF_EXDIR)/ESMF_FieldRedistEx \
		  $(ESMF_EXDIR)/ESMF_FieldSMMEx


EXAMPLES_RUN = run_ESMF_FieldCommEx \
	       run_ESMF_FieldRedistEx \
	       run_ESMF_FieldSMMEx
               
EXAMPLES_RUN_UNI = 
                   


include ${ESMF_DIR}/makefile

DIRS = 


CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS = 

#
# ESMF_FieldCommEx
#
run_ESMF_FieldCommEx:
	$(MAKE) EXNAME=FieldComm NP=4 exfrun


#
# ESMF_FieldRedistEx
#
run_ESMF_FieldRedistEx:
	$(MAKE) EXNAME=FieldRedist NP=4 exfrun


#
# ESMF_FieldSMMEx
#
run_ESMF_FieldSMMEx:
	$(MAKE) EXNAME=FieldSMM NP=4 exfrun


