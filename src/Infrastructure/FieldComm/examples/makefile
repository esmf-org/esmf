# $Id: makefile,v 1.18 2010/04/16 16:25:51 feiliu Exp $

ALL:  build_examples

run: run_examples

LOCDIR	  = src/Infrastructure/FieldComm/examples


EXAMPLES_BUILD  = $(ESMF_EXDIR)/ESMF_FieldCommEx \
		  $(ESMF_EXDIR)/ESMF_FieldRedistEx \
		  $(ESMF_EXDIR)/ESMF_FieldSMMEx \
		  $(ESMF_EXDIR)/ESMF_FieldHaloEx


EXAMPLES_RUN = run_ESMF_FieldCommEx \
	       run_ESMF_FieldRedistEx \
	       run_ESMF_FieldSMMEx \
	       run_ESMF_FieldHaloEx
               
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


#
# ESMF_FieldHaloEx
#
run_ESMF_FieldHaloEx:
	$(MAKE) EXNAME=FieldHalo NP=4 exfrun


