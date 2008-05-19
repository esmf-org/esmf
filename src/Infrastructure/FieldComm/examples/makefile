# $Id: makefile,v 1.15 2008/05/19 20:08:01 feiliu Exp $

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
LOCDIR	  = src/Infrastructure/FieldComm/examples


EXAMPLES_BUILD  = $(ESMF_EXDIR)/ESMF_FieldCommEx \
		  $(ESMF_EXDIR)/ESMF_FieldRedistEx \
		  $(ESMF_EXDIR)/ESMF_FieldSMMEx


EXAMPLES_RUN = run_ESMF_FieldCommEx \
	       run_ESMF_FieldRedistEx \
	       run_ESMF_FieldSMMEx
               
EXAMPLES_RUN_UNI = run_ESMF_FieldCommEx_uni \
		   run_ESMF_FieldRedistEx_uni \
		   run_ESMF_FieldSMMEx_uni
                   


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

run_ESMF_FieldCommEx_uni:
	echo "PASS: ESMF_FieldCommEx not set up to run on one processor." > $(ESMF_EXDIR)/ESMF_FieldCommEx.stdout

#
# ESMF_FieldRedistEx
#
run_ESMF_FieldRedistEx:
	$(MAKE) EXNAME=FieldRedist NP=4 exfrun

run_ESMF_FieldRedistEx_uni:
	echo "PASS: ESMF_FieldRedistEx not set up to run on one processor." > $(ESMF_EXDIR)/ESMF_FieldRedistEx.stdout

#
# ESMF_FieldSMMEx
#
run_ESMF_FieldSMMEx:
	$(MAKE) EXNAME=FieldSMM NP=4 exfrun

run_ESMF_FieldSMMEx_uni:
	echo "PASS: ESMF_FieldSMMEx not set up to run on one processor." > $(ESMF_EXDIR)/ESMF_FieldSMMEx.stdout

