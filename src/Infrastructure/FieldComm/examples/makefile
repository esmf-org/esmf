# $Id: makefile,v 1.12 2006/01/30 20:10:08 nscollins Exp $

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
		  $(ESMF_EXDIR)/ESMF_FieldRedistEx


EXAMPLES_RUN = run_ESMF_FieldCommEx \
	       run_ESMF_FieldRedistEx
               
EXAMPLES_RUN_UNI = run_ESMF_FieldCommEx_uni \
		   run_ESMF_FieldRedistEx_uni
                   


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
	$(MAKE) EXNAME=FieldComm NP=1 exfrun

#
# ESMF_FieldRedistEx
#
run_ESMF_FieldRedistEx:
	$(MAKE) EXNAME=FieldRedist NP=6 exfrun

run_ESMF_FieldRedistEx_uni:
	echo "PASS: ESMF_FieldRedistEx not set up to run on one processor." > $(ESMF_EXDIR)/ESMF_FieldRedistEx.stdout

