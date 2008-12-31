# $Id: makefile,v 1.27.2.1 2008/12/31 17:41:18 theurich Exp $

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
LOCDIR	  = src/Infrastructure/TimeMgr/examples

EXAMPLES_BUILD   =  $(ESMF_EXDIR)/ESMF_TimeEx \
                    $(ESMF_EXDIR)/ESMF_TimeIntervalEx \
                    $(ESMF_EXDIR)/ESMF_CalendarEx \
                    $(ESMF_EXDIR)/ESMF_ClockEx \
                    $(ESMF_EXDIR)/ESMF_AlarmEx
#		    $(ESMF_EXDIR)/ESMC_ClockEx


EXAMPLES_RUN     =  run_ESMF_TimeEx \
                    run_ESMF_TimeIntervalEx \
                    run_ESMF_CalendarEx \
                    run_ESMF_ClockEx \
                    run_ESMF_AlarmEx
#		    run_ESMC_ClockEx

EXAMPLES_RUN_UNI =  run_ESMF_TimeEx_uni \
                    run_ESMF_TimeIntervalEx_uni \
                    run_ESMF_CalendarEx_uni \
                    run_ESMF_ClockEx_uni \
                    run_ESMF_AlarmEx_uni
#		    run_ESMC_ClockEx_uni

include ${ESMF_DIR}/makefile

DIRS = 

CLEANDIRS   = 
CLEANFILES  = $(EXAMPLES_BUILD)
CLOBBERDIRS =

#
#  ESMF_TimeEx
#
run_ESMF_TimeEx:
	$(MAKE) EXNAME=Time NP=1 exfrun

run_ESMF_TimeEx_uni:
	$(MAKE) EXNAME=Time NP=1 exfrun

#
# ESMF_TimeIntervalEx
#
run_ESMF_TimeIntervalEx:
	$(MAKE) EXNAME=TimeInterval NP=1 exfrun

run_ESMF_TimeIntervalEx_uni:
	$(MAKE) EXNAME=TimeInterval NP=1 exfrun

#
# ESMF_CalendarEx
#
run_ESMF_CalendarEx:
	$(MAKE) EXNAME=Calendar NP=1 exfrun

run_ESMF_CalendarEx_uni:
	$(MAKE) EXNAME=Calendar NP=1 exfrun

#
# ESMF_ClockEx
#
run_ESMF_ClockEx:
	$(MAKE) EXNAME=Clock NP=1 exfrun

run_ESMF_ClockEx_uni:
	$(MAKE) EXNAME=Clock NP=1 exfrun

#
# ESMC_ClockEx
#
run_ESMC_ClockEx:
	$(MAKE) EXNAME=Clock NP=1 excrun

run_ESMC_ClockEx_uni:
	$(MAKE) EXNAME=Clock NP=1 excrun

#
# ESMF_AlarmEx
#
run_ESMF_AlarmEx:
	$(MAKE) EXNAME=Alarm NP=1 exfrun

run_ESMF_AlarmEx_uni:
	$(MAKE) EXNAME=Alarm NP=1 exfrun
