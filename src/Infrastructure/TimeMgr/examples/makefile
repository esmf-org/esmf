# $Id: makefile,v 1.14 2003/09/17 20:45:53 flanigan Exp $

ALL:  examples

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
		    $(ESMF_EXDIR)/ESMC_ClockEx \
                    $(ESMF_EXDIR)/ESMF_ClockAdvEx

EXAMPLES_RUN     =  run_ESMF_TimeEx \
                    run_ESMF_TimeIntervalEx \
                    run_ESMF_CalendarEx \
                    run_ESMF_ClockEx \
		    run_ESMC_ClockEx \
                    run_ESMF_ClockAdvEx

EXAMPLES_RUN_UNI =  run_ESMF_TimeEx_uni \
                    run_ESMF_TimeIntervalEx_uni \
                    run_ESMF_CalendarEx_uni \
                    run_ESMF_ClockEx_uni \
		    run_ESMC_ClockEx_uni \
                    run_ESMF_ClockAdvEx_uni

include ${ESMF_DIR}/makefile

DIRS = 

CLEANDIRS   = coredir.*
CLEANFILES  = *.o *.mod *.txt *.stdout NULL core $(EXAMPLES_BUILD)
CLOBBERDIRS =

#
#  ESMF_TimeEx
#
run_ESMF_TimeEx:
	-$(ESMF_EXDIR)/ESMF_TimeEx > $(ESMF_EXDIR)/ESMF_TimeEx.stdout


#
# ESMF_TimeIntervalEx
#
run_ESMF_TimeIntervalEx:
	-$(ESMF_EXDIR)/ESMF_TimeIntervalEx > $(ESMF_EXDIR)/ESMF_TimeIntervalEx.stdout


#
# ESMF_CalendarEx
#
run_ESMF_CalendarEx:
	-$(ESMF_EXDIR)/ESMF_CalendarEx > $(ESMF_EXDIR)/ESMF_CalendarEx.stdout


#
# ESMF_ClockEx
#
run_ESMF_ClockEx:
	-$(ESMF_EXDIR)/ESMF_ClockEx > $(ESMF_EXDIR)/ESMF_ClockEx.stdout

#
# ESMC_ClockEx
#
run_ESMC_ClockEx:
	-$(ESMF_EXDIR)/ESMC_ClockEx > $(ESMF_EXDIR)/ESMC_ClockEx.stdout

#
# ESMF_ClockAdvEx
#
run_ESMF_ClockAdvEx:
	-$(ESMF_EXDIR)/ESMF_ClockAdvEx > $(ESMF_EXDIR)/ESMF_ClockAdvEx.stdout


