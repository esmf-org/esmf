# $Id: makefile,v 1.2 2002/08/18 23:22:49 eschwab Exp $

All: 
	do_fprotex time ../ESMF_TimeMod.f90
	do_fprotex time_interval ../ESMF_TimeIntervalMod.f90
	do_fprotex time_intstant ../ESMF_TimeInstantMod.f90
	do_fprotex calendar ../ESMF_CalendarMod.f90
	do_fprotex clock ../ESMF_ClockMod.f90
	do_fprotex alarm ../ESMF_AlarmMod.f90
	do_ccprotex time ../ESMC_Time.h
	do_ccprotex time_interval ../ESMC_TimeInterval.h
	do_ccprotex time_instant ../ESMC_TimeInstant.h
	do_ccprotex calendar ../ESMC_Calendar.h
	do_ccprotex clock ../ESMC_Clock.h
	do_ccprotex alarm ../ESMC_Alarm.h
	do_latex TimeMgr des
	do_l2h TimeMgr des
	dvipdf TimeMgr_desdoc.dvi
