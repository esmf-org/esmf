# $Id: makefile,v 1.4 2003/01/13 21:08:50 eschwab Exp $

All: 
	do_fprotex Time ../interface/ESMF_Time.F90
	do_fprotex TimeInterval ../interface/ESMF_TimeInterval.F90
	do_fprotex TimeInstant ../interface/ESMF_TimeInstant.F90
	do_fprotex Calendar ../interface/ESMF_Calendar.F90
	do_fprotex Clock ../interface/ESMF_Clock.F90
	do_fprotex Alarm ../interface/ESMF_Alarm.F90
	do_ccprotex Time ../include/ESMC_Time.h
	do_ccprotex TimeInterval ../include/ESMC_TimeInterval.h
	do_ccprotex TimeInstant ../include/ESMC_TimeInstant.h
	do_ccprotex Calendar ../include/ESMC_Calendar.h
	do_ccprotex Clock ../include/ESMC_Clock.h
	do_ccprotex Alarm ../include/ESMC_Alarm.h
	do_latex TimeMgr des
	do_l2h TimeMgr des
	dvipdf TimeMgr_desdoc.dvi
