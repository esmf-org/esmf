// $Id: ESMC_Alarm_F.C,v 1.10 2003/06/11 06:58:43 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
//
//==============================================================================
//
//==============================================================================
//
// This file contains the Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include "ESMC.h"
#include "ESMC_Alarm.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The code in this file implements the inter-language code which
//  allows F90 to call C++ for supporting {\tt ESMC\_Alarm} class functions.
//
//EOP


// the interface subroutine names MUST be in lower case
extern "C" {

       void FTN(c_esmc_alarmset)(ESMC_Alarm *ptr,
                ESMC_Time *ringTime, ESMC_TimeInterval *ringInterval,
                ESMC_Time *stopTime, int *enabled, int *status) {
           *status = (ptr)->ESMC_AlarmSet(ringTime, ringInterval, stopTime,
                                          *enabled);
       }

       void FTN(c_esmc_alarmenable)(ESMC_Alarm *ptr, int *status) {
           *status = (ptr)->ESMC_AlarmEnable();
       }

       void FTN(c_esmc_alarmdisable)(ESMC_Alarm *ptr, int *status) {
           *status = (ptr)->ESMC_AlarmDisable();
       }

       void FTN(c_esmc_alarmturnon)(ESMC_Alarm *ptr, int *status) {
           *status = (ptr)->ESMC_AlarmTurnOn();
       }

       void FTN(c_esmc_alarmturnoff)(ESMC_Alarm *ptr, int *status) {
           *status = (ptr)->ESMC_AlarmTurnOff();
       }

       void FTN(c_esmc_alarmisringing)(ESMC_Alarm *ptr, 
                int *esmf_alarmIsRinging, int *status) {
           *esmf_alarmIsRinging = (int) (ptr)->ESMC_AlarmIsRinging(status);
       }

       void FTN(c_esmc_alarmcheckringtime)(ESMC_Alarm *ptr,
                int *esmf_alarmCheckRingTime, ESMC_Time *clockCurrTime, 
                int *positive, int *status) {
           *esmf_alarmCheckRingTime =
                (int) (ptr)->ESMC_AlarmCheckRingTime(clockCurrTime, *positive,
                                                     status);
       }

       void FTN(c_esmc_alarmgetringinterval)(ESMC_Alarm *ptr, 
                                             ESMC_TimeInterval *ringInterval,
                                             int *status) {
           *status = (ptr)->ESMC_AlarmGetRingInterval(ringInterval);
       }

       void FTN(c_esmc_alarmsetringinterval)(ESMC_Alarm *ptr, 
                                             ESMC_TimeInterval *ringInterval,
                                             int *status) {
           *status = (ptr)->ESMC_AlarmSetRingInterval(ringInterval);
       }

       void FTN(c_esmc_alarmgetringtime)(ESMC_Alarm *ptr, 
                                         ESMC_Time *ringTime,
                                         int *status) {
           *status = (ptr)->ESMC_AlarmGetRingTime(ringTime);
       }

       void FTN(c_esmc_alarmsetringtime)(ESMC_Alarm *ptr, 
                                         ESMC_Time *ringTime,
                                         int *status) {
           *status = (ptr)->ESMC_AlarmSetRingTime(ringTime);
       }

       void FTN(c_esmc_alarmgetprevringtime)(ESMC_Alarm *ptr, 
                                             ESMC_Time *prevRingTime,
                                             int *status) {
           *status = (ptr)->ESMC_AlarmGetPrevRingTime(prevRingTime);
       }

       void FTN(c_esmc_alarmsetprevringtime)(ESMC_Alarm *ptr, 
                                             ESMC_Time *prevRingTime,
                                             int *status) {
           *status = (ptr)->ESMC_AlarmSetPrevRingTime(prevRingTime);
       }

       void FTN(c_esmc_alarmgetstoptime)(ESMC_Alarm *ptr, 
                                         ESMC_Time *stopTime,
                                         int *status) {
           *status = (ptr)->ESMC_AlarmGetStopTime(stopTime);
       }

       void FTN(c_esmc_alarmsetstoptime)(ESMC_Alarm *ptr, 
                                         ESMC_Time *stopTime,
                                         int *status) {
           *status = (ptr)->ESMC_AlarmSetStopTime(stopTime);
       }

       void FTN(c_esmc_alarmeq)(ESMC_Alarm *alarm1, ESMC_Alarm *alarm2,
                                int *esmf_alarmEQ) {
           *esmf_alarmEQ = (int) (*alarm1 == *alarm2);
       }

       void FTN(c_esmc_alarmread)(ESMC_Alarm *ptr,
                                  ESMC_TimeInterval *ringInterval,
                                  ESMC_Time *ringTime,
                                  ESMC_Time *prevRingTime,
                                  ESMC_Time *stopTime,
                                  bool *ringing,
                                  bool *enabled,
                                  int  *id,
                                  int  *status) {
           *status = (ptr)->ESMC_AlarmRead(ringInterval, ringTime,
                                           prevRingTime, stopTime,
                                           *ringing, *enabled, *id);
       }

       void FTN(c_esmc_alarmwrite)(ESMC_Alarm *ptr,
                                   ESMC_TimeInterval *ringInterval,
                                   ESMC_Time *ringTime,
                                   ESMC_Time *prevRingTime,
                                   ESMC_Time *stopTime,
                                   bool *ringing,
                                   bool *enabled,
                                   int  *id,
                                   int  *status) {
           *status = (ptr)->ESMC_AlarmWrite(ringInterval, ringTime,
                                            prevRingTime, stopTime,
                                            ringing, enabled, id);
       }

       void FTN(c_esmc_alarmvalidate)(ESMC_Alarm *ptr, const char *opts,
                                      int *status) {
           *status = (ptr)->ESMC_AlarmValidate(opts);
       }

       void FTN(c_esmc_alarmprint)(ESMC_Alarm *ptr, const char *opts,
                                   int *status) {
           *status = (ptr)->ESMC_AlarmPrint(opts);
       }
};
