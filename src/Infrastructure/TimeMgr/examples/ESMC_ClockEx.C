// $Id: ESMC_ClockEx.C,v 1.3 2003/04/25 09:17:16 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
//
//-----------------------------------------------------------------------------
//BOP
// !PROGRAM:  ESMC_ClockEx - Clock initialization and time-stepping
//
// !DESCRIPTION:
//
// This program shows an example of how to set-up a clock
//EOP
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <ESMC.h>
 #include <iostream.h>

 // associated class definition file
 #include <ESMC_Clock.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_ClockEx.C,v 1.3 2003/04/25 09:17:16 eschwab Exp $";
//-----------------------------------------------------------------------------

 int main(int argc, char *argv[])
 {
   // result code
   int rc;

   // instantiate a clock
   ESMC_Clock clock;

   // instantiate a calendar
   ESMC_Calendar gregorianCalendar;

   // instantiate timestep, start and stop times
   ESMC_TimeInterval timeStep;
   ESMC_Time startTime;
   ESMC_Time stopTime;

   // initialize calendar to be Gregorian type
   rc = gregorianCalendar.ESMC_CalendarInit(ESMC_CAL_GREGORIAN);

   // initialize time interval to 1 hour
   int H = 1;
   rc = timeStep.ESMC_TimeIntervalInit(0, 0, 0, &H, 0, 0, 0, 0, 0,
                                       0, 0, 0, 0, 0, 0, 0, 0, 0);

   // initialize start time to 3/27/2003
   ESMF_IKIND_I8 YR = 2003;
   int  MM = 3, DD = 27;
   rc = startTime.ESMC_TimeInit(&YR, &MM, &DD, 0, 0, 0, 0, 0, 0, 0, 
                                0, 0, 0, 0, 0, 0, 0, 0, 0,
                                &gregorianCalendar, 0);

   // initialize stop time to 3/29/2003
   YR = 2003; MM = 3; DD = 29;
   rc = stopTime.ESMC_TimeInit(&YR, &MM, &DD, 0, 0, 0, 0, 0, 0, 0, 
                               0, 0, 0, 0, 0, 0, 0, 0, 0,
                               &gregorianCalendar, 0);

   // initialize the clock with the above values
   rc = clock.ESMC_ClockInit(&timeStep, &startTime, &stopTime, 0);

   // time step from start time to stop time
   while (!clock.ESMC_ClockIsStopTime(&rc)) {
     rc = clock.ESMC_ClockAdvance(0, 0);
   }

   // get the number of times the clock was advanced
   ESMF_IKIND_I8 advanceCount;
   rc = clock.ESMC_ClockGetAdvanceCount(&advanceCount);

   cout << "The clock was advanced " << advanceCount << " times." << endl;

   return(ESMF_SUCCESS);

 } // end ESMC_ClockEx
