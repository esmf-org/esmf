// $Id: ESMC_ClockEx.C,v 1.16 2004/06/18 20:39:38 eschwab Exp $
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
 static const char *const version = "$Id: ESMC_ClockEx.C,v 1.16 2004/06/18 20:39:38 eschwab Exp $";
//-----------------------------------------------------------------------------

 int main(int argc, char *argv[])
 {
   // result code
   int rc;

   //Set finalrc to success
   int finalrc=ESMF_SUCCESS;

   // instantiate a clock
   ESMC_Clock *clock;

   // instantiate a calendar
   ESMC_Calendar *gregorianCalendar;

   // instantiate timestep, start and stop times
   ESMC_TimeInterval timeStep;
   ESMC_Time startTime;
   ESMC_Time stopTime;

   // initialize calendar to be Gregorian type
   gregorianCalendar = ESMC_CalendarCreate(9, "Gregorian", 
                                           ESMC_CAL_GREGORIAN, &rc);

   if (rc != ESMF_SUCCESS) {
       finalrc = ESMF_FAILURE;
   }

   // initialize time interval to 1 hour
   int h = 1;
   rc = timeStep.ESMC_TimeIntervalSet(0, 0, 0, 0, 0, 0, &h);

   if (rc != ESMF_SUCCESS) {
       finalrc = ESMF_FAILURE;
   }

   // initialize start time to 3/27/2003
   int yy = 2003;
   int mm = 3, dd = 27;
   rc = startTime.ESMC_TimeSet(&yy, 0, &mm, &dd, 0, 0, 0, 0, 0, 0, 0, 0, 
                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                               &gregorianCalendar);

   if (rc != ESMF_SUCCESS) {
       finalrc = ESMF_FAILURE;
   }


   // initialize stop time to 3/29/2003
   yy = 2003; mm = 3; dd = 29;
   rc = stopTime.ESMC_TimeSet(&yy, 0, &mm, &dd, 0, 0, 0, 0, 0, 0, 0, 0, 
                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                              &gregorianCalendar);

   if (rc != ESMF_SUCCESS) {
       finalrc = ESMF_FAILURE;
   }

   // initialize the clock with the above values
   clock = ESMC_ClockCreate(7, "Clock 1", &timeStep, &startTime, &stopTime, 
                            0, 0, 0, &rc);

   if (rc != ESMF_SUCCESS) {
       finalrc = ESMF_FAILURE;
   }

   // time step from start time to stop time
   while (!clock->ESMC_ClockIsStopTime(&rc)) {
     rc = clock->ESMC_ClockAdvance();

     if (rc != ESMF_SUCCESS) {
         finalrc = ESMF_FAILURE;
     }

   }

   // get the number of times the clock was advanced
   ESMF_KIND_I8 advanceCount;
   rc = clock->ESMC_ClockGet(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                             0, 0, 0, &advanceCount);

   if (rc != ESMF_SUCCESS) {
       finalrc = ESMF_FAILURE;
   }

   cout << "The clock was advanced " << advanceCount << " times." << endl;
   
   // Test for the correct number of advance counts.
   if (advanceCount != 48) {
       finalrc = ESMF_FAILURE;
   }

   rc = ESMC_ClockDestroy(&clock);

   if (rc != ESMF_SUCCESS) {
       finalrc = ESMF_FAILURE;
   }

   rc = ESMC_CalendarDestroy(&gregorianCalendar);

   if (rc != ESMF_SUCCESS) {
       finalrc = ESMF_FAILURE;
   }

   if (finalrc == ESMF_SUCCESS) {
        cout << "PASS: ESMC_ClockEx.C" << endl;
        return(ESMF_SUCCESS);
   } 
   else {
        cout << "FAIL: ESMC_ClockEx.C" << endl;
        return(ESMF_FAILURE);
   }

 } // end ESMC_ClockEx
