// $Id: ESMC_ClockUTest.C,v 1.7.2.3 2009/01/21 21:25:24 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//-----------------------------------------------------------------------------
//BOP
// !PROGRAM:  ESMF_ClockTest - one line general statement about this test 
//
// !DESCRIPTION:
//
// The code in this file drives C++ Clock unit tests.
// The companion files ESMC\_Clock.h and ESMC\_Clock.C contain
// the declarations and definitions for the Clock methods.
//
// 
//
//EOP
//-----------------------------------------------------------------------------
//
// insert any higher level, 3rd party or system includes here
#include <stdio.h>
#include "ESMCI.h"

// associated class definition file
#include "ESMC_Clock.h"

// ESMC_Test function
#include "ESMCI_Test.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_ClockUTest.C,v 1.7.2.3 2009/01/21 21:25:24 cdeluca Exp $";
//-----------------------------------------------------------------------------

 int main(int argc, char *argv[])
 {
   // cumulative result: count failures; no failures equals "all pass"
   int result = 0;

   // individual test result code
   int rc;

   // individual test name
   char name[ESMF_MAXSTR];

   // individual test failure message
   char failMsg[ESMF_MAXSTR];

   // for dynamically allocated Clock's
   ESMC_Clock *clock_ptr;

   // for statically allocated Clock's
   //  tests default constructor; add args to test other constructors
   ESMC_Clock clock;

#ifdef ESMF_TESTEXHAUSTIVE
#if 0
   // perform exhaustive tests here;
   //   see #else below for non-exhaustive tests
   // future release will use run-time switching mechanism

   // test dynamic allocation of ESMC_Clock
   //   also tests default constructor
   clock_ptr = ESMC_ClockCreate(args, &rc);
   sprintf(name, "ESMC_ClockCreate"); 
   sprintf(failMsg, "rc = %d, clock_ptr = %p, args = %f",
           rc, clock_ptr, args);
   ESMC_Test((clock_ptr!=0 && rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);
    
   // test internal dynamic allocation within statically allocated
   //   ESMC_Clock
   rc = clock_ptr->ESMC_ClockConstruct(args);
   sprintf(name, "ESMC_ClockConstruct"); 
   sprintf(failMsg, "rc = %d, args = %f", rc, args);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test initialization of members of statically allocated ESMC_Clock
   //   may want to read back values via Get methods for comparison
   rc = clock_ptr->ESMC_ClockSet(args);
   sprintf(name, "ESMC_ClockSet"); 
   sprintf(failMsg, "rc = %d, args = %f", rc, args);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test setting of configuration values
   ESMC_ClockConfig config_set;
   rc = clock_ptr->ESMC_ClockSetConfig(config_set);
   sprintf(name, "ESMC_ClockSetConfig"); 
   sprintf(failMsg, "rc = %d, config_set = %f", rc, config_set);
   ESMC_Test((rc==ESMF_SUCCESS), 
              name, failMsg, &result, ESMF_SRCLINE);

   // test getting of configuration values,
   //  compare to values set previously
   ESMC_ClockConfig config_get;
   rc = clock_ptr->ESMC_ClockGetConfig(&config_get);
   sprintf(name, "ESMC_ClockGetConfig"); 
   sprintf(failMsg, "rc = %d, config_get = %f", rc, config_get);
   ESMC_Test((rc==ESMF_SUCCESS && config_get == config_set),
              name, failMsg, &result, ESMF_SRCLINE);

   // test setting of ESMC_Clock members values
   <value type> value_set;
   rc = clock_ptr->ESMC_ClockSet<Value>(value_set);
   sprintf(name, "ESMC_ClockSet<Value>"); 
   sprintf(failMsg, "rc = %d, value_set = %f", rc, value_set);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test getting of ESMC_Clock members values,
   //   compare to values set previously
   <value type> value_get;
   rc = clock_ptr->ESMC_ClockGet<Value>(&value_get);
   sprintf(name, "ESMC_ClockGet<Value>"); 
   sprintf(failMsg, "rc = %d, value_get = %f", rc, value_get);
   ESMC_Test((rc==ESMF_SUCCESS && value_get == value_set),
              name, failMsg, &result, ESMF_SRCLINE);
    
   // test validate method via option string
   char validate_options[ESMF_MAXSTR];
   rc = clock_ptr->ESMC_ClockValidate(validate_options);
   sprintf(name, "ESMC_ClockValidate"); 
   sprintf(failMsg, "rc = %d, validate_options = %s", rc, validate_options);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test print method via option string
   char print_options[ESMF_MAXSTR];
   rc = clock_ptr->ESMC_ClockPrint(print_options);
   sprintf(name, "ESMC_ClockPrint"); 
   sprintf(failMsg, "rc = %d, print_options = %s", rc, print_options);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test internal dynamic deallocation within statically allocated 
   //   ESMC_Clock
   rc = clock_ptr->ESMC_ClockDestruct();
   sprintf(name, "ESMC_ClockDestruct"); 
   sprintf(failMsg, "rc = %d", rc);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test dynamic deallocation of ESMC_Clock
   //   also tests destructor
   rc = ESMC_ClockDestroy(clock_ptr);
   sprintf(name, "ESMC_ClockDestroy"); 
   sprintf(failMsg, "rc = %d, clock_ptr = %p", rc, clock_ptr);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

#endif
#else

   // perform non-exhaustive tests here;
   //   use same templates as above

#endif

   // return number of failures to environment; 0 = success (all pass)
   return(result);
  
 } // end unit test main()
