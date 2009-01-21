// $Id: ESMC_RouteUTest.C,v 1.6.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
// !PROGRAM:  ESMF_RouteTest - one line general statement about this test 
//
// !DESCRIPTION:
//
// The code in this file drives C++ Route unit tests.
// The companion files ESMC\_Route.h and ESMC\_Route.C contain
// the declarations and definitions for the Route methods.
//
// 
//
//EOP
//-----------------------------------------------------------------------------
//
// insert any higher level, 3rd party or system includes here
#include <stdio.h>
#include "ESMCI.h"

// ESMC_Test function
#include "ESMCI_Test.h"

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_RouteUTest.C,v 1.6.2.2 2009/01/21 21:25:23 cdeluca Exp $";
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

   // for dynamically allocated Route's
   ESMC_Route *route_ptr;

   // for statically allocated Route's
   //  tests default constructor; add args to test other constructors
   ESMC_Route route;

   // get the global VM
   ESMC_VM *vm = ESMC_VMGetGlobal(&rc);

   // test dynamic allocation of ESMC_Route
   //   also tests default constructor
   route_ptr = ESMC_RouteCreate(vm, &rc);
   sprintf(failMsg, "rc = %d, route_ptr = %p", rc, route_ptr);
   ESMC_Test((route_ptr!=0 && rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);
    
   // test internal dynamic allocation within statically allocated
   //   ESMC_Route
   rc = route_ptr->ESMC_RouteConstruct(vm);
   sprintf(failMsg, "rc = %d", rc);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test setting of configuration values
   //ESMC_RouteConfig config_set;
   //rc = route_ptr->ESMC_RouteSetConfig(config_set);
   //sprintf(failMsg, "rc = %d, config_set = %f", rc, config_set);
   //ESMC_Test((rc==ESMF_SUCCESS), 
   //           name, failMsg, &result, ESMF_SRCLINE);

   // test getting of configuration values,
   //  compare to values set previously
   //ESMC_RouteConfig config_get;
   //rc = route_ptr->ESMC_RouteGetConfig(&config_get);
   //sprintf(failMsg, "rc = %d, config_get = %f", rc, config_get);
   //ESMC_Test((rc==ESMF_SUCCESS && config_get == config_set),
   //           name, failMsg, &result, ESMF_SRCLINE);

   // test setting of ESMC_Route members values
   int value_set; //<value type> value_set;
   //rc = route_ptr->ESMC_RouteSet<Value>(value_set);
   //sprintf(failMsg, "rc = %d, value_set = %f", rc, value_set);
   //ESMC_Test((rc==ESMF_SUCCESS),
   //           name, failMsg, &result, ESMF_SRCLINE);

   // test getting of ESMC_Route members values,
   //   compare to values set previously
   int value_get; //<value type> value_get;
   //rc = route_ptr->ESMC_RouteGet<Value>(&value_get);
   //sprintf(failMsg, "rc = %d, value_get = %f", rc, value_get);
   //ESMC_Test((rc==ESMF_SUCCESS && value_get == value_set),
   //           name, failMsg, &result, ESMF_SRCLINE);
    
   // test validate method via option string
   char validate_options[ESMF_MAXSTR];
   rc = route_ptr->ESMC_RouteValidate(validate_options);
   sprintf(failMsg, "rc = %d, validate_options = %s", rc, validate_options);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test print method via option string
   char print_options[ESMF_MAXSTR];
   rc = route_ptr->ESMC_RoutePrint(print_options);
   sprintf(failMsg, "rc = %d, print_options = %s", rc, print_options);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test internal dynamic deallocation within statically allocated 
   //   ESMC_Route
   rc = route_ptr->ESMC_RouteDestruct();
   sprintf(failMsg, "rc = %d", rc);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test dynamic deallocation of ESMC_Route
   //   also tests destructor
   rc = ESMC_RouteDestroy(route_ptr);
   sprintf(failMsg, "rc = %d, route_ptr = %p", rc, route_ptr);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // return number of failures to environment; 0 = success (all pass)
   return(result);
  
 } // end unit test main()
