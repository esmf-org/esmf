// $Id: ESMC_RegridUTest.C,v 1.2.8.3 2007/10/18 02:43:10 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//-----------------------------------------------------------------------------
//BOP
// !PROGRAM:  ESMF_RegridTest - one line general statement about this test 
//
// !DESCRIPTION:
//
// The code in this file drives C++ Regrid unit tests.
// The companion files ESMC\_Regrid.h and ESMC\_Regrid.C contain
// the declarations and definitions for the Regrid methods.
//
// 
//
//EOP
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <stdio.h>
 #include <ESMC.h>

 // ESMC_Test function
 #include <ESMC_Test.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_RegridUTest.C,v 1.2.8.3 2007/10/18 02:43:10 cdeluca Exp $";
//-----------------------------------------------------------------------------

 int main(int argc, char *argv[])
 {
   // cumulative result: count failures; no failures equals "all pass"
   int result = 0;

   // individual test result code
   int rc;

   // individual test failure message
   char failMsg[ESMF_MAXSTR];

   // for dynamically allocated Regrid's
   ESMC_Regrid *route_ptr;

   // for statically allocated Regrid's
   //  tests default constructor; add args to test other constructors
   ESMC_Regrid route;

   // test dynamic allocation of ESMC_Regrid
   //   also tests default constructor
   route_ptr = ESMC_RegridCreate(args, &rc);
   sprintf(failMsg, "rc = %d, route_ptr = %p, args = %f",
           rc, route_ptr, args);
   ESMC_Test((route_ptr!=0 && rc==ESMF_SUCCESS),
              failMsg, &result, ESMF_SRCLINE);
    
   // test internal dynamic allocation within statically allocated
   //   ESMC_Regrid
   rc = route_ptr->ESMC_RegridConstruct(args);
   sprintf(failMsg, "rc = %d, args = %f", rc, args);
   ESMC_Test((rc==ESMF_SUCCESS),
              failMsg, &result, ESMF_SRCLINE);

   // test setting of configuration values
   ESMC_RegridConfig config_set;
   rc = route_ptr->ESMC_RegridSetConfig(config_set);
   sprintf(failMsg, "rc = %d, config_set = %f", rc, config_set);
   ESMC_Test((rc==ESMF_SUCCESS), 
              failMsg, &result, ESMF_SRCLINE);

   // test getting of configuration values,
   //  compare to values set previously
   ESMC_RegridConfig config_get;
   rc = route_ptr->ESMC_RegridGetConfig(&config_get);
   sprintf(failMsg, "rc = %d, config_get = %f", rc, config_get);
   ESMC_Test((rc==ESMF_SUCCESS && config_get == config_set),
              failMsg, &result, ESMF_SRCLINE);

   // test setting of ESMC_Regrid members values
   //<value type> value_set;
   //rc = route_ptr->ESMC_RegridSet<Value>(value_set);
   //sprintf(failMsg, "rc = %d, value_set = %f", rc, value_set);
   //ESMC_Test((rc==ESMF_SUCCESS),
   //           failMsg, &result, ESMF_SRCLINE);

   // test getting of ESMC_Regrid members values,
   //   compare to values set previously
   //<value type> value_get;
   //rc = route_ptr->ESMC_RegridGet<Value>(&value_get);
   //sprintf(failMsg, "rc = %d, value_get = %f", rc, value_get);
   //ESMC_Test((rc==ESMF_SUCCESS && value_get == value_set),
   //           failMsg, &result, ESMF_SRCLINE);
    
   // test validate method via option string
   char validate_options[ESMF_MAXSTR];
   rc = route_ptr->ESMC_RegridValidate(validate_options);
   sprintf(failMsg, "rc = %d, validate_options = %s", rc, validate_options);
   ESMC_Test((rc==ESMF_SUCCESS),
              failMsg, &result, ESMF_SRCLINE);

   // test print method via option string
   char print_options[ESMF_MAXSTR];
   rc = route_ptr->ESMC_RegridPrint(print_options);
   sprintf(failMsg, "rc = %d, print_options = %s", rc, print_options);
   ESMC_Test((rc==ESMF_SUCCESS),
              failMsg, &result, ESMF_SRCLINE);

   // test internal dynamic deallocation within statically allocated 
   //   ESMC_Regrid
   rc = route_ptr->ESMC_RegridDestruct();
   sprintf(failMsg, "rc = %d", rc);
   ESMC_Test((rc==ESMF_SUCCESS),
              failMsg, &result, ESMF_SRCLINE);

   // test dynamic deallocation of ESMC_Regrid
   //   also tests destructor
   rc = ESMC_RegridDestroy(route_ptr);
   sprintf(failMsg, "rc = %d, route_ptr = %p", rc, route_ptr);
   ESMC_Test((rc==ESMF_SUCCESS),
              failMsg, &result, ESMF_SRCLINE);

   // return number of failures to environment; 0 = success (all pass)
   return(result);
  
 } // end unit test main()
