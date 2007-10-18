// $Id: ESMC_FieldDataMapUTest.C,v 1.2.8.3 2007/10/18 02:42:48 cdeluca Exp $
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
// !PROGRAM:  ESMF_FieldDataMapUTest - one line general statement about this test 
//
// !DESCRIPTION:
//
// The code in this file drives C++ FieldDataMap unit tests.
// The companion files ESMC\_FieldDataMap.h and ESMC\_FieldDataMap.C contain
// the declarations and definitions for the FieldDataMap methods.
//
// 
//
//EOP
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <stdio.h>
 #include <ESMC.h>

 // associated class definition file
 #include <ESMC_FieldDataMap.h>

 // ESMC_Test function
 #include <ESMC_Test.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_FieldDataMapUTest.C,v 1.2.8.3 2007/10/18 02:42:48 cdeluca Exp $";
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

   // for dynamically allocated FieldDataMap's
   ESMC_FieldDataMap *fielddatamap_ptr;

   // for statically allocated FieldDataMap's
   //  tests default constructor; add args to test other constructors
   ESMC_FieldDataMap fielddatamap;

#ifdef ESMF_EXHAUSTIVE

   // perform exhaustive tests here;
   //   see #else below for non-exhaustive tests
   // future release will use run-time switching mechanism

   // for deep classes, keep Create/Construct and remove Init.
   // for shallow classes, keep Init and remove Create/Construct

   // test dynamic allocation of ESMC_FieldDataMap
   //   also tests default constructor
   fielddatamap_ptr = ESMC_FieldDataMapCreate(args, &rc);
   sprintf(name, "ESMC_FieldDataMapCreate"); 
   sprintf(failMsg, "rc = %d, fielddatamap_ptr = %p, args = %f",
           rc, fielddatamap_ptr, args);
   ESMC_Test((fielddatamap_ptr!=0 && rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);
    
   // test internal dynamic allocation within statically allocated
   //   ESMC_FieldDataMap
   rc = fielddatamap_ptr->ESMC_FieldDataMapConstruct(args);
   sprintf(name, "ESMC_FieldDataMapConstruct"); 
   sprintf(failMsg, "rc = %d, args = %f", rc, args);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test initialization of members of statically allocated ESMC_FieldDataMap
   //   may want to read back values via Get methods for comparison
   rc = fielddatamap_ptr->ESMC_FieldDataMapSetDefault(args);
   sprintf(name, "ESMC_FieldDataMapSetDefault"); 
   sprintf(failMsg, "rc = %d, args = %f", rc, args);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test setting of configuration values
   ESMC_FieldDataMapConfig config_set;
   rc = fielddatamap_ptr->ESMC_FieldDataMapSetConfig(config_set);
   sprintf(name, "ESMC_FieldDataMapSetConfig"); 
   sprintf(failMsg, "rc = %d, config_set = %f", rc, config_set);
   ESMC_Test((rc==ESMF_SUCCESS), 
              name, failMsg, &result, ESMF_SRCLINE);

   // test getting of configuration values,
   //  compare to values set previously
   ESMC_FieldDataMapConfig config_get;
   rc = fielddatamap_ptr->ESMC_FieldDataMapGetConfig(&config_get);
   sprintf(name, "ESMC_FieldDataMapGetConfig"); 
   sprintf(failMsg, "rc = %d, config_get = %f", rc, config_get);
   ESMC_Test((rc==ESMF_SUCCESS && config_get == config_set),
              name, failMsg, &result, ESMF_SRCLINE);

   // test setting of ESMC_FieldDataMap members values
   // make <value type> below the appropriate type
   int value_set; //<value type> value_set;
   //rc = fielddatamap_ptr->ESMC_FieldDataMapSet<Value>(value_set);
   sprintf(name, "ESMC_FieldDataMapSet<Value>"); 
   sprintf(failMsg, "rc = %d, value_set = %f", rc, value_set);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test getting of ESMC_FieldDataMap members values,
   //   compare to values set previously
   // make <value type> below the appropriate type
   int value_set; //<value type> value_get;
   //rc = fielddatamap_ptr->ESMC_FieldDataMapGet<Value>(&value_get);
   sprintf(name, "ESMC_FieldDataMapGet<Value>"); 
   sprintf(failMsg, "rc = %d, value_get = %f", rc, value_get);
   ESMC_Test((rc==ESMF_SUCCESS && value_get == value_set),
              name, failMsg, &result, ESMF_SRCLINE);
    
   // test validate method via option string
   char validate_options[ESMF_MAXSTR];
   rc = fielddatamap_ptr->ESMC_FieldDataMapValidate(validate_options);
   sprintf(name, "ESMC_FieldDataMapValidate"); 
   sprintf(failMsg, "rc = %d, validate_options = %s", rc, validate_options);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test print method via option string
   char print_options[ESMF_MAXSTR];
   rc = fielddatamap_ptr->ESMC_FieldDataMapPrint(print_options);
   sprintf(name, "ESMC_FieldDataMapPrint"); 
   sprintf(failMsg, "rc = %d, print_options = %s", rc, print_options);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // for shallow classes, remove these next two tests since Destructors
   // are not needed for them.

   // test internal dynamic deallocation within statically allocated 
   //   ESMC_FieldDataMap
   rc = fielddatamap_ptr->ESMC_FieldDataMapDestruct();
   sprintf(name, "ESMC_FieldDataMapDestruct"); 
   sprintf(failMsg, "rc = %d", rc);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test dynamic deallocation of ESMC_FieldDataMap
   //   also tests destructor
   rc = ESMC_FieldDataMapDestroy(fielddatamap_ptr);
   sprintf(name, "ESMC_FieldDataMapDestroy"); 
   sprintf(failMsg, "rc = %d, fielddatamap_ptr = %p", rc, fielddatamap_ptr);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

#else

   // perform non-exhaustive tests here;
   //   use same templates as above

#endif

   // return number of failures to environment; 0 = success (all pass)
   return(result);
  
 } // end unit test main()
