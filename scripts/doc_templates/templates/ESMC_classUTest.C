// $Id: ESMC_classUTest.C,v 1.5.2.3 2010/02/01 20:48:49 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//-----------------------------------------------------------------------------
//BOP
// !PROGRAM:  ESMF_<Class>UTest - one line general statement about this test 
//
// !DESCRIPTION:
//
// The code in this file drives C++ <Class> unit tests.
// The companion files ESMC\_<Class>.h and ESMC\_<Class>.C contain
// the declarations and definitions for the <Class> methods.
//
// 
//
//EOP
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
 #include <stdio.h>
 #include <ESMC_Start.h>

 // associated class definition file
 #include <ESMC_<Class>.h>

 // ESMC_Test function
 #include <ESMC_Test.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_classUTest.C,v 1.5.2.3 2010/02/01 20:48:49 svasquez Exp $";
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

   // for dynamically allocated <Class>'s
   ESMC_<Class> *<class>_ptr;

   // for statically allocated <Class>'s
   //  tests default constructor; add args to test other constructors
   ESMC_<Class> <class>;

#ifdef ESMF_EXHAUSTIVE

   // perform exhaustive tests here;
   //   see #else below for non-exhaustive tests
   // future release will use run-time switching mechanism

   // for deep classes, keep Create/Construct and remove Init.
   // for shallow classes, keep Init and remove Create/Construct

   // test dynamic allocation of ESMC_<Class>
   //   also tests default constructor
   <class>_ptr = ESMC_<Class>Create(args, &rc);
   sprintf(name, "ESMC_<Class>Create"); 
   sprintf(failMsg, "rc = %d, <class>_ptr = %p, args = %f",
           rc, <class>_ptr, args);
   ESMC_Test((<class>_ptr!=0 && rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);
    
   // test internal dynamic allocation within statically allocated
   //   ESMC_<Class>
   rc = <class>_ptr->ESMC_<Class>Construct(args);
   sprintf(name, "ESMC_<Class>Construct"); 
   sprintf(failMsg, "rc = %d, args = %f", rc, args);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test initialization of members of statically allocated ESMC_<Class>
   //   may want to read back values via Get methods for comparison
   rc = <class>_ptr->ESMC_<Class>Init(args);
   sprintf(name, "ESMC_<Class>Init"); 
   sprintf(failMsg, "rc = %d, args = %f", rc, args);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test setting of configuration values
   ESMC_<Class>Config config_set;
   rc = <class>_ptr->ESMC_<Class>SetConfig(config_set);
   sprintf(name, "ESMC_<Class>SetConfig"); 
   sprintf(failMsg, "rc = %d, config_set = %f", rc, config_set);
   ESMC_Test((rc==ESMF_SUCCESS), 
              name, failMsg, &result, ESMF_SRCLINE);

   // test getting of configuration values,
   //  compare to values set previously
   ESMC_<Class>Config config_get;
   rc = <class>_ptr->ESMC_<Class>GetConfig(&config_get);
   sprintf(name, "ESMC_<Class>GetConfig"); 
   sprintf(failMsg, "rc = %d, config_get = %f", rc, config_get);
   ESMC_Test((rc==ESMF_SUCCESS && config_get == config_set),
              name, failMsg, &result, ESMF_SRCLINE);

   // test setting of ESMC_<Class> members values
   // make <value type> below the appropriate type
   int value_set; //<value type> value_set;
   //rc = <class>_ptr->ESMC_<Class>Set<Value>(value_set);
   sprintf(name, "ESMC_<Class>Set<Value>"); 
   sprintf(failMsg, "rc = %d, value_set = %f", rc, value_set);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test getting of ESMC_<Class> members values,
   //   compare to values set previously
   // make <value type> below the appropriate type
   int value_set; //<value type> value_get;
   //rc = <class>_ptr->ESMC_<Class>Get<Value>(&value_get);
   sprintf(name, "ESMC_<Class>Get<Value>"); 
   sprintf(failMsg, "rc = %d, value_get = %f", rc, value_get);
   ESMC_Test((rc==ESMF_SUCCESS && value_get == value_set),
              name, failMsg, &result, ESMF_SRCLINE);
    
   // test validate method via option string
   char validate_options[ESMF_MAXSTR];
   rc = <class>_ptr->ESMC_<Class>Validate(validate_options);
   sprintf(name, "ESMC_<Class>Validate"); 
   sprintf(failMsg, "rc = %d, validate_options = %s", rc, validate_options);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test print method via option string
   char print_options[ESMF_MAXSTR];
   rc = <class>_ptr->ESMC_<Class>Print(print_options);
   sprintf(name, "ESMC_<Class>Print"); 
   sprintf(failMsg, "rc = %d, print_options = %s", rc, print_options);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // for shallow classes, remove these next two tests since Destructors
   // are not needed for them.

   // test internal dynamic deallocation within statically allocated 
   //   ESMC_<Class>
   rc = <class>_ptr->ESMC_<Class>Destruct();
   sprintf(name, "ESMC_<Class>Destruct"); 
   sprintf(failMsg, "rc = %d", rc);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test dynamic deallocation of ESMC_<Class>
   //   also tests destructor
   rc = ESMC_<Class>Destroy(<class>_ptr);
   sprintf(name, "ESMC_<Class>Destroy"); 
   sprintf(failMsg, "rc = %d, <class>_ptr = %p", rc, <class>_ptr);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

#else

   // perform non-exhaustive tests here;
   //   use same templates as above

#endif

   // return number of failures to environment; 0 = success (all pass)
   return(result);
  
 } // end unit test main()
