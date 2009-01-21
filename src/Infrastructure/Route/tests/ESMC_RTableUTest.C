// $Id: ESMC_RTableUTest.C,v 1.5.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
// !PROGRAM:  ESMF_RTableTest - one line general statement about this test 
//
// !DESCRIPTION:
//
// The code in this file drives C++ RTable unit tests.
// The companion files ESMC\_RTable.h and ESMC\_RTable.C contain
// the declarations and definitions for the RTable methods.
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
 static const char *const version = "$Id: ESMC_RTableUTest.C,v 1.5.2.2 2009/01/21 21:25:23 cdeluca Exp $";
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

   // for dynamically allocated RTable's
   ESMC_RTable *rtable_ptr;

   // for statically allocated RTable's
   //  tests default constructor; add args to test other constructors
   ESMC_RTable rtable;

   // test dynamic allocation of ESMC_RTable
   //   also tests default constructor
   rtable_ptr = ESMC_RTableCreate(args, &rc);
   sprintf(name, "ESMC_RTableCreate"); 
   sprintf(failMsg, "rc = %d, rtable_ptr = %p, args = %f",
           rc, rtable_ptr, args);
   ESMC_Test((rtable_ptr!=0 && rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);
    
   // test internal dynamic allocation within statically allocated
   //   ESMC_RTable
   rc = rtable_ptr->ESMC_RTableConstruct(args);
   sprintf(name, "ESMC_RTableConstruct"); 
   sprintf(failMsg, "rc = %d, args = %f", rc, args);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test initialization of members of statically allocated ESMC_RTable
   //   may want to read back values via Get methods for comparison
   rc = rtable_ptr->ESMC_RTableInit(args);
   sprintf(name, "ESMC_RTableInit"); 
   sprintf(failMsg, "rc = %d, args = %f", rc, args);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test setting of configuration values
   ESMC_RTableConfig config_set;
   rc = rtable_ptr->ESMC_RTableSetConfig(config_set);
   sprintf(name, "ESMC_RTableSetConfig"); 
   sprintf(failMsg, "rc = %d, config_set = %f", rc, config_set);
   ESMC_Test((rc==ESMF_SUCCESS), 
              name, failMsg, &result, ESMF_SRCLINE);

   // test getting of configuration values,
   //  compare to values set previously
   ESMC_RTableConfig config_get;
   rc = rtable_ptr->ESMC_RTableGetConfig(&config_get);
   sprintf(name, "ESMC_RTableGetConfig"); 
   sprintf(failMsg, "rc = %d, config_get = %f", rc, config_get);
   ESMC_Test((rc==ESMF_SUCCESS && config_get == config_set),
              name, failMsg, &result, ESMF_SRCLINE);

   // test setting of ESMC_RTable members values
   <value type> value_set;
   rc = rtable_ptr->ESMC_RTableSet<Value>(value_set);
   sprintf(name, "ESMC_RTableSet<Value>"); 
   sprintf(failMsg, "rc = %d, value_set = %f", rc, value_set);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test getting of ESMC_RTable members values,
   //   compare to values set previously
   <value type> value_get;
   rc = rtable_ptr->ESMC_RTableGet<Value>(&value_get);
   sprintf(name, "ESMC_RTableGet<Value>"); 
   sprintf(failMsg, "rc = %d, value_get = %f", rc, value_get);
   ESMC_Test((rc==ESMF_SUCCESS && value_get == value_set),
              name, failMsg, &result, ESMF_SRCLINE);
    
   // test validate method via option string
   char validate_options[ESMF_MAXSTR];
   rc = rtable_ptr->ESMC_RTableValidate(validate_options);
   sprintf(name, "ESMC_RTableValidate"); 
   sprintf(failMsg, "rc = %d, validate_options = %s", rc, validate_options);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test print method via option string
   char print_options[ESMF_MAXSTR];
   rc = rtable_ptr->ESMC_RTablePrint(print_options);
   sprintf(name, "ESMC_RTablePrint"); 
   sprintf(failMsg, "rc = %d, print_options = %s", rc, print_options);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test internal dynamic deallocation within statically allocated 
   //   ESMC_RTable
   rc = rtable_ptr->ESMC_RTableDestruct();
   sprintf(name, "ESMC_RTableDestruct"); 
   sprintf(failMsg, "rc = %d", rc);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test dynamic deallocation of ESMC_RTable
   //   also tests destructor
   rc = ESMC_RTableDestroy(rtable_ptr);
   sprintf(name, "ESMC_RTableDestroy"); 
   sprintf(failMsg, "rc = %d, rtable_ptr = %p", rc, rtable_ptr);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // return number of failures to environment; 0 = success (all pass)
   return(result);
  
 } // end unit test main()
