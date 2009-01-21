// $Id: ESMC_CommTableUTest.C,v 1.6.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
// !PROGRAM:  ESMF_CommTableTest - one line general statement about this test 
//
// !DESCRIPTION:
//
// The code in this file drives C++ CommTable unit tests.
// The companion files ESMC\_CommTable.h and ESMC\_CommTable.C contain
// the declarations and definitions for the CommTable methods.
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
 static const char *const version = "$Id: ESMC_CommTableUTest.C,v 1.6.2.2 2009/01/21 21:25:23 cdeluca Exp $";
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

   // for dynamically allocated CommTable's
   ESMC_CommTable *commtable_ptr;

   // for statically allocated CommTable's
   //  tests default constructor; add args to test other constructors
   ESMC_CommTable commtable;

   // test dynamic allocation of ESMC_CommTable
   //   also tests default constructor
   commtable_ptr = ESMC_CommTableCreate(args, &rc);
   sprintf(name, "ESMC_CommTableCreate"); 
   sprintf(failMsg, "rc = %d, commtable_ptr = %p, args = %f",
           rc, commtable_ptr, args);
   ESMC_Test((commtable_ptr!=0 && rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);
    
   // test initialization of members of statically allocated ESMC_CommTable
   //   may want to read back values via Get methods for comparison
   rc = commtable_ptr->ESMC_CommTableInit(args);
   sprintf(name, "ESMC_CommTableInit"); 
   sprintf(failMsg, "rc = %d, args = %f", rc, args);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test setting of configuration values
   ESMC_CommTableConfig config_set;
   rc = commtable_ptr->ESMC_CommTableSetConfig(config_set);
   sprintf(name, "ESMC_CommTableSetConfig"); 
   sprintf(failMsg, "rc = %d, config_set = %f", rc, config_set);
   ESMC_Test((rc==ESMF_SUCCESS), 
              name, failMsg, &result, ESMF_SRCLINE);

   // test getting of configuration values,
   //  compare to values set previously
   ESMC_CommTableConfig config_get;
   rc = commtable_ptr->ESMC_CommTableGetConfig(&config_get);
   sprintf(name, "ESMC_CommTableGetConfig"); 
   sprintf(failMsg, "rc = %d, config_get = %f", rc, config_get);
   ESMC_Test((rc==ESMF_SUCCESS && config_get == config_set),
              name, failMsg, &result, ESMF_SRCLINE);

   // test setting of ESMC_CommTable members values
   <value type> value_set;
   rc = commtable_ptr->ESMC_CommTableSet<Value>(value_set);
   sprintf(name, "ESMC_CommTableSet<Value>"); 
   sprintf(failMsg, "rc = %d, value_set = %f", rc, value_set);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test getting of ESMC_CommTable members values,
   //   compare to values set previously
   <value type> value_get;
   rc = commtable_ptr->ESMC_CommTableGet<Value>(&value_get);
   sprintf(name, "ESMC_CommTableGet<Value>"); 
   sprintf(failMsg, "rc = %d, value_get = %f", rc, value_get);
   ESMC_Test((rc==ESMF_SUCCESS && value_get == value_set),
              name, failMsg, &result, ESMF_SRCLINE);
    
   // test validate method via option string
   char validate_options[ESMF_MAXSTR];
   rc = commtable_ptr->ESMC_CommTableValidate(validate_options);
   sprintf(name, "ESMC_CommTableValidate"); 
   sprintf(failMsg, "rc = %d, validate_options = %s", rc, validate_options);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test print method via option string
   char print_options[ESMF_MAXSTR];
   rc = commtable_ptr->ESMC_CommTablePrint(print_options);
   sprintf(name, "ESMC_CommTablePrint"); 
   sprintf(failMsg, "rc = %d, print_options = %s", rc, print_options);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // test dynamic deallocation of ESMC_CommTable
   //   also tests destructor
   rc = ESMC_CommTableDestroy(commtable_ptr);
   sprintf(name, "ESMC_CommTableDestroy"); 
   sprintf(failMsg, "rc = %d, commtable_ptr = %p", rc, commtable_ptr);
   ESMC_Test((rc==ESMF_SUCCESS),
              name, failMsg, &result, ESMF_SRCLINE);

   // return number of failures to environment; 0 = success (all pass)
   return(result);
  
 } // end unit test main()
