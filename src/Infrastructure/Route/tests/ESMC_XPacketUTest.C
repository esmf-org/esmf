// $Id: ESMC_XPacketUTest.C,v 1.6.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
// !PROGRAM:  ESMF_XPacketTest - one line general statement about this test 
//
// !DESCRIPTION:
//
// The code in this file drives C++ XPacket unit tests.
// The companion files ESMC\_XPacket.h and ESMC\_XPacket.C contain
// the declarations and definitions for the XPacket methods.
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
 static const char *const version = "$Id: ESMC_XPacketUTest.C,v 1.6.2.2 2009/01/21 21:25:23 cdeluca Exp $";
//-----------------------------------------------------------------------------

 int main(int argc, char *argv[])
 {
   // cumulative result: count failures; no failures equals "all pass"
   int result = 0;

   // individual test result code
   int rc;

   // individual test failure message
   char failMsg[ESMF_MAXSTR];

   // for dynamically allocated XPacket's
   ESMC_XPacket *xpacket_ptr;

   // for statically allocated XPacket's
   //  tests default constructor; add args to test other constructors
   ESMC_XPacket xpacket;

   // test dynamic allocation of ESMC_XPacket
   //   also tests default constructor
   xpacket_ptr = ESMC_XPacketCreate(args, &rc);
   sprintf(failMsg, "rc = %d, xpacket_ptr = %p, args = %f",
           rc, xpacket_ptr, args);
   ESMC_Test((xpacket_ptr!=0 && rc==ESMF_SUCCESS),
              failMsg, &result, ESMF_SRCLINE);
    
   // test internal dynamic allocation within statically allocated
   //   ESMC_XPacket
   rc = xpacket_ptr->ESMC_XPacketConstruct(args);
   sprintf(failMsg, "rc = %d, args = %f", rc, args);
   ESMC_Test((rc==ESMF_SUCCESS),
              failMsg, &result, ESMF_SRCLINE);

   // test initialization of members of statically allocated ESMC_XPacket
   //   may want to read back values via Get methods for comparison
   rc = xpacket_ptr->ESMC_XPacketSetDefault(args);
   sprintf(failMsg, "rc = %d, args = %f", rc, args);
   ESMC_Test((rc==ESMF_SUCCESS),
              failMsg, &result, ESMF_SRCLINE);

   // test setting of configuration values
   ESMC_XPacketConfig config_set;
   rc = xpacket_ptr->ESMC_XPacketSetConfig(config_set);
   sprintf(failMsg, "rc = %d, config_set = %f", rc, config_set);
   ESMC_Test((rc==ESMF_SUCCESS), 
              failMsg, &result, ESMF_SRCLINE);

   // test getting of configuration values,
   //  compare to values set previously
   ESMC_XPacketConfig config_get;
   rc = xpacket_ptr->ESMC_XPacketGetConfig(&config_get);
   sprintf(failMsg, "rc = %d, config_get = %f", rc, config_get);
   ESMC_Test((rc==ESMF_SUCCESS && config_get == config_set),
              failMsg, &result, ESMF_SRCLINE);

   // test setting of ESMC_XPacket members values
   <value type> value_set;
   rc = xpacket_ptr->ESMC_XPacketSet<Value>(value_set);
   sprintf(failMsg, "rc = %d, value_set = %f", rc, value_set);
   ESMC_Test((rc==ESMF_SUCCESS),
              failMsg, &result, ESMF_SRCLINE);

   // test getting of ESMC_XPacket members values,
   //   compare to values set previously
   <value type> value_get;
   rc = xpacket_ptr->ESMC_XPacketGet<Value>(&value_get);
   sprintf(failMsg, "rc = %d, value_get = %f", rc, value_get);
   ESMC_Test((rc==ESMF_SUCCESS && value_get == value_set),
              failMsg, &result, ESMF_SRCLINE);
    
   // test validate method via option string
   char validate_options[ESMF_MAXSTR];
   rc = xpacket_ptr->ESMC_XPacketValidate(validate_options);
   sprintf(failMsg, "rc = %d, validate_options = %s", rc, validate_options);
   ESMC_Test((rc==ESMF_SUCCESS),
              failMsg, &result, ESMF_SRCLINE);

   // test print method via option string
   char print_options[ESMF_MAXSTR];
   rc = xpacket_ptr->ESMC_XPacketPrint(print_options);
   sprintf(failMsg, "rc = %d, print_options = %s", rc, print_options);
   ESMC_Test((rc==ESMF_SUCCESS),
              failMsg, &result, ESMF_SRCLINE);

   // test internal dynamic deallocation within statically allocated 
   //   ESMC_XPacket
   rc = xpacket_ptr->ESMC_XPacketDestruct();
   sprintf(failMsg, "rc = %d", rc);
   ESMC_Test((rc==ESMF_SUCCESS),
              failMsg, &result, ESMF_SRCLINE);

   // test dynamic deallocation of ESMC_XPacket
   //   also tests destructor
   rc = ESMC_XPacketDestroy(xpacket_ptr);
   sprintf(failMsg, "rc = %d, xpacket_ptr = %p", rc, xpacket_ptr);
   ESMC_Test((rc==ESMF_SUCCESS),
              failMsg, &result, ESMF_SRCLINE);

   // return number of failures to environment; 0 = success (all pass)
   return(result);
  
 } // end unit test main()
