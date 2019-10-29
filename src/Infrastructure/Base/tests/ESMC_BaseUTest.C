// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//=============================================================================

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <iostream>

#include "ESMC.h"
#include "ESMC_Test.h"
#include "ESMCI_Macros.h"
#include "ESMCI_LogErr.h"
#include "ESMCI_Util.h"
#include "ESMCI_VM.h"
#include "ESMCI_Base.h"

using namespace ESMCI;
using namespace std;

//=============================================================================
//BOP
// !PROGRAM: ESMC_BaseUTest - Test C Base
//
// !DESCRIPTION: Test C Base
//
//EOP
//-----------------------------------------------------------------------------

void finalizeFailure(int& rc, char failMsg[], string msg) {
  rc = ESMF_FAILURE;
  strcpy(failMsg, msg.c_str());
  return;
};

#undef  ESMC_METHOD
#define ESMC_METHOD "testSerializeDeserialize()"
void testSerializeDeserialize(int& rc, char failMsg[]) {
  rc = ESMF_FAILURE;
  std::string logmsg;

  ESMC_Base base;

  char *nullbuffer = nullptr;
  int length = 0;
  int offset = 0;

  rc = base.ESMC_Serialize(nullbuffer, &length, &offset, ESMC_ATTRECONCILE_OFF,
    ESMF_INQUIREONLY);
  if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return;

  int inquire_offset = offset;

  logmsg = std::string(ESMC_METHOD) + ": inquire_offset=" + std::to_string(inquire_offset);
  ESMC_LogWrite(logmsg.c_str(), ESMC_LOGMSG_INFO);

  length = inquire_offset;
  char buffer[length];
  offset = 0;
  rc = base.ESMC_Serialize(buffer, &length, &offset, ESMC_ATTRECONCILE_OFF,
                           ESMF_NOINQUIRE);
  if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return;

  logmsg = std::string(ESMC_METHOD) + ": offset=" + std::to_string(offset);
  ESMC_LogWrite(logmsg.c_str(), ESMC_LOGMSG_INFO);

  if (inquire_offset != offset) {
    return finalizeFailure(rc, failMsg, "offsets not equal with inquire switch");
  }

  return;
};

int main(void) {

  char name[80];
  char failMsg[80];
  int result = 0;
  int rc = ESMF_FAILURE;

  strcpy(failMsg, "Did not return ESMF_SUCCESS");  // Default fail message

  //---------------------------------------------------------------------------
  ESMC_TestStart(__FILE__, __LINE__, 0);
  //---------------------------------------------------------------------------

  //---------------------------------------------------------------------------
  //NEX_UTest
  strcpy(name, "testSerializeDeserialize");
  testSerializeDeserialize(rc, failMsg);
  ESMC_Test((rc==ESMF_SUCCESS), name, failMsg, &result, __FILE__, __LINE__, 0);
  //---------------------------------------------------------------------------

  //---------------------------------------------------------------------------
  ESMC_TestEnd(__FILE__, __LINE__, 0);
  //---------------------------------------------------------------------------

  return 0;
};
