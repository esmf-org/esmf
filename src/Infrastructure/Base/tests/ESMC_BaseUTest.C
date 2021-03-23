// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2021, University Corporation for Atmospheric Research,
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

const bool TO_STDOUT = false;
const bool TO_LOG = false;

void finalizeFailure(int& rc, char failMsg[], string msg) {
  rc = ESMF_FAILURE;
  strcpy(failMsg, msg.c_str());
  return;
};

void lognprint(const std::string logmsg, bool to_stdout) {
  if (to_stdout) {
    std::cout << logmsg << std::endl;
  }
  if (TO_LOG) {
    ESMC_LogWrite(logmsg.c_str(), ESMC_LOGMSG_INFO);
  }
}

#undef  ESMC_METHOD
#define ESMC_METHOD "testSerializeDeserialize()"
void testSerializeDeserialize(int& rc, char failMsg[]) {
  rc = ESMF_FAILURE;
  std::string logmsg;

  char *nullbuffer = nullptr;
  const std::vector<ESMC_AttReconcileFlag> arflags = {ESMC_ATTRECONCILE_OFF,
                                                      ESMC_ATTRECONCILE_ON};

  // Test with and without attribute (info) reconciliation
  for (const auto arflag : arflags) {
    logmsg = std::string(ESMC_METHOD) + ": arflag=" + std::to_string(arflag);
    lognprint(logmsg, TO_STDOUT);

    ESMC_Base *base_src = new ESMC_Base(1);
    ESMC_Base *base_dst = new ESMC_Base(-1);

    int length = 0;
    int offset = 0;

    // Add some attributes to the source base
    ESMCI::Info *info = base_src->ESMC_BaseGetInfo();
    try {
      info->set<int>("key1", 44, false);
      info->set<double>("key123", 3.146789, false);
      info->set<std::string>("a-string-key", "some-data", false);
    }
    ESMF_CATCH_INFO

    rc = base_src->ESMC_Serialize(nullbuffer, &length, &offset, arflag, ESMF_INQUIREONLY);
    if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return;

    int inquire_offset = offset;

    logmsg = std::string(ESMC_METHOD) + ": inquire_offset=" + std::to_string(inquire_offset);
    ESMC_LogWrite(logmsg.c_str(), ESMC_LOGMSG_INFO);

    // Add a shift to emulate what happens in Reconcile
    int shift = 16;
    length = inquire_offset + shift;
    char buffer[length];
    offset = shift;
    rc = base_src->ESMC_Serialize(buffer, &length, &offset, arflag, ESMF_NOINQUIRE);
    if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return;

    logmsg = std::string(ESMC_METHOD) + ": offset=" + std::to_string(offset);
    ESMC_LogWrite(logmsg.c_str(), ESMC_LOGMSG_INFO);

    if (inquire_offset < offset) {
      return finalizeFailure(rc, failMsg, "offset should be shorter than inquire");
    }

    offset = shift;
    rc = base_dst->ESMC_Deserialize(buffer, &offset, arflag);
    if (ESMC_LogDefault.MsgFoundError(rc, ESMCI_ERR_PASSTHRU, ESMC_CONTEXT, &rc)) return;

    ESMCI::Info *infod = base_dst->ESMC_BaseGetInfo();
    try {
      std::string dump = infod->dump();
      if (arflag == ESMC_ATTRECONCILE_OFF) {
        if (dump != "{}") {
          return finalizeFailure(rc, failMsg, "if not reconciling info, then object should be empty");
        }
      } else {
        const std::string desired = "{\"a-string-key\":\"some-data\",\"key1\":44,\"key123\":3.146789}";
        if (dump != desired) {
          return finalizeFailure(rc, failMsg, "if reconciling info, then object should have data");
        }
      }
    }
    ESMF_CATCH_INFO
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
