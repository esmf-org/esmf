// $Id: ESMCI_Test.C,v 1.12.2.1 2010/02/05 20:14:07 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research,    
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// ESMCI Test method implementation (body) file
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
//  
// !DESCRIPTION:
//  
// The code in this file implements the C++ Test methods declared
// in the companion file ESMCI_Test.h
//
//-----------------------------------------------------------------------------
//
// insert any higher level, 3rd party or system includes here
#include <stdio.h>
#include "ESMCI.h"

// remove define which automatically appends the "CONTEXT" to all
// LogWrite() calls - we do not want to add this file name and line number;
// the calls to the Test subroutines already include the offending filename
// and line number, which is formatted into the message to be printed.
#undef Write

// associated class definition file
#include "ESMCI_Test.h"

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_Test.C,v 1.12.2.1 2010/02/05 20:14:07 svasquez Exp $";
//-----------------------------------------------------------------------------

namespace ESMCI {

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Test routines
//
// TODO: these all use printf, but should use log write routines so output
//  goes to the log, not stdout.
//

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMCI::Test() - Prints whether a test passed or failed
//
// !INTERFACE:   
int Test(
//
// !RETURN VALUE:
//    ESMF_SUCCESS or ESMF_FAILURE
//
// !ARGUMENTS:
  int condition,  // in - the test pass/fail condition
  char *name,     // in - the test name
  char *failMsg,  // in - optional message printed on test failure
  int *result,    // in/out - cumulative failure count
  char *file,     // in - test filename
  int line,       // in - test line number in test filename
  int only) {     // in - if set to 0, print on stderr also
// 
// !DESCRIPTION:
//    Prints PASS/FAIL based on passed-in condition.  If FAIL, prints
//    optional failure message and increments failure result counter.
//    If {\tt only} is zero, also print same message to stderr as well
//    as the normal output on stdout.  The default for {\tt only} is 1.
//
//EOP
//-----------------------------------------------------------------------------
  char msgbuf[ESMF_MAXSTR];
  ESMCI::LogErr *whichLog;

  // TODO: this should be settable by the user
  whichLog = &ESMC_LogDefault;

  if (name == NULL || result == NULL || failMsg == NULL || file == NULL) {
    sprintf(msgbuf, "FAIL %s, line %d, null pointer(s) passed to "
      "ESMCI::Test()\n", __FILE__, __LINE__);
    whichLog->Write(msgbuf, ESMC_LOG_INFO);
    if (!only)
      fprintf(stderr, msgbuf);
    return(ESMF_FAILURE);
  }

  if (condition) {
    sprintf(msgbuf, "PASS %s, %s, line %d\n", name, file, line);
    whichLog->Write(msgbuf, ESMC_LOG_INFO);
    if (!only)
      fprintf(stderr, msgbuf);
  }else {
    sprintf(msgbuf, "FAIL %s, %s, line %d, %s\n", name, file, line, failMsg);
    whichLog->Write(msgbuf, ESMC_LOG_INFO);
    if (!only)
      fprintf(stderr, msgbuf);
    (*result)++; // count total failures; 0 = all pass
  }

  return(ESMF_SUCCESS);

}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMCI::TestEnd() - Print a standard end test message
//
// !INTERFACE:   
int TestEnd(
//
// !RETURN VALUE:
//    ESMF_SUCCESS or ESMF_FAILURE
//
// !ARGUMENTS:
  int result,     // in - cumulative failure count
  char *file,     // in - test filename
  int line,       // in - test line number in test filename
  int only) {     // in - if set to 0, print on stderr also
// 
// !DESCRIPTION:
//    Prints summary message about total failures, and standard exit message.
//    If {\tt only} is zero, also print same message to stderr as well
//    as the normal output on stdout.  The default for {\tt only} is 1.
//
//EOP
//-----------------------------------------------------------------------------
  int rc;
  char msgbuf[ESMF_MAXSTR];
  ESMCI::LogErr *whichLog;

  // TODO: this should be settable by the user
  whichLog = &ESMC_LogDefault;

  if (file == NULL) {
    sprintf(msgbuf, "FAIL %s, line %d, null filename passed to "
      "ESMCI::TestEnd()\n", __FILE__, __LINE__);
    whichLog->Write(msgbuf, ESMC_LOG_INFO);
    if (!only)
      fprintf(stderr, msgbuf);
    return(ESMF_FAILURE);
  }

  sprintf(msgbuf, "Number of failed tests: %d\n", result);
  whichLog->Write(msgbuf, ESMC_LOG_INFO);
  if (!only)
    fprintf(stderr, msgbuf);

  sprintf(msgbuf, "Ending Test, file %s, line %d\n", file, line);
  whichLog->Write(msgbuf, ESMC_LOG_INFO);
  if (!only)
    fprintf(stderr, msgbuf);

  rc = ESMCI_Finalize();
  if (rc != ESMF_SUCCESS) {
    sprintf(msgbuf, "FAIL: %s, line %d, Finalizing ESMF\n", file, line);
    whichLog->Write(msgbuf, ESMC_LOG_INFO);
    if (!only)
      fprintf(stderr, msgbuf);
    return(rc);
  }
 
  return(ESMF_SUCCESS);

}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMCI::TestMaxPETs() - Verify there are not too many PETs
//
// !INTERFACE:   
bool TestMaxPETs(
//
// !RETURN VALUE:
//    true or false
//
// !ARGUMENTS:
  int petCount,   // in - the maximum acceptable number of PETs
  char *file,     // in - test filename
  int line,       // in - test line number in test filename
  int only) {     // in - if set to 0, print on stderr also
// 
// !DESCRIPTION:
//    Returns true if there are not more than the specified number of PETs.
//    If {\tt only} is zero, also print same message to stderr as well
//    as the normal output on stdout.  The default for {\tt only} is 1.
//
//EOP
//-----------------------------------------------------------------------------
  int rc;
  ESMCI::VM *globalVM;
  char msgbuf[ESMF_MAXSTR], failMsg[ESMF_MAXSTR];
  int numPETs;
  ESMCI::LogErr *whichLog;

  // TODO: this should be settable by the user
  whichLog = &ESMC_LogDefault;

  if (file == NULL) {
    sprintf(msgbuf, "FAIL %s, line %d, null filename passed to "
      "ESMCI::TestMaxPETs()\n", __FILE__, __LINE__);
    whichLog->Write(msgbuf, ESMC_LOG_INFO);
    if (!only)
      fprintf(stderr, msgbuf);
    return (false);
  }

  globalVM = ESMCI::VM::getGlobal(&rc);
  if ((globalVM == NULL) || (rc != ESMF_SUCCESS)) {
    sprintf(msgbuf, "FAIL  rc=%d, %s, line %d, Unable to get GlobalVM\n", 
      rc, file, line);

    whichLog->Write(msgbuf, ESMC_LOG_INFO);
    if (!only)
      fprintf(stderr, msgbuf);

    return (false);
  }

  numPETs = globalVM->getPetCount();

  if (numPETs > petCount) {
    sprintf(failMsg, "These tests must not run on more than %d processors.\n", 
      petCount);
    sprintf(msgbuf, "SKIP  %s, %s, line %d\n", failMsg, file, line);
    whichLog->Write(msgbuf, ESMC_LOG_INFO);
    if (!only)
      fprintf(stderr, msgbuf);

    return (false);
  }

  return (true);

}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMCI::TestMinPETs() - Verify there are not too few PETs
//
// !INTERFACE:   
bool TestMinPETs(
//
// !RETURN VALUE:
//    true or false
//
// !ARGUMENTS:
  int petCount,   // in - the minimum acceptable number of PETs
  char *file,     // in - test filename
  int line,       // in - test line number in test filename
  int only) {     // in - if set to 0, print on stderr also
// 
// !DESCRIPTION:
//    Returns true if there are at least petCount PETs.
//    If {\tt only} is zero, also print same message to stderr as well
//    as the normal output on stdout.  The default for {\tt only} is 1.
//
//EOP
//-----------------------------------------------------------------------------
  int rc;
  ESMCI::VM *globalVM;
  char msgbuf[ESMF_MAXSTR], failMsg[ESMF_MAXSTR];
  int numPETs;
  ESMCI::LogErr *whichLog;

  // TODO: this should be settable by the user
  whichLog = &ESMC_LogDefault;

  if (file == NULL) {
    sprintf(msgbuf, "FAIL %s, line %d, null filename passed to "
      "ESMCI::TestMinPETs()\n", __FILE__, __LINE__);
    whichLog->Write(msgbuf, ESMC_LOG_INFO);
    if (!only)
      fprintf(stderr, msgbuf);
    return (false);
  }

  globalVM = ESMCI::VM::getGlobal(&rc);
  if ((globalVM == NULL) || (rc != ESMF_SUCCESS)) {
    sprintf(msgbuf, "FAIL  rc=%d, %s, line %d, Unable to get GlobalVM\n", 
      rc, file, line);

    whichLog->Write(msgbuf, ESMC_LOG_INFO);
    if (!only)
      fprintf(stderr, msgbuf);

    return (false);
  }

  numPETs = globalVM->getPetCount();

  if (numPETs < petCount) {
    sprintf(failMsg, "These tests must not run on less than %d processors.\n",
      petCount);
    sprintf(msgbuf, "SKIP  %s, %s, line %d\n", failMsg, file, line);
   whichLog->Write(msgbuf, ESMC_LOG_INFO);
    if (!only)
      fprintf(stderr, msgbuf);

    return (false);
  }

  return (true);

}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMCI::TestNumPETs() - Verify there are exactly this number of PETs
//
// !INTERFACE:   
bool TestNumPETs(
//
// !RETURN VALUE:
//    true or false
//
// !ARGUMENTS:
  int petCount,   // in - the exact acceptable number of PETs
  char *file,     // in - test filename
  int line,       // in - test line number in test filename
  int only) {     // in - if set to 0, print on stderr also
// 
// !DESCRIPTION:
//    Returns true only if there are exactly the number of requested PETs.
//    If {\tt only} is zero, also print same message to stderr as well
//    as the normal output on stdout.  The default for {\tt only} is 1.
//
//EOP
//-----------------------------------------------------------------------------
  int rc;
  ESMCI::VM *globalVM;
  char msgbuf[ESMF_MAXSTR], failMsg[ESMF_MAXSTR];
  int numPETs;
  ESMCI::LogErr *whichLog;

  // TODO: this should be settable by the user
  whichLog = &ESMC_LogDefault;

  if (file == NULL) {
    sprintf(msgbuf, "FAIL %s, line %d, null filename passed to "
      "ESMCI::TestNumPETs()\n", __FILE__, __LINE__);
    whichLog->Write(msgbuf, ESMC_LOG_INFO);
    if (!only)
      fprintf(stderr, msgbuf);
    return (false);
  }

  globalVM = ESMCI::VM::getGlobal(&rc);
  if ((globalVM == NULL) || (rc != ESMF_SUCCESS)) {
    sprintf(msgbuf, "FAIL  rc=%d, %s, line %d, Unable to get GlobalVM\n", rc,
      file, line);

    whichLog->Write(msgbuf, ESMC_LOG_INFO);
    if (!only)
      fprintf(stderr, msgbuf);

    return (false);
  }

  numPETs = globalVM->getPetCount();

  if (numPETs != petCount) {
    sprintf(failMsg, "These tests must run on exactly %d processors.\n", 
                    petCount);
    sprintf(msgbuf, "SKIP  %s, %s, line %d\n", failMsg, file, line);
    whichLog->Write(msgbuf, ESMC_LOG_INFO);
    if (!only)
      fprintf(stderr, msgbuf);

    return (false);
  }

  return (true);

}
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMCI::TestStart() - Initialize the framework, print a standard msg
//
// !INTERFACE:   
int TestStart(
//
// !RETURN VALUE:
//    ESMF_SUCCESS or ESMF_FAILURE
//
// !ARGUMENTS:
  char *file,     // in - test filename
  int line,       // in - test line number in test filename
  int only) {     // in - if set to 0, print on stderr also
// 
// !DESCRIPTION:
//    Initializes the framework, prints out the standard messages needed
//    by the testing scripts.
//    If {\tt only} is zero, also print same message to stderr as well
//    as the normal output on stdout.  The default for {\tt only} is 1.
//
//EOP
//-----------------------------------------------------------------------------
  int rc;
  ESMCI::VM *globalVM;
  char msgbuf[ESMF_MAXSTR], failMsg[ESMF_MAXSTR];
  int numPETs;
  ESMCI::LogErr *whichLog;

  // TODO: this should be settable by the user
  whichLog = &ESMC_LogDefault;

  if (file == NULL) {
    sprintf(msgbuf, "FAIL %s, line %d, null filename passed to "
      "ESMCI::TestStart()\n", __FILE__, __LINE__);
    whichLog->Write(msgbuf, ESMC_LOG_INFO);
    if (!only)
      fprintf(stderr, msgbuf);
    return(ESMF_FAILURE);
  }

  char logFileName[160];
  char *underScore = strchr(file, '_');
  if (underScore == NULL) underScore = file-1;
  char *period = strrchr(file, '.');
  int numChars = period - underScore;
  strncpy(logFileName, underScore+1, numChars);
  strcpy(logFileName+numChars, "Log\0");

  rc = ESMCI_Initialize((char *)"", ESMC_CAL_NOCALENDAR, logFileName,
    ESMC_LOG_MULTI);
  if (rc != ESMF_SUCCESS) {
    sprintf(msgbuf, "FAIL  rc=%d, %s, line %d, Unable to initialize ESMF\n", rc,
      file, line);
    whichLog->Write(msgbuf, ESMC_LOG_INFO);
    if (!only)
      fprintf(stderr, msgbuf);
    return(rc);
  }

  globalVM = ESMCI::VM::getGlobal(&rc);
  if ((globalVM == NULL) || (rc != ESMF_SUCCESS)) {
    sprintf(msgbuf, "FAIL  rc=%d, %s, line %d, Unable to get GlobalVM\n", rc,
      file, line);

    whichLog->Write(msgbuf, ESMC_LOG_INFO);
    if (!only)
      fprintf(stderr, msgbuf);

    return (false);
  }

  numPETs = globalVM->getPetCount();

  sprintf(msgbuf, "Beginning Test, file %s, line %d\n", file, line);
  whichLog->Write(msgbuf, ESMC_LOG_INFO);
  if (!only)
    fprintf(stderr, msgbuf);

  sprintf(msgbuf, "NUMBER_OF_PROCESSORS %d\n", numPETs);
  whichLog->Write(msgbuf, ESMC_LOG_INFO);
  if (!only)
    fprintf(stderr, msgbuf);
 
  return(ESMF_SUCCESS);

} // end ESMC_TestStart
//-----------------------------------------------------------------------------

} // namespace ESMCI


//-----------------------------------------------------------------------------
extern "C"{
  void FTN(c_esmc_printpassflush)(){
    printf("PASS: \n");
    fflush(stdout);
  }
}
//-----------------------------------------------------------------------------

