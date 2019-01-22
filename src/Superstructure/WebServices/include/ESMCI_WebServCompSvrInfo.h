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
//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times


#ifndef ESMCI_WebServCompSvrInfo_H
#define ESMCI_WebServCompSvrInfo_H

#include <vector>
#include <string>

using namespace std;

//-------------------------------------------------------------------------
//BOPI
// !CLASS: ESMCI::ESMCI_WebServCompSvrInfo
//
// !DESCRIPTION:
//
// The code in this file defines the C++ CompSvrInfo members and method
// signatures (prototypes).  The companion file ESMCI\_WebServCompSvrInfo.C
// contains the full code (bodies) for the CompSvrInfo methods.
//
// This class contains the information about a client session and provides
// the methods for setting and retrieving that information.
//
//EOPI
//-------------------------------------------------------------------------

namespace ESMCI
{

  class ESMCI_WebServCompSvrInfo
  {
  public:

     // constructor and destructor
          ESMCI_WebServCompSvrInfo();
          ~ESMCI_WebServCompSvrInfo();

     // get methods
          int                                   clientId()              { return theClientId; }
          string                        jobId()                 { return theJobId; }
          string                        hostName()              { return theHostName; }
          int                                   portNum()               { return thePortNum; }
          string                        name()                  { return theName; }
          string                        desc()                  { return theDesc; }
          string                        physHostName()  { return thePhysHostName; }
          int                                   status()                        { return theCurrentStatus; }

     // set methods
     void  setClientId(int  clientId);
          void  setJobId(string  jobId);
          void  setHostName(string  hostName);
          void  setPortNum(int  portNum);
          void  setName(string  name);
          void  setDesc(string  desc);
          void  setPhysHostName(string  hostName);
     void  setStatus(int  status);

     void  print();

  private:

          int                                   theClientId;            // id for the client assigned to svr
     string                             theJobId;                       // the unique id for the svr
          string                                theHostName;            // name of the host which initiated
                                       //   server started
          int                                   thePortNum;       // port number of the server socket
     string                             theName;                           // name of the component
     string                             theDesc;                           // description for the component
          string                                thePhysHostName;        // name of the host on which the svr
                                       //   is actually running
          int                                   theCurrentStatus;       // current status of the server
  };

} // end namespace

#endif          // ESMCI_WebServCompSvrInfo_H
