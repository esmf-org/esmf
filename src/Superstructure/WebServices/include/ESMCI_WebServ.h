// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2018, University Corporation for Atmospheric Research,
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

#ifndef ESMCI_WebServ_H
#define ESMCI_WebServ_H

#include "ESMCI_Comp.h"

//-----------------------------------------------------------------------------
//BOPI
// !CLASS:  ESMCI_WebServInterface - Public C interface to the classes
//          necessary to create a component service.
//
// !DESCRIPTION:
//
// The code in this file defines the public C interface to the methods
// necessary to create a component service.  These include public methods in
// the ESMCI_WebServComponentSvr, ESMCI_WebServNetEsmfServer, and
// ESMCI_WebServRegistrarClient classes.  These interfaces are necessary to
// create a component service with a Fortran app driver.
//
// (KDS: I have a feeling this file is going to be moved to the interfaces
//       directory.  I'm also guessing the function names will end up
//       needing to be changed.)
//
//EOPI
//-----------------------------------------------------------------------------


extern "C"
{
        // service loop from ESMCI_WebServComponentSvr class
   void  FTN_X(c_esmc_componentsvcloop)(
                  char*                   clientId,
                  char*                   registrarHost,
                  ESMCI::GridComp*        comp,
                  ESMCI::State*           importState,
                  ESMCI::State*           exportState,
                  ESMCI::Clock*           clock,
                  ESMC_BlockingFlag*      blockingFlag,
                  int*                    phase,
                  int*                    portNum,
                  int*                    rc,
                  ESMCI_FortranStrLenArg  clientIdLen,
                  ESMCI_FortranStrLenArg  registrarHostLen);

        // service loop from ESMCI_WebServComponentSvr class
   void  FTN_X(c_esmc_cplcomponentsvcloop)(
                  char*                   clientId,
                  char*                   registrarHost,
                  ESMCI::CplComp*         comp,
                  ESMCI::State*           importState,
                  ESMCI::State*           exportState,
                  ESMCI::Clock*           clock,
                  ESMC_BlockingFlag*      blockingFlag,
                  int*                    phase,
                  int*                    portNum,
                  int*                    rc,
                  ESMCI_FortranStrLenArg  clientIdLen,
                  ESMCI_FortranStrLenArg  registrarHostLen);

        // registration methods from ESMCI_WebServRegistrarClient class
   void  FTN_X(c_esmc_registercomponent)(
                  char*                   compName,
                  char*                   compDesc,
                  char*                   clientId,
                  char*                   registrarHost,
                  int*                    portNum,
                  int*                    rc,
                  ESMCI_FortranStrLenArg  compNameLen,
                  ESMCI_FortranStrLenArg  compDescLen,
                  ESMCI_FortranStrLenArg  clientIdLen,
                  ESMCI_FortranStrLenArg  registrarHostLen);

   void  FTN_X(c_esmc_unregistercomponent)(
                  char*                   clientId,
                  char*                   registrarHost,
                  int*                    rc,
                  ESMCI_FortranStrLenArg  clientIdLen,
                  ESMCI_FortranStrLenArg  registrarHostLen);

        void  FTN_X(c_esmc_getportnum)(int*  portNum,
                                  int*  rc);

        void  FTN_X(c_esmc_addoutputfilename)(char*                   filename,
                                         int*                    rc,
                                         ESMCI_FortranStrLenArg  filenameLen);

/*
        void  FTN(c_esmc_addoutputdata)(double*                 timestamp,
                                   char*                   varName,
                                   double**                dataValues,
                                   int*                    rc,
                                   ESMCI_FortranStrLenArg  filenameLen);
*/
        void  FTN_X(c_esmc_addoutputdata)(int*  rc);
};


#endif          // ESMCI_WebServ_H
