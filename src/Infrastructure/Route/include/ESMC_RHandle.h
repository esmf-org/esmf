// $Id: ESMC_RHandle.h,v 1.1 2003/08/21 19:57:11 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMF RHandle C++ declaration include file
//
// (all lines below between the !BOP and !EOP markers will be included in 
//  the automated document processing.)
//-----------------------------------------------------------------------------
//
 // these lines prevent this file from being read more than once if it
 // ends up being included multiple times

 #ifndef ESMC_RHandle_H
 #define ESMC_RHandle_H

//-----------------------------------------------------------------------------

 // Put any constants or macros which apply to the whole component in this file.
 // Anything public or esmf-wide should be up higher at the top level
 // include files.

//-----------------------------------------------------------------------------
//BOP
// !CLASS:  ESMC_RouteHandle - Handle which points to precomputed information
//              
//
// !DESCRIPTION:
//
// The code in this file defines the C++ RouteHandle class and declares method 
// signatures (prototypes).  The companion file ESMC_RHandle.C contains
// the definitions (full code bodies) for the RouteHandle methods.
//
// 
//
//-----------------------------------------------------------------------------
// 
// !USES:
 #include <ESMC_Base.h>  // all classes inherit from the ESMC Base class.
 #include <ESMC_LocalArray.h>
 #include <ESMC_XPacket.h> 
 #include <ESMC_Route.h>

// !PUBLIC TYPES:
 class ESMC_RouteHandle;

typedef enum { ESMC_HALO=1, ESMC_REDIST, ESMC_REGRID, ESMC_UNINITIALIZED } 
ESMC_HandleType;

// !PRIVATE TYPES:


 // class declaration type
 class ESMC_RouteHandle {   // does NOT inherit from Base class

   private:
     ESMC_HandleType htype;
     ESMC_Route *rhandle1;
     ESMC_Route *rhandle2;
     ESMC_LocalArray *localdata;
     char *label;  // for debug
 
// !PUBLIC MEMBER FUNCTIONS:
//

  public:
    int ESMC_RouteHandleConstruct(void);
    int ESMC_RouteHandleDestruct(void);    

 // accessor methods for class members
    int ESMC_RouteHandleGet(ESMC_HandleType *h, ESMC_Route **rh1,
                            ESMC_Route **rh2, ESMC_LocalArray **ldata, 
                            char **l) const;
    int ESMC_RouteHandleSet(ESMC_HandleType h, ESMC_Route *rh1,
                            ESMC_Route *rh2, ESMC_LocalArray *ldata,
                            char *l);

    ESMC_HandleType ESMC_RouteHandleGetType(void) const { return htype; }
    ESMC_Route *ESMC_RouteHandleGetRHandle(void) const { return rhandle1; }
    ESMC_Route *ESMC_RouteHandleGetRHandle(int count) const { 
       if (count == 1) return rhandle1;
       if (count == 2) return rhandle2;
       return NULL; }
    ESMC_LocalArray *ESMC_RouteHandleGetLocalData(void) const { return localdata; }
    char *ESMC_RouteHandleGetLabel(void) { return label; }
    
    int ESMC_RouteHandleSetType(ESMC_HandleType h) { 
         htype = h; return ESMF_SUCCESS; }
    int ESMC_RouteHandleSetRHandle(ESMC_Route *rh1) {
         rhandle1 = rh1; return ESMF_SUCCESS; }
    int ESMC_RouteHandleSetRHandle(ESMC_Route *rh1, ESMC_Route *rh2) {
         rhandle1 = rh1; rhandle2 = rh2; return ESMF_SUCCESS; }
    int ESMC_RouteHandleSetRHandle(int count, ESMC_Route *rh) {
       if (count == 1) rhandle1 = rh;
       if (count == 2) rhandle2 = rh; 
       return ESMF_SUCCESS; }
    int ESMC_RouteHandleSetLocalData(ESMC_LocalArray *ldata) {
        localdata = ldata; return ESMF_SUCCESS; }
    int ESMC_RouteHandleSetLabel(char *l) {
        int len = strlen(l) + 1;
        if (label != NULL) delete [] label;
        label = new char[len]; 
        strcpy(label, l);
        return ESMF_SUCCESS; }

 // required methods inherited and overridden from the ESMC_Base class
    int ESMC_RouteHandleValidate(const char *options) const;
    int ESMC_RouteHandlePrint(const char *options) const;

 // native C++ constructors/destructors
	ESMC_RouteHandle(void);
	~ESMC_RouteHandle(void);
  
  
// !PRIVATE MEMBER FUNCTIONS:
//
  private: 

//
//
//EOP
//-----------------------------------------------------------------------------

 };   // end class ESMC_RouteHandle

// Create and Destroy are declared as class helper functions (not methods)
// since they create and destroy an ESMC_RHandle object itself. E.g. if Create
// were a method, the ESMC_RHandle object on whose behalf it was being invoked
// would need to already exist!  These functions are supersets of C++ new
// and delete; they perform allocation/deallocation specialized to
// an ESMC_RouteHandle object.

 ESMC_RouteHandle *ESMC_RouteHandleCreate(int *rc);
 int ESMC_RouteHandleDestroy(ESMC_RouteHandle *rtable);

 #endif  // ESMC_RHandle_H
