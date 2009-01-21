// $Id: ESMC_RHandle.h,v 1.10.2.4 2009/01/21 21:25:23 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

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
 #include "ESMC_TValues.h"

// !PUBLIC TYPES:
 class ESMC_RouteHandle;

// does this handle describe the memory movements needed to execute
// a halo, redistribution, or regrid operation?  set by the corresponding
// store operation, and available to be error checked at run time.
typedef enum { 
    ESMC_HALOHANDLE=1, 
    ESMC_REDISTHANDLE, 
    ESMC_REGRIDHANDLE, 
    ESMC_UNINITIALIZEDHANDLE,
    ESMC_ARRAYXXE,
    ESMC_ARRAYBUNDLEXXE
} ESMC_HandleType;

// how many route tables are there inside a single handle?  one to one means
// the store and run calls must supply the exact same number of addresses
// as routes or transform values.  one to all means a list of addresses 
// comes in, but there is a single route which applies to all.  
// TODO: and finally, indirect is left as a placeholder for smarter code
// which should be written in the bundle code, which identifies which
// fields in the bundle are congruent, and are part of the store call.
// the smallest subset of unique routes are created, and the indirect map
// maps input addresses to routes.  presumably the number of routes will
// be smaller than the input addresses.
typedef enum { 
    ESMC_1TO1HANDLEMAP=1, 
    ESMC_ALLTO1HANDLEMAP, 
    ESMC_INDIRECTHANDLEMAP, 
    ESMC_NOHANDLEMAP, 
    ESMC_UNKNOWNHANDLEMAP
} ESMC_HandleMapping;

// !PRIVATE TYPES:

 // class declaration type
 class ESMC_RouteHandle : public ESMC_Base {    // inherits from ESMC_Base class

   private:
     ESMC_HandleType htype;         // halo, redist, or regrid
     int nroutes;                   // count of number of routes
     ESMC_HandleMapping rmapping;   // mapping type: 1to1, Allto1, or indirect
     ESMC_Route *routes;          // array of 'nroutes' routes
     int *rmap;                     // if indirect, the actual map
                                    // the next 3 are used by regrid only
     int ntvalues;                  // count of number of tvalues 
     ESMC_HandleMapping tvmapping;  // mapping type: 1to1, Allto1, or indirect
     ESMC_TransformValues *tvalues; // array of 'ntvalues' tvalues
     int *tvmap;                    // if indirect, the actual map
     char *label;                   // for debug, info, etc.
     //gjt: the following member can be used to attach any information to
     //a routehandle, e.g. temporary storage needed in new Array communications
     void *storage;                 // storage used by specific communication
 
// !PUBLIC MEMBER FUNCTIONS:
//

  public:
    int ESMC_RouteHandleConstruct(void);
    int ESMC_RouteHandleDestruct(void);    

 // general get/set routines
    int ESMC_RouteHandleGet(ESMC_HandleType *h, 
                            int *rt_count, ESMC_HandleMapping *rmaptype,
                            int which_rt, ESMC_Route **rh, 
                            int *tv_count, ESMC_HandleMapping *tvmaptype,
                            int which_tv, ESMC_TransformValues **td, 
                            char **l) const;
    int ESMC_RouteHandleSet(ESMC_HandleType h, 
                            int rt_count, ESMC_HandleMapping rmaptype, 
                            int which_rt, ESMC_Route *rh,
                            int tv_count, ESMC_HandleMapping tvmaptype, 
                            int which_tv, ESMC_TransformValues *td, char *l);

 // accessor methods for individual class members
    ESMC_HandleType ESMC_RouteHandleGetType(void) const { return htype; }
    int ESMC_RouteHandleGetRTCount(void) const { return nroutes; }
    ESMC_Route *ESMC_RouteHandleGetRoute(int which_rt=0) 
           const { return &routes[which_rt]; }
    int ESMC_RouteHandleGetTVCount(void) const { return ntvalues; }
    ESMC_TransformValues *ESMC_RouteHandleGetTValues(int which_tv=0) 
           const { return &tvalues[which_tv]; }
    char *ESMC_RouteHandleGetLabel(void) const { return label; }
    
    int ESMC_RouteHandleSetType(ESMC_HandleType h) { 
         htype = h; return ESMF_SUCCESS; }
    int ESMC_RouteHandleSetRouteCount(int rtcount);
    int ESMC_RouteHandleSetRMapType(ESMC_HandleMapping rmaptype) {
         rmapping = rmaptype; return ESMF_SUCCESS; }
    int ESMC_RouteHandleSetRoute(int which_rt=0, ESMC_Route *rh=NULL) {
         if (which_rt >= nroutes) return ESMF_FAILURE;
	 if (rh == NULL) return ESMF_SUCCESS;
         routes[which_rt] = *rh; return ESMF_SUCCESS; }
    int ESMC_RouteHandleSetTVCount(int tvcount);
    int ESMC_RouteHandleSetTVMapType(ESMC_HandleMapping tvmaptype) {
         tvmapping = tvmaptype; return ESMF_SUCCESS; }
    int ESMC_RouteHandleSetTValues(int which_tv=0, 
                                   ESMC_TransformValues *tv=NULL) {
        if (which_tv >= ntvalues) return ESMF_FAILURE;
        if (tv == NULL) return ESMF_SUCCESS;
        tvalues[which_tv] = *tv; return ESMF_SUCCESS; }
    int ESMC_RouteHandleSetLabel(char *l);

    void *ESMC_RouteHandleGetStorage(void) const{
      return storage;
    }
    int ESMC_RouteHandleSetStorage(void *ptr){
      storage = ptr;
      return ESMF_SUCCESS;
    }
        
        
        
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

// fortran interface calls
extern "C" {

 void FTN(c_esmc_transformvaluescreate)(ESMC_TransformValues **ptr, 
                         int *count, int *status);

 void FTN(c_esmc_transformvaluesdestroy)(ESMC_TransformValues **ptr, int *status);

 void FTN(c_esmc_transformvaluesget)(ESMC_TransformValues **ptr, 
          int *numlist, ESMC_LocalArray **src, ESMC_LocalArray **dst, 
          ESMC_LocalArray **w, int *status);

 // the int needs to be an enum, the label needs to be added and handled
 void FTN(c_esmc_transformvaluesgetf90ptr)(ESMC_TransformValues **ptr, 
          int *numlist, struct c_F90ptr *src, struct c_F90ptr *dst, 
          struct c_F90ptr *w, int *status);

 void FTN(c_esmc_transformvaluesset)(ESMC_TransformValues **ptr, 
          int *numlist, ESMC_LocalArray **src, ESMC_LocalArray **dst, 
          ESMC_LocalArray **w, int *status);

 void FTN(c_esmc_transformvaluesvalidate)(ESMC_TransformValues **ptr, 
                                          char *opts, int *status);

 void FTN(c_esmc_transformvaluesprint)(ESMC_TransformValues **ptr, 
                                       char *opts, int *status);

 void FTN(c_esmc_routehandlecreate)(ESMC_RouteHandle **ptr, int *status);

 void FTN(c_esmc_routehandledestroy)(ESMC_RouteHandle **ptr, int *status);


 // the int needs to be an enum, the label needs to be added and handled
  void FTN(c_esmc_routehandleget)(ESMC_RouteHandle **ptr, int *htype, 
                          int *rt_count, ESMC_HandleMapping *rmaptype, 
                          int *which_rt, ESMC_Route **r,
                          int *tv_count, ESMC_HandleMapping *tvmaptype, 
                          int *which_tv, ESMC_TransformValues **tv, 
                          char *label, int *status, int labellen);

  // get a specific route
  void FTN(c_esmc_routehandlegetroute)(ESMC_RouteHandle **ptr, 
                                int *which_rt, ESMC_Route **r, int *status);

  // get a specific tv
  void FTN(c_esmc_routehandlegettv)(ESMC_RouteHandle **ptr, int *which_tv,
                                    ESMC_TransformValues **tv, int *status);

 // get just interesting numbers
  void FTN(c_esmc_routehandlegetinfo)(ESMC_RouteHandle **ptr, int *htype, 
                    int *rt_count, ESMC_HandleMapping *rmaptype,
                    int *tv_count, ESMC_HandleMapping *tvmaptype, int *status);

 void FTN(c_esmc_routehandleset)(ESMC_RouteHandle **ptr, 
      int *htype, int *rt_count, ESMC_HandleMapping *rmaptype, 
      int *which_rt, ESMC_Route **r, int *tv_count, 
      ESMC_HandleMapping *tvmaptype, int *which_tv, ESMC_TransformValues **tv,
      char *label, int *status, int labellen);

 void FTN(c_esmc_routehandlesettype)(ESMC_RouteHandle **ptr, 
                                     int *h, int *status);

 void FTN(c_esmc_routehandlesetroutecount)(ESMC_RouteHandle **ptr, 
                                                 int *rtcount, int *status);

 void FTN(c_esmc_routehandlesetrmaptype)(ESMC_RouteHandle **ptr, 
  			           ESMC_HandleMapping *rmaptype, int *status);

  void FTN(c_esmc_routehandlesetroute)(ESMC_RouteHandle **ptr, 
                                 int *which_rt, ESMC_Route **rh, int *status);

  void FTN(c_esmc_routehandlesettvcount)(ESMC_RouteHandle **ptr, 
	 				      int *tvcount, int *status);

  void FTN(c_esmc_routehandlesettvmaptype)(ESMC_RouteHandle **ptr, 
                                   ESMC_HandleMapping *tvmaptype, int *status);

  void FTN(c_esmc_routehandlesettvalues)(ESMC_RouteHandle **ptr, 
                     int *which_tv, ESMC_TransformValues **tv, int *status);

  void FTN(c_esmc_routehandlesetlabel)(ESMC_RouteHandle **ptr, 
                                            char *label, int *status, 
                                            int labellen);

  void FTN(c_esmc_routehandlevalidate)(ESMC_RouteHandle **ptr, char *opts,
                                            int *status);

  void FTN(c_esmc_routehandleprint)(ESMC_RouteHandle **ptr, char *opts, 
                                         int *status);

}  // end extern "C"

 #endif  // ESMC_RHandle_H
