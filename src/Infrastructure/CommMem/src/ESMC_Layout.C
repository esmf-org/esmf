// $Id: ESMC_Layout.C,v 1.17 2003/02/26 22:16:02 jwolfe Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC Layout method code (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Layout methods declared
// in the companion file ESMC_Layout.h
//
// < insert a paragraph or two explaining what you'll find in this file >
//
//-----------------------------------------------------------------------------
//
 // insert any higher level, 3rd party or system includes here
#include <iostream.h>  // cout
//#include <iostream> // TODO: use when namespaces consistently implemented
//using std::cout;
//using std::cerr;
//using std::endl;
#include <new>       // new, bad_alloc
#include <ESMC.h>

 // associated class definition file
 #include <ESMC_Layout.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Layout.C,v 1.17 2003/02/26 22:16:02 jwolfe Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Layout routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutCreate - Create a new 2D Layout from a simple DE list
//
// !INTERFACE:
      ESMC_Layout *ESMC_LayoutCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_Layout
//
// !ARGUMENTS:
      int nx,                    // in - number of DE's in the x direction
      int ny,                    // in - number of DE's in the y direction
      int *delist,               // in - 1 dimensional list of DE id's
                                 //      (e.g. mpi ranks)
      ESMC_CommHint_e commhint,  // in - fastest communication direction hint
      int *rc) {                 // out - return code
//
// !DESCRIPTION:
//      Allocates memory for a new Layout
//      object and uses the internal routine ESMC\_LayoutContruct to
//      initialize it. There can be multiple overloaded methods with the 
//      same name, but different argument lists.
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

  ESMC_Layout *layout;

  try {
    layout = new ESMC_Layout;
//cout << "ESMC_LayoutCreate() succesful\n";
    *rc = layout->ESMC_LayoutConstruct(nx, ny, delist, commhint);
    return(layout);
  }
  catch (...) {
// TODO:  call ESMF log/err handler
    cerr << "ESMC_LayoutCreate() memory allocation failed\n";
    *rc = ESMF_FAILURE;
    return(0);
  }

 } // end ESMC_LayoutCreate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutCreate - Create a new 3D Layout from a PE List
//
// !INTERFACE:
      ESMC_Layout *ESMC_LayoutCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_Layout
//
// !ARGUMENTS:
      int nx,                    // in - number of DE's in the x direction
      int ny,                    // in - number of DE's in the y direction
      int nz,                    // in - number of DE's in the z direction
      ESMC_PEList *pelist,       // in - PEList
      ESMC_CommHint_e commhint,  // in - fastest communication direction hint
      int *rc) {                 // out - return code
//
// !DESCRIPTION:
//      Allocates memory for a new Layout
//      object and uses the internal routine ESMC\_LayoutContruct to
//      initialize it. There can be multiple overloaded methods with the 
//      same name, but different argument lists.
//
//EOP
// !REQUIREMENTS:  AAAn.n.n

  ESMC_Layout *layout;

// TODO: ?? use exception handling when universally supported (pgCC doesn't)
#if 1
  try {
    layout = new ESMC_Layout;
//cout << "ESMC_LayoutCreate() succesful\n";
    *rc = layout->ESMC_LayoutConstruct(nx, ny, nz, pelist, commhint);
    return(layout);
  }
//  catch (bad_alloc) {  // TODO: use when IBM supports it (blackforest doesn't)
  catch (...) {
// TODO:  call ESMF log/err handler
    cerr << "ESMC_LayoutCreate() memory allocation failed\n";
    *rc = ESMF_FAILURE;
    return(0);
  }
#endif

#if 0
// use this section if exception handling not supported
// TODO:  IBM (blackforest) doesn't support "new (nothrow)"
  if ((layout = new (nothrow) ESMC_Layout) == 0) {
// TODO:  call ESMF log/err handler
    cerr << "ESMC_LayoutCreate() memory allocation failed\n";
    *rc = ESMF_FAILURE;
    return(0);
  }

//cout << "ESMC_LayoutCreate() succesful\n";
  *rc = layout->ESMC_LayoutConstruct(nx, ny, nz, pelist, commhint);
  return(layout);
#endif

 } // end ESMC_LayoutCreate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutDestroy - free a Layout created with Create
//
// !INTERFACE:
      int ESMC_LayoutDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_Layout *layout) {  // in - ESMC_Layout to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys a Layout object previously allocated
//      via an ESMC\_LayoutCreate routine.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//cout << "ESMC_LayoutDestroy, layout = " << layout << endl;
  if (layout != 0) {
    //layout->ESMC_LayoutDestruct(); constructor calls it!
    delete layout;
//cout << "ESMC_LayoutDestroy() successful\n";
    return(ESMF_SUCCESS);
  } else {
    return(ESMF_FAILURE);
  }

 } // end ESMC_LayoutDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutConstruct - Build a 2D DE topology from a DE list
//
// !INTERFACE:
      int ESMC_Layout::ESMC_LayoutConstruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int nx,                     // in - number of DE's in the x direction
      int ny,                     // in - number of DE's in the y direction
      int *delist,                // in - DEList
      ESMC_CommHint_e commhint) { // in - fastest communication direction hint
//
// !DESCRIPTION:
//      ESMF routine which fills in the contents of an already
//      allocated Layout object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC\_LayoutDestruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC\_LayoutCreate, which calls
//      ESMC\_LayoutConstruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

  // Initialize comm, PE, DE, Machine
  ESMC_LayoutInit();

  //
  // construct 2D array of ESMC_DE's
  //

  try {
    // construct 2D array of ESMC_DE's

    // first, create array of (nx) pointers to ESMC_DE pointers
    layout = new ESMC_DE**[nx];

    // then allocate an array of (ny) ESMC_DE pointers for each x pointer
    for (int i=0; i<nx; i++) {
      layout[i] = new ESMC_DE*[ny];
      // finally allocate a Z array of ESMC_DE's for each y pointer
      // only 1 long, since this version of Construct is 2D
      for (int j=0; j<ny; j++) {
        layout[i][j] = new ESMC_DE[1];
      }
    }
  }
  catch(...) {
// TODO:  call ESMF log/err handler
    cerr << "ESMC_LayoutConstruct() memory allocation failed\n";
    return(ESMF_FAILURE);
  }

  nxLayout = nx;
  nyLayout = ny;
  nzLayout = 1;
  //deList = delist;
  commHint = commhint;

  // TODO: ?? make commHint lookup table & share with Print()

  //
  // Assign DElist to DE's in layout according to communication hint.
  // Assume given DE list consists of unique DE ids that are pre-sorted
  // by fastest communication affinity (e.g. node, thread, process)
  //
  int ni, nj;  // loop limits
  int i, j;    // i outer loop, j inner loop (fastest)
  int *x, *y;  // layout coordinates to loop through
  switch (commHint)
  {
    case ESMC_XFAST:
    case ESMC_NOHINT:
      ni = nyLayout;  y = &i; // 2nd fastest (outer loop)
      nj = nxLayout;  x = &j; // fastest (inner loop)
      break;
    case ESMC_YFAST:
      ni = nxLayout; x = &i; // 2nd fastest (outer loop)
      nj = nyLayout; y = &j; // fastest (inner loop)
      break;
    default:
      break;
  }

//cout << "ESMC_LayoutConstruct() ni, nj " << ni << ", " << nj << endl;

  int DEix=0;
  for (i=0; i<ni; i++) {
    for(j=0; j<nj; j++) {
        // assign DE in given list to this DE in layout
//cout << "ESMC_LayoutConstruct(): " << i << ", " << j  << "\n";
//cout << "ESMC_LayoutConstruct(): " << *x<< ", " << *y << "\n";
        layout[*x][*y][0].ESMC_DESetESMFID(delist[DEix++]);
    }
  }

//cout << "ESMC_LayoutConstruct() successful\n";
  return(ESMF_SUCCESS);

 } // end ESMC_LayoutConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutConstruct - Build a 3D DE topology from a PE list
//
// !INTERFACE:
      int ESMC_Layout::ESMC_LayoutConstruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int nx,                     // in - number of DE's in the x direction
      int ny,                     // in - number of DE's in the y direction
      int nz,                     // in - number of DE's in the z direction
      ESMC_PEList *pelist,        // in - PEList
      ESMC_CommHint_e commhint) { // in - fastest communication direction hint
//
// !DESCRIPTION:
//      ESMF routine which fills in the contents of an already
//      allocated Layout object.  May need to do additional allocations
//      as needed.  Must call the corresponding ESMC\_LayoutDestruct
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use ESMC\_LayoutCreate, which calls
//      ESMC\_LayoutConstruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

  // Initialize comm, PE, DE, Machine
  ESMC_LayoutInit();

  // construct 3D array of ESMC_DE's

// TODO: ?? use exception handling when universally supported (pgCC doesn't)
#if 1
  try {
    // construct 2D array of ESMC_DE's

    // first, create array of (nx) pointers to ESMC_DE pointers
    layout = new ESMC_DE**[nx];

    // then allocate an array of (ny) ESMC_DE pointers for each x pointer
    for (int i=0; i<nx; i++) {
      layout[i] = new ESMC_DE*[ny];
      // finally allocate an array of ESMC_DE's for each y pointer
      for (int j=0; j<ny; j++) {
        layout[i][j] = new ESMC_DE[nz];
      }
    }
  }
//  catch(bad_alloc) {  // TODO: use when IBM supports it (blackforest doesn't)
  catch(...) {
// TODO:  call ESMF log/err handler
    cerr << "ESMC_LayoutConstruct() memory allocation failed\n";
    return(ESMF_FAILURE);
  }
#endif

#if 0
// use this section if exception handling not supported
// TODO:  IBM (blackforest) doesn't support "new (nothrow)"
  // first, create array of (nx) pointers to ESMC_DE pointers
  if((layout = new (nothrow) ESMC_DE**[nx]) == 0) {
// TODO:  call ESMF log/err handler
    cerr << "ESMC_LayoutConstruct() memory allocation failed\n";
    return(ESMF_FAILURE);
  }

  // then allocate an array of (ny) ESMC_DE pointers for each x pointer
  for (int i=0; i<nx; i++) {
    if ((layout[i] = new (nothrow) ESMC_DE*[ny]) == 0) {
  // TODO:  call ESMF log/err handler
      cerr << "ESMC_LayoutConstruct() memory allocation failed\n";
      return(ESMF_FAILURE);
    }

    // finally allocate an array of (nz) ESMC_DE's for each y pointer
    for (int j=0; j<ny; j++) {
      if ((layout[i][j] = new (nothrow) ESMC_DE[nz]) == 0) {
    // TODO:  call ESMF log/err handler
        cerr << "ESMC_LayoutConstruct() memory allocation failed\n";
        return(ESMF_FAILURE);
      }
    }
  }
#endif

  nxLayout = nx;
  nyLayout = ny;
  nzLayout = nz;
  peList = pelist;
  commHint = commhint;

  // TODO: ?? make commHint lookup table & share with Print()

  // assign PE's to DE's in layout according to communication hint
  int ni, nj, nk;
  int i, j, k;
  int *x, *y, *z;
  switch (commHint)
  {
    case ESMC_XFAST:
    case ESMC_NOHINT:
      ni = nzLayout;  z = &i; // 3rd fastest (for outer loop)
      nj = nyLayout;  y = &j; // 2nd fastest (for middle loop)
      nk = nxLayout;  x = &k; // fastest (for inner loop)
      break;
    case ESMC_YFAST:
      ni = nzLayout; z = &i;
      nj = nxLayout; x = &j;
      nk = nyLayout; y = &k;
      break;
    case ESMC_ZFAST:
      ni = nyLayout; y = &i;
      nj = nxLayout; x = &j;
      nk = nzLayout; z = &k;
      break;
    default:
      break;
  }

//cout << "ESMC_LayoutConstruct() ni, nj, nk: "
          //<< ni << ", " << nj << ", " << nk << endl;

  int PEix=0;
  ESMC_PE *pe;
  for (i=0; i<ni; i++) {
    for(j=0; j<nj; j++) {
      for(k=0; k<nk; k++) {

        // retrieve next PE from our list
        peList->ESMC_PEListGetPE(PEix++, &pe);

        // then assign it to this DE
//cout << "ESMC_LayoutConstruct(): " << i << ", " << j << ", " << k << "\n";
//cout << "ESMC_LayoutConstruct(): " << *x<< ", " << *y<< ", " << *z<< "\n";
        layout[*x][*y][*z].ESMC_DESetPE(pe);
      }
    }
  }

//cout << "ESMC_LayoutConstruct() successful\n";
  return(ESMF_SUCCESS);

 } // end ESMC_LayoutConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutDestruct - release resources associated w/a Layout
//
// !INTERFACE:
      int ESMC_Layout::ESMC_LayoutDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      ESMF\_LayoutConstruct, does any additional cleanup before the
//      original Layout object is freed.  Intended for internal ESMF
//      use only; end-users use ESMC\_LayoutDestroy, which calls
//      ESMC\_LayoutDestruct.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//
//cout << "ESMC_LayoutDestruct() invoked\n";

  // first delete each array of (ny) ESMC_DE's for each x pointer
  for (int i=0; i<nxLayout; i++) {
    for (int j=0; j<nyLayout; j++) {
      // delete array of ESMC_DE's in z direction
      delete[] layout[i][j];
    }
    // then delete array of ny pointers
    delete[] layout[i];
  }
  // finally delete the array of (nx) pointers
  delete[] layout;

  layout = 0;
  peList = 0;
  nxLayout = 0;
  nyLayout = 0;
  nzLayout = 0;
  commHint = ESMC_NOHINT;

  return(ESMF_SUCCESS);

 } // end ESMC_LayoutDestruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutInit - initializes a Layout object
//
// !INTERFACE:
      int ESMC_Layout::ESMC_LayoutInit(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) {
//
// !DESCRIPTION:
//      ESMF routine which only initializes Layout values; it does not
//      allocate any resources.  Define for shallow classes only,
//      for deep classes define and use routines Create/Destroy and
//      Construct/Destruct.  Can be overloaded like ESMC\_LayoutCreate.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

  // initialize to an empty layout 
  layout = 0;
  nxLayout = 0;
  nyLayout = 0;
  nzLayout = 0;
  peList = 0;
  commHint = ESMC_NOHINT;

  //
  // initialize my DE, PE, Comm, and Machine model
  //
  int argc = 0;    // TODO pass into LayoutCreate ?
  char **argv = 0; // TODO pass into LayoutCreate ?
  int myDEid=0;

  myDE.ESMC_DESetType(ESMC_PROCESS); // TODO: auto determine proc or thread,
                                     //       or get from config file ?
  comm.ESMC_CommInit(&argc, &argv, &myDE); // computes unique ESMF DE id
  myPE.ESMC_PEInit(&Mach);        // gets cpu, node ids from machine
  myDE.ESMC_DEGetESMFID(&myDEid);
  myPE.ESMC_PESetEsmfID(myDEid);  // assume 1-to-1 DE-to-PE for now TODO: ?
//cout << "myDEid = " << myDEid << "\n";

#if 0
  int mypeid=0, mycpuid=0, mynodeid=0;
  myPE.ESMC_PEGetEsmfID(&mypeid); //   (mypeid = myDEid)
  myPE.ESMC_PEGetCpuID(&mycpuid);
  myPE.ESMC_PEGetNodeID(&mynodeid);
cout << "mypeid, mycpuid, mynodeid = " << mypeid << "," << mycpuid << ", "
       << mynodeid << "\n";
#endif

  return(ESMF_SUCCESS);

 } // end ESMC_LayoutInit

#if 0
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutGetConfig - get configuration info from a Layout
//
// !INTERFACE:
      int ESMC_Layout::ESMC_LayoutGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_LayoutConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the Layout object was configured with.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_LayoutGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutSetConfig - set configuration info for a Layout
//
// !INTERFACE:
      int ESMC_Layout::ESMC_LayoutSetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_LayoutConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the Layout object with set of resources given.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_LayoutSetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutGet<Value> - get <Value> for a Layout
//
// !INTERFACE:
      int ESMC_Layout::ESMC_LayoutGet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of Layout member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_LayoutGet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutSet<Value> - set <Value> for a Layout
//
// !INTERFACE:
      int ESMC_Layout::ESMC_LayoutSet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the Layout member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_LayoutSet<Value>
#endif

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutGetSize - get (nx,ny) size of 2D Layout
//
// !INTERFACE:
      int ESMC_Layout::ESMC_LayoutGetSize(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *nx,             // out - number of DE's in x direction
      int *ny) const {     // out - number of DE's in y direction
//
// !DESCRIPTION:
//    returns overall x,y dimensions of 2D Layout
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

  *nx = nxLayout;
  *ny = nyLayout;

  return(ESMF_SUCCESS);

 } // end ESMC_LayoutGetSize

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutGetSize - get (nx,ny,nz) size of Layout
//
// !INTERFACE:
      int ESMC_Layout::ESMC_LayoutGetSize(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *nx,             // out - number of DE's in x direction
      int *ny,             // out - number of DE's in y direction
      int *nz) const {     // out - number of DE's in z direction
//
// !DESCRIPTION:
//    returns overall x,y,z dimensions of Layout
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

  *nx = nxLayout;
  *ny = nyLayout;
  *nz = nzLayout;

  return(ESMF_SUCCESS);

 } // end ESMC_LayoutGetSize

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutGetDEPosition - get x,y position of my DE in 2D Layout
//
// !INTERFACE:
      int ESMC_Layout::ESMC_LayoutGetDEPosition(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *x,             // out - x position of DE in layout
      int *y) const {     // out - y position of DE in layout
//
// !DESCRIPTION:
//    returns (x,y) position of my DE in 2D layout
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

  // linear search for DE TODO: compute once on initialization ?
  for (int i=0; i<nxLayout; i++) {
    for (int j=0; j<nyLayout; j++) {
      if (myDE.esmfID == layout[i][j][0].esmfID) {
        // found -- return (x,y) position
        *x = i;
        *y = j;
        return(ESMF_SUCCESS);
      }
    }
  }

  // not found - return error
  return(ESMF_FAILURE);

 } // end ESMC_LayoutGetDEPosition

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutGetDEPosition - get (x,y,z) position of DE in Layout
//
// !INTERFACE:
      int ESMC_Layout::ESMC_LayoutGetDEPosition(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_DE *de,        // in  - given DE
      int *x,             // out - x position of DE in layout
      int *y,             // out - y position of DE in layout
      int *z) const {     // out - z position of DE in layout
//
// !DESCRIPTION:
//    returns (x,y,z) position of given DE in layout
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

  // linear search for DE
  for (int i=0; i<nxLayout; i++) {
    for (int j=0; j<nyLayout; j++) {
      for (int k=0; k<nzLayout; k++) {
        if (de->esmfID == layout[i][j][k].esmfID) {
          // found -- return (x,y,z) position
          *x = i;
          *y = j;
          *z = k;
          return(ESMF_SUCCESS);
        }
      }
    }
  }

  // not found - return error
  return(ESMF_FAILURE);

 } // end ESMC_LayoutGetDEPosition

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutGetDEid - get id of our DE
//
// !INTERFACE:
      int ESMC_Layout::ESMC_LayoutGetDEid(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *deid) const {     // out - our DE id
//
// !DESCRIPTION:
//    Returns our DE id
//EOP
// !REQUIREMENTS:  developer's guide for classes

  myDE.ESMC_DEGetESMFID(deid);

  return(ESMF_SUCCESS);

 } // end ESMC_LayoutGetDEid

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutSetAxisIndex - set an axis index from a layout
//
// !INTERFACE:
      int ESMC_Layout::ESMC_LayoutSetAxisIndex(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int global_counts[],      // in  - total (global) number of
                                //       elements per axis (array)
      int size_gcount,          // in  - size of global_counts array
      int decompids[],          // in  - decomposition identifier for each
                                //       axis (array)
      int size_decomp,          // in  - size of decomp array
      ESMC_AxisIndex *AIPtr) {  // out - pointer to array of AxisIndex
                                //       structures
//
// !DESCRIPTION:
//    returns an array of AxisIndex types representing the decomposition of
//    an arbitrary number of axis by a layout
//
//EOP
// !REQUIREMENTS:

// check and make sure global_counts and decompids arrays are the same size
  if (size_gcount != size_decomp) {
    return(ESMF_FAILURE);
  }

  int x, y;
  this->ESMC_LayoutGetDEPosition( &x, &y);
  // loop to set AxisIndex array
  for (int i=0; i<size_gcount; i++, AIPtr++) {
    AIPtr->decomp = decompids[i];
    // check if decomp is out of bounds
    if ((decompids[i] < 0) || (decompids[i] > 2)) {
      return(ESMF_FAILURE);
    }
    // if decomp is 0, no decomposition of the axis
    if (decompids[i] == 0) {
      AIPtr->l = 0;
      AIPtr->r = global_counts[i]-1;
      AIPtr->max = global_counts[i];
    }
    // if decomp is 1, use nxLayout
    if (decompids[i] == 1) {
      int n1 = (global_counts[i]+nxLayout-1)/nxLayout; // round to nearest
      AIPtr->l = 0;
      AIPtr->r = n1;
      AIPtr->max = global_counts[i];
      AIPtr->gstart = x*n1;
    }
    // if decomp is 2, use nyLayout
    if (decompids[i] == 2) {
      int n2 = (global_counts[i]+nyLayout-1)/nyLayout; // round to nearest
      AIPtr->l = 0;
      AIPtr->r = n2;
      AIPtr->max = global_counts[i];
      AIPtr->gstart = y*n2;
    }
  }
   
  return(ESMF_SUCCESS);

 } // end ESMC_LayoutSetAxisIndex

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutGatherArrayI - gather a distributed integer array
//
// !INTERFACE:
      int ESMC_Layout::ESMC_LayoutGatherArrayI(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *DistArray,            // in  - distributed array
      int decompids[],           // in  - decomposition identifier for each
                                 //       axis for the array
      int size_decomp,           // in  - size of decomp arrays
      ESMC_AxisIndex *AIPtr,     // in  - pointer to array of AxisIndex
                                 //       structures for exclusive data
      ESMC_AxisIndex *AIPtr2,    // in  - pointer to array of AxisIndex
                                 //       structures for total data
      int *GlobalArray) {        // out - global array
//
// !DESCRIPTION:
//    returns an array of AxisIndex types representing the decomposition of
//    an arbitrary number of axis by a layout
//
//EOP
// !REQUIREMENTS:

  int rc = ESMF_FAILURE;
  int i, j, k, l, m;     // general counter vars

  // get layout size
  int nx, ny;
  this->ESMC_LayoutGetSize(&nx, &ny);
  int nde = nx*ny;
  int rankx, ranky;

  // switch based on array rank
  switch (size_decomp) {
    case 1:
      cout << "no code to handle array rank " << size_decomp << " yet\n";
    break;
    case 2:
      {
        // figure out which ranks are decomposed and figure out the
        // number of separate data chunks per rank and size of data
        // chunks
        int rmax[2];
        int rsize[2];
        int rsize_tot[2];
        int rskip[2];
        int rbreak[1];
        int rbcount = 0;
        for (i=0; i<size_decomp; i++) {
          rmax[i] = AIPtr[i].max;
          rsize[i] = AIPtr[i].r - AIPtr[i].l + 1;
          rsize_tot[i] = AIPtr2[i].r - AIPtr2[i].l + 1;
          if (decompids[i] == 0) {
            rbreak[rbcount]=i;
            rbcount++;
          }
          if (decompids[i] == 1) {
            rankx = i;
          }
          if (decompids[i] == 2) {
            ranky = i;
            rbreak[rbcount]=i;
            rbcount++;
          }
        }
        rskip[0] = 1;
        for (i=1; i<size_decomp; i++) {
          rskip[i] = rskip[i-1]*rmax[i-1];
        }
        // loop over ranks, skipping the first decomposed one, loading
        // up chunks of data to gather
        int k, j_tot;
        int *sendbuf, *recvbuf;
        int sendcount;
        int* recvcounts = new int[nde];
        int* displs = new int[nde];
        for (int j=0; j<rsize[rbreak[0]]; j++) {
          j_tot = j + AIPtr[ranky].l;
          sendbuf = &DistArray[j_tot*rsize_tot[rankx] + AIPtr[rankx].l];
          sendcount = rsize[rankx];
          recvbuf = &GlobalArray[j*rmax[rankx]];
          for (int kx=0; kx<nx; kx++) {
            for (int ky=0; ky<ny; ky++) {
              k = ky*nx + kx;
              recvcounts[k] = rsize[rankx]; // TODO: fix so variable
              displs[k] = kx*rsize[rankx] + ky*rskip[ranky]*rsize[ranky];
            }
          }
          // call layout gather routine
          comm.ESMC_CommAllGatherV(sendbuf, sendcount, recvbuf, recvcounts, 
                                   displs, ESMC_INT);
        }
        delete [] recvcounts;
        delete [] displs;
      }
    break;
    case 3:
      {
        // figure out which ranks are decomposed and figure out the
        // number of separate data chunks per rank and size of data
        // chunks
        int rmax[3];
        int rsize[3];
        int rskip[3];
        int rbreak[2];
        int rbcount = 0;
        for (i=0; i<size_decomp; i++) {
          rmax[i] = AIPtr[i].max;
          rsize[i] = AIPtr[i].r - AIPtr[i].l + 1;
          if (decompids[i] == 0) {
            rbreak[rbcount]=i;
            rbcount++;
          }
          if (decompids[i] == 1) {
            rankx = i;
          }
          if (decompids[i] == 2) {
            ranky = i;
            rbreak[rbcount]=i;
            rbcount++;
          }
        }
        rskip[0] = 1;
        for (i=1; i<size_decomp; i++) {
          rskip[i] = rskip[i-1]*rmax[i-1];
        }
        // loop over ranks, skipping the first decomposed one, loading
        // up chunks of data to gather
        int k;
        int *sendbuf, *recvbuf;
        int sendcount;
        int* recvcounts = new int[nde];
        int* displs = new int[nde];
        for (i=0; i<rsize[rbreak[1]]; i++) {
          for (int j=0; j<rsize[rbreak[0]]; j++) {
            sendbuf = &DistArray[j*rsize[rankx]
                    + i*rsize[rankx]*rsize[rbreak[0]]];
            sendcount = rsize[rankx];
            recvbuf = &GlobalArray[j*rmax[rankx]
                    + i*rmax[rankx]*rmax[rbreak[0]]];
            for (int kx=0; kx<nx; kx++) {
              for (int ky=0; ky<ny; ky++) {
                k = ky*nx + kx;
                recvcounts[k] = rsize[rankx]; // TODO: fix so variable
                displs[k] = kx*rsize[rankx] + ky*rskip[ranky]*rsize[ranky];
              }
            }
          // call layout gather routine
          comm.ESMC_CommAllGatherV(sendbuf, sendcount, recvbuf, recvcounts, 
                                   displs, ESMC_INT);
          }
        }
        delete [] recvcounts;
        delete [] displs;
      }
    break;
    case 4:
    break;
    case 5:
      cout << "no code to handle array rank " << size_decomp << " yet\n";
    break;
    default:
      cout << "no code to handle array rank " << size_decomp << " yet\n";
    break;
  }

  rc = ESMF_SUCCESS;
  return rc;

 } // end ESMC_LayoutGatherArray

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutValidate - internal consistency check for a Layout
//
// !INTERFACE:
      int ESMC_Layout::ESMC_LayoutValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a Layout is internally consistent.
//      Returns error code if problems are found.  ESMC\_Base class method.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

  return(ESMF_SUCCESS);

 } // end ESMC_LayoutValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutPrint - print contents of a Layout
//
// !INTERFACE:
      int ESMC_Layout::ESMC_LayoutPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a Layout.  The options control the
//      type of information and level of detail.  ESMC\_Base class method.
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

  //cout << "nxLayout, nyLayout, nzLayout = " << nxLayout << "," << nyLayout << "," << nzLayout << endl;
  //cout << "commHint = " << commHint << "\n";

  int i,j,k;
  int ni,nj,nk;
  int *x, *y, *z;

  // TODO:  ?? create commHint lookup table & share with Construct()

  switch (commHint)
  {
    case ESMC_XFAST:
    case (ESMC_NOHINT):
      ni = nzLayout; z = &i; // outer loop
      nj = nyLayout; y = &j; // middle loop
      nk = nxLayout; x = &k; // inner loop
      //cout << "i=z, j=y, k=x \n";
      break;
    case (ESMC_YFAST):
      ni = nzLayout; z = &i;
      nj = nxLayout; x = &j;
      nk = nyLayout; y = &k;
      //cout << "i=z, j=x, k=y \n";
      break;
    case (ESMC_ZFAST):
      ni = nyLayout; y = &i;
      nj = nxLayout; x = &j;
      nk = nzLayout; z = &k;
      //cout << "i=y, j=x, k=z \n";
      break;
  }

  for(i=0; i<ni; i++) {
    for (j=0; j<nj; j++) {
      for (k=0; k<nk; k++) {
        //cout << "layout[" << i << "][" << j << "][" << k << "] = ";
        cout << "layout[" << *x<< "][" << *y<< "][" << *z<< "] = ";
        layout[*x][*y][*z].ESMC_DEPrint();
      }
    }
  }

  return(ESMF_SUCCESS);

 } // end ESMC_LayoutPrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_Layout - native C++ constructor
//
// !INTERFACE:
      ESMC_Layout::ESMC_Layout(
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
      void) {  // in
//
// !DESCRIPTION:
//      Calls standard ESMF deep or shallow methods for initialization
//      with default or passed-in values
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//cout << "ESMC_Layout constructor invoked\n";

  // Initialize comm, PE, DE, Machine
  ESMC_LayoutInit();

 } // end ESMC_Layout

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_Layout - native C++ destructor
//
// !INTERFACE:
      ESMC_Layout::~ESMC_Layout(void) {
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Calls standard ESMF deep or shallow methods for destruction
//
//EOP
// !REQUIREMENTS:  SSSn.n, GGGn.n

//cout << "~ESMC_Layout() invoked\n";
  ESMC_LayoutDestruct();

 } // end ~ESMC_Layout

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutAllGatherVI - Perform MPI-like Allgatherv of equally-sized integer data arrays across a layout
//
// !INTERFACE:
      int ESMC_Layout::ESMC_LayoutAllGatherVI(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *sndArray,           // in  - integer send data array
      int  sndLen,             // in  - length of send data array
      int *rcvArray,           // out - gathered data array
      int *rcvLen,             // in  - array of receive data array lengths
      int *rcvDispls) {        // in  - array of rcvArray displacements
//
// !DESCRIPTION:
//    Perform MPI-like Allgatherv of integer data arrays
//    across all DEs in a layout
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

  // TODO: make comm public and invoke directly rather than at Layout level ?
  //       (does not depend on any Layout knowledge)

  // perform Allgatherv operation across all DEs in the layout
  int rc;
  rc = comm.ESMC_CommAllGatherV(sndArray, sndLen, rcvArray, rcvLen, rcvDispls,
                                ESMC_INT);
  if (rc != ESMF_SUCCESS) {
    cout << "ESMC_LayoutAllGatherVI() error" << endl;
  }

  return(rc);

 } // end ESMC_LayoutAllGatherVI

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutAllReduce - reduce 1D integer data array across layout
//
// !INTERFACE:
      int ESMC_Layout::ESMC_LayoutAllReduce(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *dataArray,          // in  - 1D integer data array
      int *result,             // out - single integer value
      int arrayLen,            // in  - length of dataArray
      ESMC_Op_e op)  {         // in  - reduction operation (sum, min, max ...)
//
// !DESCRIPTION:
//      performs requested reduction operation on given data array across
//      all DEs in layout
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

  // TODO: make comm public and invoke directly rather than at Layout level ?
  //       (does not depend on any Layout knowledge)

  // TODO: put this loop logic in ESMC_Comm ?
  // perform reduction operation within our DE (given dataArray)
  int localResult = 0;
  for(int i=0; i<arrayLen; i++) {
    switch (op)
    {
      case ESMC_SUM:
        localResult += dataArray[i];
        break;
      default:
        break;
    }
  }

  // cout << "ESMC_LayoutAllReduce localResult = " << localResult << endl;

  // perform reduction operation across all DEs in the layout
  int rc;
  rc = comm.ESMC_CommAllReduce(&localResult, result, 1, ESMC_INT, op);
  if (rc != ESMF_SUCCESS) {
    cout << "ESMC_LayoutAllReduce(1D) error" << endl;
  }

  return(rc);

 } // end ESMC_LayoutAllReduce
