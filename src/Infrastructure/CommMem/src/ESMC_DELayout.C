// $Id: ESMC_DELayout.C,v 1.34 2003/07/11 01:00:00 eschwab Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC DELayout method code (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ DELayout methods declared
// in the companion file ESMC_DELayout.h
//
// 
//
//-----------------------------------------------------------------------------
//
#include <iostream.h>  // cout
#include <stdio.h>
//#include <iostream>  // TODO: use when namespaces consistently implemented
//using std::cout;
//using std::cerr;
//using std::endl;
#include <new>         // new, bad_alloc
#include <ESMC.h>
#include <mpi.h>

 // associated class definition file
 #include <ESMC_DELayout.h>

#define ESMF_MPI_TAG 1

 // single place to turn on and off debug messages; when real error & logging
 // is available replace this.
static int verbose = 1;

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
           "$Id: ESMC_DELayout.C,v 1.34 2003/07/11 01:00:00 eschwab Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the DELayout routines
//
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutCreate - Create a default DELayout
//
// !INTERFACE:
      ESMC_DELayout *ESMC_DELayoutCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_DELayout
//
// !ARGUMENTS:
      int *rc) {                 // out - return code
//
// !DESCRIPTION:
//      Allocates memory for a new DELayout using a self-discovered
//      PEList
//
//EOP

  ESMC_DELayout *layout;

  try {
    layout = new ESMC_DELayout;
      //cout << "ESMC_DELayoutCreate() successful\n";
    *rc = layout->ESMC_DELayoutConstruct();
    return(layout);
  }
  catch (...) {
    // TODO:  call ESMF log/err handler
    cerr << "ESMC_DELayoutCreate() memory allocation failed\n";
    *rc = ESMF_FAILURE;
    return(ESMC_NULL_POINTER);
  }
 } // end ESMC_DELayoutCreate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutCreate - Create a new DELayout from a parent DELayout
//
// !INTERFACE:
      ESMC_DELayout *ESMC_DELayoutCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_DELayout
//
// !ARGUMENTS:
      ESMC_DELayout *parent,          // in - parent DELayout
      int *parent_offsets,            // in - offsets from start of parent axes
      int *de_indices,                // in - null-terminated list of parent DE 
                                      //      indices for new delayout
      int ndim,                       // in - number of dimensions
      int *lengths,                   // in - number of DEs in each dimension
      ESMC_CommType *commtypes,       // in - array of communication types
      int *rc) {                      // out - return code
//
// !DESCRIPTION:
//  Create a new {\tt ESMC\_DELayout} using a parent {\tt ESMC\_DELayout}'s {\tt ESMC\_DE}s. 
//
//EOP

  ESMC_DELayout *layout;

// TODO: ?? use exception handling when universally supported (pgCC doesn't)
  try {
    layout = new ESMC_DELayout;
    //cout << "ESMC_DELayoutCreate() successful\n";
    *rc = layout->ESMC_DELayoutConstruct(parent, parent_offsets, de_indices, 
					 ndim, lengths, commtypes);
    return(layout);
  }
  catch (...) {
//  catch (bad_alloc) {  // TODO: use when IBM supports it (blackforest doesn't)
// TODO:  call ESMF log/err handler
    cerr << "ESMC_DELayoutCreate() memory allocation failed\n";
    *rc = ESMF_FAILURE;
    return(ESMC_NULL_POINTER);
  }

 } // end ESMC_DELayoutCreate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutCreate - Create a new DELayout from a PE List
//
// !INTERFACE:
      ESMC_DELayout *ESMC_DELayoutCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_DELayout
//
// !ARGUMENTS:
      ESMC_PEList *pelist,       // in - PEList
      int ndim, 		 // in - number of dimensions		
      int *lengths,              // in - number of DEs in each dimension
      ESMC_CommType *commtypes,  // in - array of communication types
      int *rc) {                 // out - return code
//
// !DESCRIPTION:
//      Allocates memory for a new {\tt ESMC\_DELayout}
//      object and uses the internal routine {\tt ESMC\_DELayoutConstruct} to
//      initialize it. There can be multiple overloaded methods with the 
//      same name, but different argument lists.
//
//EOP


  ESMC_DELayout *layout;

// TODO: ?? use exception handling when universally supported (pgCC doesn't)
#if 1
  try {
    layout = new ESMC_DELayout;
//cout << "ESMC_DELayoutCreate() successful\n";
    *rc = layout->ESMC_DELayoutConstruct(pelist, ndim, lengths, commtypes);
    return(layout);
  }
//  catch (bad_alloc) {  // TODO: use when IBM supports it (blackforest doesn't)
  catch (...) {
// TODO:  call ESMF log/err handler
    cerr << "ESMC_DELayoutCreate() memory allocation failed\n";
    *rc = ESMF_FAILURE;
    return(ESMC_NULL_POINTER);
  }
#endif

#if 0
// use this section if exception handling not supported
// TODO:  IBM (blackforest) doesn't support "new (nothrow)"
  if ((layout = new (nothrow) ESMC_DELayout) == 0) {
// TODO:  call ESMF log/err handler
    cerr << "ESMC_DELayoutCreate() memory allocation failed\n";
    *rc = ESMF_FAILURE;
    return(ESMC_NULL_POINTER);
  }

//cout << "ESMC_DELayoutCreate() successful\n";
  *rc = layout->ESMC_DELayoutConstruct(pelist, ndim, lengths, commtypes);
  return(layout);
#endif

 } // end ESMC_DELayoutCreate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutCreate - Create a new DELayout from a DE array
//
// !INTERFACE:
      ESMC_DELayout *ESMC_DELayoutCreate(
//
// !RETURN VALUE:
//     pointer to newly allocated ESMC_DELayout
//
// !ARGUMENTS:
      int *delist,               // in - array of DE indices
      int ndim, 		 // in - number of dimensions		
      int *lengths,              // in - number of DEs in each dimension
      ESMC_CommType *commtypes,  // in - array of communication types
      int *rc) {                 // out - return code
//
// !DESCRIPTION:
//      Allocates memory for a new {\tt ESMC\_DELayout}
//      object and uses the internal routine {\tt ESMC\_DELayoutConstruct} to
//      initialize it. There can be multiple overloaded methods with the 
//      same name, but different argument lists.
//
//EOP

//  TODO:  This is a malformed function, in that it needs to be a DE list
//   *relative* to another layout, or maybe by simple implication to a 1xN
//   layout which is a global or something.   for now that is what is assumed,
//   but create by PE list or by DE numbers relative to another layout are
//   the preferred ways to create another layout.

  ESMC_DELayout *layout;

// TODO: ?? use exception handling when universally supported (pgCC doesn't)
#if 1
  try {
    layout = new ESMC_DELayout;
//cout << "ESMC_DELayoutCreate() successful\n";
    *rc = layout->ESMC_DELayoutConstruct(delist, ndim, lengths, commtypes);
    return(layout);
  }
//  catch (bad_alloc) {  // TODO: use when IBM supports it (blackforest doesn't)
  catch (...) {
// TODO:  call ESMF log/err handler
    cerr << "ESMC_DELayoutCreate() memory allocation failed\n";
    *rc = ESMF_FAILURE;
    return(ESMC_NULL_POINTER);
  }
#endif

#if 0
// use this section if exception handling not supported
// TODO:  IBM (blackforest) doesn't support "new (nothrow)"
  if ((layout = new (nothrow) ESMC_DELayout) == 0) {
// TODO:  call ESMF log/err handler
    cerr << "ESMC_DELayoutCreate() memory allocation failed\n";
    *rc = ESMF_FAILURE;
    return(ESMC_NULL_POINTER);
  }

//cout << "ESMC_DELayoutCreate() successful\n";
  *rc = layout->ESMC_DELayoutConstruct(delist, ndim, lengths, commtypes);
  return(layout);
#endif

 } // end ESMC_DELayoutCreate

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutDestroy - free a DELayout created with Create
//
// !INTERFACE:
      int ESMC_DELayoutDestroy(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_DELayout *layout) {  // in - ESMC_DELayout to destroy
//
// !DESCRIPTION:
//      ESMF routine which destroys a DELayout object previously allocated
//      via an {\tt ESMC\_DELayoutCreate} routine.  Define for deep classes only.
//
//EOP

//cout << "ESMC_DELayoutDestroy, layout = " << layout << endl;
  if (layout != ESMC_NULL_POINTER) {
    //layout->ESMC_DELayoutDestruct(); constructor calls it!
    delete layout;
    layout = ESMC_NULL_POINTER;
//cout << "ESMC_DELayoutDestroy() successful\n";
    return(ESMF_SUCCESS);
  } else {
    return(ESMF_FAILURE);
  }

 } // end ESMC_DELayoutDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutConstruct - Build a 1D DELayout from a
//                                    self-discovered PEList
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutConstruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMC routine which fills in the contents of an already
//      allocated DELayout object.  May need to do additional allocations
//      as needed.  Must call the corresponding {\tt ESMC\_DELayoutDestruct}
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use {\tt ESMC\_DELayoutCreate}, which calls
//      {\tt ESMC\_DELayoutConstruct}.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  

  //
  // self discover the PEList by sharing our PE with all other DEs
  //
  // TODO: currently, this works only in the default case of all MPI DEs: no
  //  threads
  //
  int i, myEsmfID;
  MPI_Group mpigroup;

  // Initialize comm, PE, DE, Machine
  ESMC_DELayoutInit();

  ndim = 1;
  parent=ESMC_NULL_POINTER;

  // get total number of DEs
  comm.ESMC_CommGetNumDEs(&nDEs);
  //cout << "DELayoutCreate(void) comm group size = " << nDEs << endl;

  // Construct the sorted PE list object.  When this returns, peList is valid.
  ESMC_DELayoutSetPEList();

  // construct 1D array of ESMC_DE's
  try {
    // first, create array of (nx) pointers to ESMC_DE pointers
    layout = new ESMC_DE**[nDEs];

    // then allocate ny=1, nz=1 DEs
    for (i=0; i<nDEs; i++) {
      layout[i] = new ESMC_DE*[1];
      layout[i][0] = new ESMC_DE[1];
    }
  }
  catch(...) {
//  catch(bad_alloc) {  // TODO: use when IBM supports it (blackforest doesn't)
    // TODO:  call ESMF log/err handler
    cerr << "ESMC_DELayoutConstruct() memory allocation failed\n";
    return(ESMF_FAILURE);
  }

  this->length = new int[ESMF_MAXDECOMPDIM];
  this->length[0] = nDEs;
  for (i=1; i<ESMF_MAXDECOMPDIM; i++)
      this->length[i] = 1;

  ESMC_PE *pe;
  for (i=0; i<nDEs; i++) {
    // retrieve next PE from our list
    peList->ESMC_PEListGetPE(i, &pe);

    // then assign it to this DE
    layout[i][0][0].ESMC_DESetPE(pe);

    // assign local ESMF id in DE struct from 0 to nDEs - 1
    layout[i][0][0].ESMC_DESetESMFID(i);

    //layout[i][0][0].ESMC_DEPrint();
  }

  // TODO: create true child communicators 
  MPI_Comm_group(MPI_COMM_WORLD, &mpigroup);
  MPI_Comm_create(MPI_COMM_WORLD, mpigroup, &decomm.mpicomm);
  // we need to call the comm with the child comms, not world

  // TODO: revisit this, but for now make the default commtype MPI
  this->commType = new ESMC_CommType[ESMF_MAXDECOMPDIM];
  for (i=0; i<ESMF_MAXDECOMPDIM; i++)
      this->commType[i] = ESMC_COMMTYPE_SHR;

  if (verbose) {
    cout << "ESMC_DELayoutConstruct (default 1xN) successful\n";
    //cout << "Create default 1xN Layout:" << endl;
    //ESMC_DELayoutPrint();
  }

  return(ESMF_SUCCESS);

 } // end ESMC_DELayoutConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutConstruct - Build a new DELayout from a parent
//                                   
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutConstruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_DELayout *inparent,       // in/out - parentDELayout
      int *parent_offsets,           // in - offsets from start of parent axes
      int *de_indices,               // in - null-ternminated list of parent DE 
                                     //      indices for new delayout
      int indim,                     // in - number of dimensions
      int *lengths,                  // in - number of DE's in each dimension
      ESMC_CommType *commtypes) {    // in - communication type
//
// !DESCRIPTION:
//      ESMF routine which fills in the contents of an already
//      allocated DELayout object.  May need to do additional allocations
//      as needed.  Must call the corresponding {\tt ESMC\_DELayoutDestruct}
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use {\tt ESMC\_DELayoutCreate}, which calls
//      {\tt ESMC\_DELayoutConstruct}.  Define for deep classes only.
//
//EOP

  int ii, nx, ny, nz;
  int userwants;
  this->ndim=indim;

  // Initialize comm, PE, DE, Machine
  ESMC_DELayoutInit();

  // get total number of DEs
  comm.ESMC_CommGetNumDEs(&nDEs);
  //cout << "DELayout(parent) comm group size = " << nDEs << endl;

  // do some error checks
  userwants = 1;
  for(ii=0; ii<ndim; ii++)  
    userwants *= lengths[ii];
   
  if (userwants > nDEs) {
    cout << "request needs " << userwants << " DEs, only " 
         << nDEs << " available " << endl;
    return ESMF_FAILURE;
  }
  //cout << "user wants " << userwants << " which is ok." << endl;

  // TODO: This is NOT right - it should be a true child communicator.
  decomm.mpicomm = MPI_COMM_WORLD;

  this->length = new int[ESMF_MAXDECOMPDIM];
  this->commType = new ESMC_CommType[ESMF_MAXDECOMPDIM];
  for(ii=0; ii<ndim; ii++) {
    this->commType[ii]=commtypes[ii];
    this->length[ii]=lengths[ii];
    this->nDEs*=this->length[ii];
  }
  for(ii=ndim; ii<ESMF_MAXDECOMPDIM; ii++) {
    this->commType[ii]=ESMC_COMMTYPE_SHR;
    this->length[ii]=1;
   }

  // now you can use all the dims, regardless of what ndim is.
  nx=length[0];
  ny=length[1];
  nz=length[2];

  // construct 3D array of ESMC_DE's

  try {
    // construct 3D array of ESMC_DE's

    // first, create array of (nx) pointers to ESMC_DE pointers
    layout = new ESMC_DE**[nx];

    // then allocate an array of (ny) ESMC_DE pointers for each x pointer
    for (int i=0; i<nx; i++) {
      layout[i] = new ESMC_DE*[ny];
      // finally allocate a Z array of ESMC_DE's for each y pointer
      for (int j=0; j<ny; j++) {
        layout[i][j] = new ESMC_DE[nz];
      }
    }
  }
  catch(...) {
//  catch(bad_alloc) {  // TODO: use when IBM supports it (blackforest doesn't)
// TODO:  call ESMF log/err handler
    cerr << "ESMC_DELayoutConstruct() memory allocation failed\n";
    return(ESMF_FAILURE);
  }

  // create a PE list object to hold the parent PE sub-list
  int rc;
  peList = ESMC_PEListCreate(nDEs, &rc); // assume #PEs = nDEs for now

  //
  // Assign parent DELayout DE's to DE's in new layout according to
  // communication hint.  Assume parent layout is 1D; contained DE list
  // consists of unique DE ids that are pre-sorted
  // by fastest communication affinity (e.g. node, thread, process)
  //
  int ni, nj, nk;  // loop limits
  int i, j, k;     // i outer loop, j inner loop (fastest)
  int *x, *y, *z;  // layout coordinates to loop through

  // TODO: Wire this correctly for communication types
  commHint = ESMC_XFAST;

  switch(commHint)
  {
    case ESMC_XFAST:
    case ESMC_NOHINT:
      ni = length[2];  z = &i; // 3rd fastest (for outer loop)
      nj = length[1];  y = &j; // 2nd fastest (for middle loop)
      nk = length[0];  x = &k; // fastest (for inner loop)
      break;
    case ESMC_YFAST:
      ni = length[2]; z = &i;
      nj = length[0]; x = &j;
      nk = length[1]; y = &k;
      break;
    case ESMC_ZFAST:
      ni = length[1]; y = &i;
      nj = length[0]; x = &j;
      nk = length[2]; z = &k;
      break;
    default:
      break;
  }

//cout << "ESMC_DELayoutConstruct() ni, nj " << ni << ", " << nj << endl;

  ESMC_DE de;
  ESMC_PE *pe;
  int index=0;
  for (i=0; i<ni; i++) {
    for(j=0; j<nj; j++) {
      for(k=0; k<nk; k++) {

        //cout << "ESMC_DELayoutConstruct(): " << i << ", " << j  << "\n";
        //cout << "ESMC_DELayoutConstruct(): " << *x<< ", " << *y << "\n";

        // parent DE cannot be reused by subsequent DELayoutCreate calls
        rc = inparent->ESMC_DELayoutGetDE(de_indices[index], 0, 0, &de);
        //cout << "index = " << index << ", ESMC_DELayoutGetDE(" 
        //     << de_indices[index] << ") called, rc = " << rc << endl;

        
        if (rc == ESMF_SUCCESS) {
          // assign DE in given parent layout to this DE in layout
          // this makes a copy of the contents so we can now change it
          // to have the right id's for the new layout
          layout[*x][*y][*z] = de;

          // copy PE sub-list from parent layout; determine from DE list
          de.ESMC_DEGetPE(&pe);
          peList->ESMC_PEListSetPE(index, pe);

          layout[*x][*y][*z].ESMC_DESetESMFID(index);
          index++;
        }
        else {
          // TODO: log err
          cout << "ESMC_DELayoutConstruct(): ESMC_DELayoutGetDE()" <<
                  " returned ESMF_FAILURE" << endl;
          return(ESMF_FAILURE);
        }
      }
    }
  }
  //peList->ESMC_PEListPrint();

  if (verbose) {
    cout << "ESMC_DELayoutConstruct (from Parent) successful\n";
    //cout << "Create Layout from Parent:" << endl;
    //ESMC_DELayoutPrint();
  }
  return(ESMF_SUCCESS);

 } // end ESMC_DELayoutConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutConstruct - Build a DELayout from a DE list
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutConstruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *delist,                // in - array of DE indices
      int indim,                  // in - number of dimensions
      int *lengths,               // in - number of DEs in each dimension
      ESMC_CommType *commtypes) { // in - array of communication types
//
// !DESCRIPTION:
//      ESMF routine which fills in the contents of an already
//      allocated DELayout object.  May need to do additional allocations
//      as needed.  Must call the corresponding {\tt ESMC\_DELayoutDestruct}
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use {\tt ESMC\_DELayoutCreate}, which calls
//      {\tt ESMC\_DELayoutConstruct}.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  

  int nx, ny, nz;
  int i, j, k;
  int userwants;

  // Initialize comm, PE, DE, Machine
  ESMC_DELayoutInit();

  // get total number of DEs
  comm.ESMC_CommGetNumDEs(&nDEs);
  //cout << "DELayoutCreate(delist) comm group size = " << nDEs << endl;

  // do some error checks
  userwants = 1;
  for(i=0; i<indim; i++)  
    userwants *= lengths[i];
   
  if (userwants > nDEs) {
    cout << "request needs " << userwants << " DEs, only " 
         << nDEs << " available " << endl;
    return ESMF_FAILURE;
  }
  //cout << "user wants " << userwants << " which is ok." << endl;

  // TODO: This is NOT right - it should be a true child communicator.
  decomm.mpicomm = MPI_COMM_WORLD;

  // make space for the lists
  this->length = new int[ESMF_MAXDECOMPDIM];
  this->commType = new ESMC_CommType[ESMF_MAXDECOMPDIM];

  nDEs = 1;
  for (i=0; i<indim; i++) {
      this->length[i] = lengths[i];
      this->commType[i] = commtypes[i];
      nDEs *= lengths[i];
  }
  for (i=indim; i<ESMF_MAXDECOMPDIM; i++) {
      this->length[i] = 1;
      this->commType[i] = ESMC_COMMTYPE_SHR;
  }

  // now all the lengths and commtypes are initialized, regardless of
  // what ndim is.

  // create the corresponding PE list
  ESMC_DELayoutSetPEList();

  //
  // construct 3D array of ESMC_DE's
  //

  this->ndim=indim;
  nx=length[0];
  ny=length[1];
  nz=length[2];

  // TODO: this loop is now ok for 3D, but further down it's only 2D
  try {

    // first, create array of (nx) pointers to ESMC_DE pointers
    layout = new ESMC_DE**[nx];

    // then allocate an array of (ny) ESMC_DE pointers for each x pointer
    for (i=0; i<nx; i++) {
      layout[i] = new ESMC_DE*[ny];
      // finally allocate a Z array of ESMC_DE's for each y pointer
      // only 1 long, since this version of Construct is 2D
      for (int j=0; j<ny; j++) {
        layout[i][j] = new ESMC_DE[nz];
      }
    }
  }
  catch(...) {
    // TODO:  call ESMF log/err handler
    cerr << "ESMC_DELayoutConstruct() memory allocation failed\n";
    return(ESMF_FAILURE);
  }

  //deList = delist;
  commHint = ESMC_XFAST;

  // TODO: ?? make commHint lookup table & share with Print()

  //
  // Assign DElist to DE's in layout according to communication hint.
  // Assume given DE list consists of unique DE ids that are pre-sorted
  // by fastest communication affinity (e.g. node, thread, process)
  //
  // TODO: this part is 2D only
  //
  int ni, nj, nk;  // loop limits
                   // i outer loop, j inner loop (fastest)
  int *x, *y, *z;  // layout coordinates to loop through
  switch (commHint)
  {
    case ESMC_XFAST:
    case ESMC_NOHINT:
      ni = length[2];  z = &i; // 3rd fastest (for outer loop)
      nj = length[1];  y = &j; // 2nd fastest (for middle loop)
      nk = length[0];  x = &k; // fastest (for inner loop)
      break;
    case ESMC_YFAST:
      ni = length[2]; z = &i;
      nj = length[0]; x = &j;
      nk = length[1]; y = &k;
      break;
    case ESMC_ZFAST:
      ni = length[1]; y = &i;
      nj = length[0]; x = &j;
      nk = length[2]; z = &k;
      break;
    default:
      break;
  }

//cout << "ESMC_DELayoutConstruct() ni, nj " << ni << ", " << nj << endl;

  int DEix=0;
  int PEix=0;
  ESMC_PE *pe;
  for (i=0; i<ni; i++) {
    for(j=0; j<nj; j++) {
      for(k=0; k<nk; k++) {
        // assign DE in given list to this DE in layout
//cout << "ESMC_DELayoutConstruct(): " << i << ", " << j  << "\n";
//cout << "ESMC_DELayoutConstruct(): " << *x<< ", " << *y << "\n";
        layout[*x][*y][*z].ESMC_DESetESMFID(delist[DEix++]);

        // TODO: this is new code.  it should be well tested. 
        peList->ESMC_PEListGetPE(PEix++, &pe);
        layout[*x][*y][*z].ESMC_DESetPE(pe);

        // TODO: and what was this?  it was commented out.
        //layout[*x][*y][*z].ESMC_DESetPEID(delist[DEix++]);
      }
    }
  }

  if (verbose) {
    cout << "ESMC_DELayoutConstruct (from delist) successful\n";
    //cout << "Create Layout from delist:" << endl;
    //ESMC_DELayoutPrint();
  }
  return(ESMF_SUCCESS);

 } // end ESMC_DELayoutConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutConstruct - Build a DELayout from a PE List
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutConstruct(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_PEList *pelist,        // in - PEList
      int indim,                  // in - number of dimensions
      int *lengths,               // in - number of DEs in each dimension
      ESMC_CommType *commtypes) { // in - communication type for each dimension
//
// !DESCRIPTION:
//      ESMF routine which fills in the contents of an already
//      allocated DELayout object.  May need to do additional allocations
//      as needed.  Must call the corresponding {\tt ESMC\_DELayoutDestruct}
//      routine to free the additional memory.  Intended for internal
//      ESMF use only; end-users use {\tt ESMC\_DELayoutCreate}, which calls
//      {\tt ESMC\_DELayoutConstruct}.  Define for deep classes only.
//
//EOP
// !REQUIREMENTS:  

  int nx, ny, nz;
  int i;
  int userwants;

  // Initialize comm, PE, DE, Machine
  ESMC_DELayoutInit();

  // get total number of DEs
  comm.ESMC_CommGetNumDEs(&nDEs);
  //cout << "DELayoutCreate(pelist) comm group size = " << nDEs << endl;

  // do some error checks
  userwants = 1;
  for(i=0; i<indim; i++)  
    userwants *= lengths[i];
   
  if (userwants > nDEs) {
    cout << "request needs " << userwants << " DEs, only " 
         << nDEs << " available " << endl;
    return ESMF_FAILURE;
  }

  this->ndim = indim;
  
  this->length = new int[ESMF_MAXDECOMPDIM];
  this->commType = new ESMC_CommType[ESMF_MAXDECOMPDIM];

  nDEs = 1;
  for (i=0; i<ndim; i++) {
    this->length[i] = lengths[i];
    nDEs *= lengths[i];
  }
  for(i=ndim; i<ESMF_MAXDECOMPDIM; i++)
    this->length[i] = 1;

  peList = pelist;
  commHint = ESMC_XFAST;

  // TODO: This is NOT right - it should be a true child communicator.
  decomm.mpicomm = MPI_COMM_WORLD;

  // construct 3D array of ESMC_DE's

// TODO: ?? use exception handling when universally supported (pgCC doesn't)
#if 1
  try {
    // construct 3D array of ESMC_DE's

    // first, create array of (length[0]) pointers to ESMC_DE pointers
    layout = new ESMC_DE**[length[0]];

    // then allocate an array of (length[1]) ESMC_DE pointers for each x pointer
    for (i=0; i<length[0]; i++) {
      layout[i] = new ESMC_DE*[length[1]];
      // finally allocate an array of ESMC_DE's for each y pointer
      for (int j=0; j<length[1]; j++) {
        layout[i][j] = new ESMC_DE[length[2]];
      }
    }
  }
//  catch(bad_alloc) {  // TODO: use when IBM supports it (blackforest doesn't)
  catch(...) {
// TODO:  call ESMF log/err handler
    cerr << "ESMC_DELayoutConstruct() memory allocation failed\n";
    return(ESMF_FAILURE);
  }
#endif


  // TODO: ?? make commHint lookup table & share with Print()

  // assign PE's to DE's in layout according to communication hint
  int ni, nj, nk;
  int j, k;
  int *x, *y, *z;
  switch (commHint)
  {
    case ESMC_XFAST:
    case ESMC_NOHINT:
      ni = length[2];  z = &i; // 3rd fastest (for outer loop)
      nj = length[1];  y = &j; // 2nd fastest (for middle loop)
      nk = length[0];  x = &k; // fastest (for inner loop)
      break;
    case ESMC_YFAST:
      ni = length[2]; z = &i;
      nj = length[0]; x = &j;
      nk = length[1]; y = &k;
      break;
    case ESMC_ZFAST:
      ni = length[1]; y = &i;
      nj = length[0]; x = &j;
      nk = length[2]; z = &k;
      break;
    default:
      break;
  }

//cout << "ESMC_DELayoutConstruct() ni, nj, nk: "
          //<< ni << ", " << nj << ", " << nk << endl;

  int PEix=0;
  ESMC_PE *pe;
  for (i=0; i<ni; i++) {
    for(j=0; j<nj; j++) {
      for(k=0; k<nk; k++) {

        // retrieve next PE from our list
        peList->ESMC_PEListGetPE(PEix++, &pe);

        // then assign it to this DE
//cout << "ESMC_DELayoutConstruct(): " << i << ", " << j << ", " << k << "\n";
//cout << "ESMC_DELayoutConstruct(): " << *x<< ", " << *y<< ", " << *z<< "\n";
        layout[*x][*y][*z].ESMC_DESetPE(pe);
      }
    }
  }

  if (verbose) {
    cout << "ESMC_DELayoutConstruct (from pelist) successful\n";
    //cout << "Create Layout from pelist:" << endl;
    //ESMC_DELayoutPrint();
  }
  return(ESMF_SUCCESS);

 } // end ESMC_DELayoutConstruct

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutDestruct - release resources associated w/a DELayout
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutDestruct(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      ESMF routine which deallocates any space allocated by
//      {\tt ESMF\_DELayoutConstruct}, does any additional cleanup before the
//      original DELayout object is freed.  Intended for internal ESMF
//      use only; end-users use {\tt ESMC\_DELayoutDestroy}, which calls
//      {\tt ESMC\_DELayoutDestruct}.  Define for deep classes only.
//
//EOP

//
//  code goes here
//
//cout << "ESMC_DELayoutDestruct() invoked\n";

  // first delete each array of (ny) ESMC_DE's for each x pointer
  for (int i=0; i<length[0]; i++) {
    for (int j=0; j<length[1]; j++) {
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
  delete[] length;
  length = ESMC_NULL_POINTER;
  delete[] commType;
  commType = ESMC_NULL_POINTER;
  commHint = ESMC_NOHINT;

  return(ESMF_SUCCESS);

 } // end ESMC_DELayoutDestruct


//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  ESMC_DELayoutSetPEList - Create a PE list
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutSetPEList(void) {
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
//    none
//
// !DESCRIPTION:
//      Do an MPI communication to create a {\tt ESMC\_PE} list for all {\tt ESMC\_PE}s
//      involved in this layout.
//
//EOPI

  comm.ESMC_CommGetNumDEs(&nDEs);

  // Get our PE ids
  int mypeid=0, mycpuid=0, mynodeid=0;
  myPE.ESMC_PEGetEsmfID(&mypeid);   // (mypeid != myDEid) anymore...
  myPE.ESMC_PEGetCpuID(&mycpuid);
  myPE.ESMC_PEGetNodeID(&mynodeid);
  //cout << "mypeid, mycpuid, mynodeid = " << mypeid << "," << mycpuid << ", "
  //     << mynodeid << "\n";

  // prepare send buffer with our PE ids
  //  TODO: ?? use MPI derived type to send whole PE object rather
  //           than 3 ints. Then use resulting receive buffer as PE list array,
  //           thereby avoiding a copy operation.
  int sendbuf[3];
  sendbuf[0] = mypeid;
  sendbuf[1] = mycpuid;
  sendbuf[2] = mynodeid;

  // temporary global buffer used to perform all gather
  int *gbuf=0;
  try {
    gbuf = new int[nDEs * 3 * sizeof(int)];
  }
  catch(...) {
    // TODO:  call ESMF log/err handler
    cerr << "ESMC_DELayoutConstruct() gbuf memory allocation failed\n";
    return(ESMF_FAILURE);
  }

  // share/gather all PE IDs from all DEs
  comm.ESMC_CommAllGather(sendbuf, gbuf, 3, ESMC_INT);
  //cout << "DELayoutCreate(): exited ESMC_CommAllGather" << endl;

  // create PE List from gathered IDs
  // create a PE list object entirely on the heap
  int rc;
  peList = ESMC_PEListCreate(nDEs, &rc); // assume #PEs = nDEs
                                         //   for now
  for(int i=0; i<nDEs; i++) {
    // populate PE list with gathered ids
    peList->ESMC_PEListInit(i, gbuf[i*3], gbuf[i*3+1], gbuf[i*3+2]);
  }

  // done with gbuf, delete it
  delete gbuf;

  // sort PE list by fastest communication neighbors (node) to
  //   prep assignment to layout
  //   TODO: other sort criteria (different node types) ?
  //peList->ESMC_PEListPrint();
  peList->ESMC_PEListSort();
  //peList->ESMC_PEListPrint();

  return ESMF_SUCCESS;
}

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutInit - initializes a DELayout object
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutInit(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) {
//
// !DESCRIPTION:
//      ESMF routine which only initializes {\tt ESMC\_DELayout} values; it does not
//      allocate any resources.  Define for shallow classes only,
//      for deep classes define and use routines Create/Destroy and
//      Construct/Destruct.  Can be overloaded like {\tt ESMC\_DELayoutCreate}.
//
//EOP
// !REQUIREMENTS:  

  // initialize to an empty layout 
  layout = 0;
  peList = 0;
  commHint = ESMC_NOHINT;

  //
  // initialize my DE, PE, Comm, and Machine model
  //
  int argc = 0;    // TODO pass into DELayoutCreate ?
  char **argv = 0; // TODO pass into DELayoutCreate ?
  int myEsmfID;

  myDE.ESMC_DESetType(ESMC_PROCESS); // TODO: auto determine proc or thread,
                                     //       or get from config file ?
  comm.ESMC_CommInit(&argc, &argv, &myDE); // computes unique ESMF DE id

  // initialize machine to defaults TODO:  This is no longer set here.
  //  A global Machine object has already been instantiated.  This is backwards
  //  anyway - it should query the hardware with sysinfo() or similar calls
  //  and not set them with static (and untrue) values.
  //Mach.ESMC_MachineInit(256, 1024, 4, true, true, true, 1, 200, 2, 100);
  myPE.ESMC_PEInit(&Machine);        // gets cpu, node ids from machine

  Machine.ESMC_MachineGetCpuID(&myEsmfID);
  myPE.ESMC_PESetEsmfID(myEsmfID);  

//cout << "myDEid = " << myDEid << "\n";

#if 0
  // debug
  int mypeid=0, mycpuid=0, mynodeid=0;
  myPE.ESMC_PEGetEsmfID(&mypeid); //   (mypeid = myDEid)
  myPE.ESMC_PEGetCpuID(&mycpuid);
  myPE.ESMC_PEGetNodeID(&mynodeid);
cout << "mypeid, mycpuid, mynodeid = " << mypeid << "," << mycpuid << ", "
       << mynodeid << "\n";
#endif

  return(ESMF_SUCCESS);

 } // end ESMC_DELayoutInit

#if 0
//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutGetConfig - get configuration info from a DELayout
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutGetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_DELayoutConfig *config) const {  // out - resources
//
// !DESCRIPTION:
//    Returns the set of resources the {\tt DELayout} object was configured with.
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_DELayoutGetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutSetConfig - set configuration info for a DELayout
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutSetConfig(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      const ESMC_DELayoutConfig *config) {     // in - resources
//
// !DESCRIPTION:
//    Configures the {\tt ESMC\_DELayout} object with set of resources given.
//
//EOP
// !REQUIREMENTS:  

 } // end ESMC_DELayoutSetConfig

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutGet<Value> - get <Value> for a DELayout
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutGet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> *value) const {     // out - value
//
// !DESCRIPTION:
//     Returns the value of {\tt ESMC\_DELayout} member <Value>.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_DELayoutGet<Value>

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutSet<Value> - set <Value> for a DELayout
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutSet<Value>(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      <value type> value) {     // in - value
//
// !DESCRIPTION:
//     Sets the {\tt ESMC\_DELayout} member <Value> with the given value.
//     Can be multiple routines, one per value
//
//EOP
// !REQUIREMENTS:  

//
//  code goes here
//

 } // end ESMC_DELayoutSet<Value>
#endif

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutGetNumDEs - get the total number of DEs in layout
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutGetNumDEs(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *ndes) const {     // out - total number of DEs in layout
//
// !DESCRIPTION:
//    Returns the total number of {\tt ESMC\_DE}s in the layout
//EOP
// !REQUIREMENTS:  

  *ndes = nDEs;
  return(ESMF_SUCCESS);

 } // end ESMC_DELayoutGetNumDEs

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutGetSize - get (nx,ny) size of 2D DELayout
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutGetSize(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *nx,             // out - number of DE's in x direction
      int *ny) const {     // out - number of DE's in y direction
//
// !DESCRIPTION:
//    returns overall x,y dimensions of 2D {\tt ESMC\_DELayout}
//
//EOP
// !REQUIREMENTS:  

  *nx = length[0];
  *ny = length[1];

  return(ESMF_SUCCESS);

 } // end ESMC_DELayoutGetSize

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutGetSize - get (nx,ny,nz) size of DELayout
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutGetSize(
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
//    returns overall x,y,z dimensions of {\tt ESMC\_DELayout}
//
//EOP
// !REQUIREMENTS:  

  *nx = length[0];
  *ny = length[1];
  *nz = length[2];

  return(ESMF_SUCCESS);

 } // end ESMC_DELayoutGetSize

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutGetDEPosition - get x,y position of my DE in 2D DELayout
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutGetDEPosition(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *x,             // out - x position of DE in layout
      int *y) const {     // out - y position of DE in layout
//
// !DESCRIPTION:
//    returns (x,y) position of my {\tt ESMC\_DE} in 2D layout
//
//EOP
// !REQUIREMENTS:  

  // linear search for DE TODO: compute once on initialization ?
  for (int i=0; i<length[0]; i++) {
    for (int j=0; j<length[1]; j++) {
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

 } // end ESMC_DELayoutGetDEPosition

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutGetDEPosition - get (x,y,z) position of DE in DELayout
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutGetDEPosition(
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
//    returns (x,y,z) position of given {\tt ESMC\_DE} in layout
//
//EOP
// !REQUIREMENTS:  

  // linear search for DE
  for (int i=0; i<length[0]; i++) {
    for (int j=0; j<length[1]; j++) {
      for (int k=0; k<length[2]; k++) {
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

 } // end ESMC_DELayoutGetDEPosition

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutGetDE - get DE at position (x,y,z)
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutGetDE(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int x,               // in - x position of DE in layout
      int y,               // in - y position of DE in layout
      int z,               // in - z position of DE in layout
      ESMC_DE *de) const { // out - DE at (x,y,z)
//
// !DESCRIPTION:
//    returns {\tt ESMC\_DE} at position (x,y,z) in layout
//
//EOP
// !REQUIREMENTS:  

  if (x >= 0 && x < length[0] &&
      y >= 0 && y < length[1] &&
      z >= 0 && z < length[2] &&
      de != 0) {
    *de = layout[x][y][z];
    return(ESMF_SUCCESS);
  }
  else {
    // TODO: log error
    return(ESMF_FAILURE);
  }

 } // end ESMC_DELayoutGetDE

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutGetDE - get DE with given ID
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutGetDE(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int deid,              // in  - given DE id
      ESMC_DE **de) const {   // out - matching DE
//
// !DESCRIPTION:
//    Returns {\tt ESMC\_DE} for given DE id
//EOP
// !REQUIREMENTS:  

  int testid;

  // linear search for DE id
  for (int i=0; i<length[0]; i++) {
    for (int j=0; j<length[1]; j++) {
      for (int k=0; k<length[2]; k++) {
        layout[i][j][k].ESMC_DEGetESMFID(&testid);
        if (testid == deid) {
          // found -- return this DE
          *de = &layout[i][j][k];
          return(ESMF_SUCCESS);
        }
      }
    }
  }

  //cout << "id not found" << endl;
  return(ESMF_FAILURE);

 } // end ESMC_DELayoutGetDE

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutGetDEIDat - get DE ID at position (x,y,z)
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutGetDEIDat(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int x,               // in - x position of DE in layout
      int y,               // in - y position of DE in layout
      int z,               // in - z position of DE in layout
      int *deid) const   { // out - DE ID at (x,y,z)
//
// !DESCRIPTION:
//    returns {\tt ESMC\_DE} at position (x,y,z) in layout
//
//EOP
// !REQUIREMENTS:  

  if ((x >= 0 && x < length[0]) &&
      (y >= 0 && y < length[1]) &&
      (z >= 0 && z < length[2]) &&
      (deid != NULL)) {
       layout[x][y][z].ESMC_DEGetESMFID(deid);
    return(ESMF_SUCCESS);
  }
  else {
    // TODO: log error
    if(deid != NULL)  
       *deid = -1; 
    return(ESMF_FAILURE);
  }

 } // end ESMC_DELayoutGetDEIDat

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutGetDEIDat - get DE ID at position (x,y)
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutGetDEIDat(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int x,               // in - x position of DE in layout
      int y,               // in - y position of DE in layout
      int *deid) const   { // out - DE ID at (x,y)
//
// !DESCRIPTION:
//    returns {\tt ESMC\_DE} at position (x,y) in layout
//
//EOP
// !REQUIREMENTS:  

  if ((x >= 0 && x < length[0]) &&
      (y >= 0 && y < length[1]) &&
      (deid != NULL)) {
       layout[x][y][0].ESMC_DEGetESMFID(deid);
    return(ESMF_SUCCESS);
  }
  else {
    // TODO: log error
    if(deid != NULL)  
       *deid = -1; 
    return(ESMF_FAILURE);
  }

 } // end ESMC_DELayoutGetDEIDat

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutGetDEID - get ID of our DE
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutGetDEID(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *deid) const {     // out - our DE id
//
// !DESCRIPTION:
//    Returns our {\tt ESMC\_DE} id
//EOP
// !REQUIREMENTS:  

  int peid, testid;

  myPE.ESMC_PEGetEsmfID(&peid);
  //cout << "ESMC_DELayoutGetDEID(): myPEid = " << peid << endl;

  // linear search for DE TODO: compute once on initialization ?
  for (int i=0; i<length[0]; i++) {
    for (int j=0; j<length[1]; j++) {
      for (int k=0; k<length[2]; k++) {
        layout[i][j][k].PE->ESMC_PEGetEsmfID(&testid); // PE number, unique
        if (peid == testid) {
          // found -- return this id
          layout[i][j][k].ESMC_DEGetESMFID(deid);
          //cout << "ESMC_DELayoutGetDEID(): i,j,k, deid = " << i << j << k << *deid << endl;
          return(ESMF_SUCCESS);
        }
      }
    }
  }

  // old code.  DE id is no longer same as pe id
  // TODO: make myDE point directly to proper layout item.
  //myDE.ESMC_DEGetESMFID(deid);

  //cout << "id not found" << endl;
  return(ESMF_FAILURE);

 } // end ESMC_DELayoutGetDEID

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutGetParentDEID - get corresponding Parent DE ID
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutGetParentDEID(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int childdeid,           // in - DE ID in child layout
      ESMC_DELayout *inparent, // in - Parent which child was created from
      int *parentdeid) const { // out - DE ID in parent layout
//
// !DESCRIPTION:
//    Method applied to a Child layout.
//    Input is a {\tt ESMC\_DE} ID from the Child layout, plus the Parent layout from
//    which this child was created.  Returns the corresponding {\tt ESMC\_DE} ID number
//    in the Parent's layout for the given Child ID.
//EOP
// !REQUIREMENTS:  

  return this->ESMC_DELayoutGetSameDEID(childdeid, inparent, parentdeid);

 } // end ESMC_DELayoutGetParentDEID

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutGetChildDEID - get corresponding Child DE ID
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutGetChildDEID(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int parentdeid,        // in - DE ID in parent layout
      ESMC_DELayout *child,  // in - Child layout created from this Parent
      int *childdeid) const {  // out - DE ID in child layout 
//
// !DESCRIPTION:
//    Method applied to a Parent layout.
//    Input is a {\tt ESMC\_DE} ID from the Parent layout, plus a Child layout created
//    from this parent.  Returns the corresponding {\tt ESMC\_DE} ID number
//    in the Child's layout for the given Parent ID.
//EOP
// !REQUIREMENTS:  

  int esmfid;

  return this->ESMC_DELayoutGetSameDEID(parentdeid, child, childdeid);

 } // end ESMC_DELayoutGetChildDEID

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutGetDEExists - does a DE exist in other layout
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutGetDEExists(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int deid,               // in - DE ID in "this" layout
      ESMC_DELayout *other,   // in - Other layout - must be parent/child
      ESMC_Logical *exists) const {   // out - true/false
//
// !DESCRIPTION:
//    Method applied to a layout which has a child or parent relationship
//    with the "other" layout.   {\tt ESMC\_DE} ID is relative to the "this" layout,
//    boolean return is whether the same {\tt ESMC\_DE} ID is valid in the other layout.
//EOP
// !REQUIREMENTS:  
  
  int rc, dummy;

  rc = this->ESMC_DELayoutGetSameDEID(deid, other, &dummy);
  if (rc == ESMF_FAILURE) 
      *exists = ESMF_TF_FALSE;
  else
      *exists = ESMF_TF_TRUE;

  return ESMF_SUCCESS;

 } // end ESMC_DELayoutGetDEExists

//-----------------------------------------------------------------------------
//BOPI
// !IROUTINE:  ESMC_DELayoutGetSameDEID - return same DE ID from another layout
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutGetSameDEID(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int srcid,               // in - DE ID in this layout 
      ESMC_DELayout *other,    // in - parent or child layout of 'this'
      int *otherid) const {    // out - DE ID in other layout
//
// !DESCRIPTION:
//    Common code for either GetParent or GetChild {\tt ESMC\_DE} ID.  This is a
//    private method - go through the other interfaces for external access.
//EOP
// !REQUIREMENTS:  

  int esmfid, newid;
  int i, j, k;

  // TODO: this may not always work.  the DE ESMF id's in the layout are not
  // yet always set, and sometimes the PE id's aren't set.  it needs lots of
  // testing...
  for(i=0; i<length[0]; i++) {
    for (j=0; j<length[1]; j++) {
      for (k=0; k<length[2]; k++) {
        // TODO:  right now, the DE information in the layout is empty
        //  except for the PE pointer.  All DE information is kept in myDE
        //  locally each proc.  the init code needs another allgather to
        //  spread the de info to each layout array.   right now the layout
        //  array has the right shape but is basically empty except for
        //  the PE info.

        layout[i][j][k].ESMC_DEGetESMFID(&newid);      // DE number, 0 based
        if (newid == srcid) {
            if (layout[i][j][k].PE == ESMC_NULL_POINTER) {
                *otherid = -1;
                return ESMF_FAILURE;
            }
            layout[i][j][k].PE->ESMC_PEGetEsmfID(&esmfid); // PE number, unique
            // have to break out of 3 loops; this is simplest way.
            // also bypasses the 'not found' code.
            goto found;
        }
            
        //  
      }
    }
  }

 //printf("DE ID %d not found in layout\n", srcid);
 return ESMF_FAILURE;

 found:
  // Now find corresponding DE in second layout which matches this
  // same esmfid.
  for(i=0; i<other->length[0]; i++) {
    for (j=0; j<other->length[1]; j++) {
      for (k=0; k<other->length[2]; k++) {
        other->layout[i][j][k].PE->ESMC_PEGetEsmfID(&newid);  // PE number
        if (esmfid == newid) {
           other->layout[i][j][k].ESMC_DEGetESMFID(otherid);  // DE number
           goto done;
        }
      }
    }
  }
 //printf("PE ID %d not found in other layout\n", esmfid);
 return ESMF_FAILURE;

 done:
  return(ESMF_SUCCESS);

 } // end ESMC_DELayoutGetSameDEID

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutGetXXX - is current PE part of given layout
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutGetXXX(void) const { 

//
// !RETURN VALUE:
//    int error return code 
//

// !DESCRIPTION:
//   If current PE is part of this given layout, returns {\tt ESMF\_SUCCESS}.
//   If not, returns {\tt ESMF\_FAILURE} for no without printing an error message.

//EOP
// !REQUIREMENTS:  

  int peid, testid;

  myPE.ESMC_PEGetEsmfID(&peid);

  // linear search for DE TODO: compute once on initialization ?
  for (int i=0; i<length[0]; i++) {
    for (int j=0; j<length[1]; j++) {
      for (int k=0; k<length[2]; k++) {
        layout[i][j][k].PE->ESMC_PEGetEsmfID(&testid); // PE number, unique
        if (peid == testid) {
          //cout << "pe found" << endl;
          return(ESMF_SUCCESS);
        }
      }
    }
  }

  //cout << "pe not found" << endl;
  return(ESMF_FAILURE);

 } // end ESMC_DELayoutGetXXX

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutSetAxisIndex - set an axis index from a layout
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutSetAxisIndex(
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
  this->ESMC_DELayoutGetDEPosition( &x, &y);
  // loop to set AxisIndex array
  for (int i=0; i<size_gcount; i++, AIPtr++) {
    // AIPtr->decomp = decompids[i];   jw
    // check if decomp is out of bounds
    if ((decompids[i] < 0) || (decompids[i] > 2)) {
      return(ESMF_FAILURE);
    }
    // if decomp is 0, no decomposition of the axis
    if (decompids[i] == 0) {
      AIPtr->min = 0;
      AIPtr->max = global_counts[i]-1;
      AIPtr->stride = global_counts[i]; // jw?
    }
    // if decomp is 1, use nxDELayout
    if (decompids[i] == 1) {
      int n1 = (global_counts[i]+length[0]-1)/length[0]; // round to nearest
      AIPtr->min = 0;
      AIPtr->max = n1-1;
      AIPtr->stride = global_counts[i];    // jw?
 //     AIPtr->gstart = x*n1;
    }
    // if decomp is 2, use nyDELayout
    if (decompids[i] == 2) {
      int n2 = (global_counts[i]+length[1]-1)/length[1]; // round to nearest
      AIPtr->min = 0;
      AIPtr->max = n2-1;
      AIPtr->stride = global_counts[i];   // jw?
  //    AIPtr->gstart = y*n2;
    }
  }
   
  return(ESMF_SUCCESS);

 } // end ESMC_DELayoutSetAxisIndex

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutGatherArrayI - gather a distributed integer array
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutGatherArrayI(
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
  this->ESMC_DELayoutGetSize(&nx, &ny);
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
          rmax[i] = AIPtr[i].stride;  // jw?  needs to be the global length
          rsize[i] = AIPtr[i].max - AIPtr[i].min + 1;
          rsize_tot[i] = AIPtr2[i].max - AIPtr2[i].min + 1;
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
          j_tot = j + AIPtr[ranky].min;
          sendbuf = &DistArray[j_tot*rsize_tot[rankx] + AIPtr[rankx].min];
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
          rmax[i] = AIPtr[i].stride;   // jw? same
          rsize[i] = AIPtr[i].max - AIPtr[i].min + 1;
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

 } // end ESMC_DELayoutGatherArray

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutGatherArrayF - gather a distributed float array
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutGatherArrayF(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      float *DistArray,          // in  - distributed array
      int decompids[],           // in  - decomposition identifier for each
                                 //       axis for the array
      int size_decomp,           // in  - size of decomp arrays
      ESMC_AxisIndex *AIPtr,     // in  - pointer to array of AxisIndex
                                 //       structures for exclusive data
      ESMC_AxisIndex *AIPtr2,    // in  - pointer to array of AxisIndex
                                 //       structures for total data
      float *GlobalArray) {      // out - global array
//
// !DESCRIPTION:
//    returns an array of AxisIndex types representing the decomposition of
//    an arbitrary number of axis by a layout
//
//EOP
// !REQUIREMENTS:
// TODO:  this might be a good place to use templates

  int rc = ESMF_FAILURE;
  int i, j, k, l, m;     // general counter vars
  MPI_Datatype mpidatatype;

  // get layout size
  int nx, ny;
  this->ESMC_DELayoutGetSize(&nx, &ny);
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
          rmax[i] = AIPtr[i].stride;  // jw?
          rsize[i] = AIPtr[i].max - AIPtr[i].min + 1;
          rsize_tot[i] = AIPtr2[i].max - AIPtr2[i].min + 1;
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
        float *sendbuf, *recvbuf;
        int sendcount;
        int* recvcounts = new int[nde];
        int* displs = new int[nde];
        for (int j=0; j<rsize[rbreak[0]]; j++) {
          j_tot = j + AIPtr[ranky].min;
          sendbuf = &DistArray[j_tot*rsize_tot[rankx] + AIPtr[rankx].min];
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
                                   displs, ESMC_FLOAT);
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
          rmax[i] = AIPtr[i].stride;   // jw?
          rsize[i] = AIPtr[i].max - AIPtr[i].min + 1;
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
        float *sendbuf, *recvbuf;
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
                                   displs, ESMC_FLOAT);
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

 } // end ESMC_DELayoutGatherArrayF

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutSendRecv - send / receive operation within a delayout
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutSendRecv(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void *sbuf,               // in  - send array
      void *rbuf,               // in  - receive array
      int snum,                 // in  - send array length
      int rnum,                 // in  - receive array length
      int sde_index,            // in  - send de index
      int rde_index,            // in  - receive de index
      ESMC_Datatype type) {     // in  - datatype of buffers

//
// !DESCRIPTION:
//    Performs a send and receive data transfer between {\tt ESMC\_DE}s
//    on a single {\tt ESMC\_DELayout}.
//
//EOP

  int rc;
  MPI_Datatype mpidatatype;
  MPI_Status status;

  mpidatatype = comm.ESMC_DatatypeToMPI[type];

  // If we're both sending and receiving from our own process,
  // then do a straight memory copy and don't call message passing.
  
  // TODO: this is not the right test - we need to test our current
  // address space against the address spaces of both the send and 
  // receive index numbers, not the indicies against each other.
  // so this is commented out for now, but it can be a bit performance
  // win if the source and destination are local, so it should be
  // revisited again.
  //if (rde_index == sde_index) {
  //   if (snum != rnum) { 
  //      printf("sending bytes != receiving bytes in DELayoutSendRecv\n");
  //      return ESMF_FAILURE;
  //   }
  //   memcpy(rbuf, sbuf, snum*sizeof(float));
  //   rc = ESMF_SUCCESS;
    
  //} else {
      if (MPI_Sendrecv(sbuf, snum, mpidatatype, rde_index, ESMF_MPI_TAG, 
		       rbuf, rnum, mpidatatype, sde_index, MPI_ANY_TAG, 
		       decomm.mpicomm, &status) == MPI_SUCCESS) {
        rc = ESMF_SUCCESS;
      }
      else {
        printf("Failure in MPI_Sendrecv \n");
        return ESMF_FAILURE;
      }
  //}

  return(rc);

 } // end ESMC_DELayoutSendRecv

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutBcast - broadcast from a root DE to an entire layout
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutBcast(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:  
      void *buf,                // in  - buffer     
      int num,                  // in  - buffer length
      int rootde_index,         // in  - index of root de
      ESMC_Datatype type) {     // in  - data type of buffer

//
// !DESCRIPTION:
//    Broadcasts data from a root {\tt ESMC\_DE} to all other {\tt ESMC\_DE}s in
//    the {\tt ESMC\_DELayout}.  
//
//EOP

  int rc;
  MPI_Datatype mpidatatype;
  MPI_Status status;

  mpidatatype = comm.ESMC_DatatypeToMPI[type];

  if (MPI_Bcast(buf, num, mpidatatype, rootde_index, 
                decomm.mpicomm) == MPI_SUCCESS) {
    rc = ESMF_SUCCESS;
  }
  else {
    printf("Failure in MPI_Bcast \n");
    return ESMF_FAILURE;
  }
  return(rc);

 } // end ESMC_DELayoutBcast

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutValidate - internal consistency check for a DELayout
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutValidate(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) const {    // in - validate options
//
// !DESCRIPTION:
//      Validates that a {\tt ESMC\_DELayout} is internally consistent.
//      Returns error code if problems are found.  {\tt ESMC\_Base} class method.
//
//EOP

  return(ESMF_SUCCESS);

 } // end ESMC_DELayoutValidate


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutPrint - print contents of a DELayout
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutPrint(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void) const {     //  in - print options
//
// !DESCRIPTION:
//      Print information about a {\tt ESMC\_DELayout}.  The options control the
//      type of information and level of detail.  {\tt ESMC\_Base} class method.
//
//EOP

  cout << "DELayout ndim=" << ndim << " nx,ny,nz=" << length[0] << "," << length[1] << "," << length[2] << endl;
  //cout << "commHint = " << commHint << "\n";

  int i,j,k;
  int ni,nj,nk;
  int *x, *y, *z;

  // TODO:  ?? create commHint lookup table & share with Construct()

  switch (commHint)
  {
    case ESMC_XFAST:
    case (ESMC_NOHINT):
      ni = length[2]; z = &i; // outer loop
      nj = length[1]; y = &j; // middle loop
      nk = length[0]; x = &k; // inner loop
      cout << "No hint or X fast, i=z, j=y, k=x;";
      break;
    case (ESMC_YFAST):
      ni = length[2]; z = &i;
      nj = length[0]; x = &j;
      nk = length[1]; y = &k;
      cout << "Y fast, i=z, j=x, k=y;";
      break;
    case (ESMC_ZFAST):
      ni = length[1]; y = &i;
      nj = length[0]; x = &j;
      nk = length[2]; z = &k;
      cout << "Z fast i=y, j=x, k=z;";
      break;
  }

  cout <<  "  ni,nj,nk = " <<  ni << "," << nj << "," << nk << endl;
  for(i=0; i<ni; i++) {
    for (j=0; j<nj; j++) {
      for (k=0; k<nk; k++) {
        //cout << "layout[" << i << "][" << j << "][" << k << "] = ";
        cout << "layout[" << *x<< "][" << *y<< "][" << *z<< "]: ";
        layout[*x][*y][*z].ESMC_DEPrint();
      }
    }
  }
  cout << "myDE: ";
  myDE.ESMC_DEPrint();
  cout << "myPE: ";
  myPE.ESMC_PEPrint();

  cout << "end of DELayout Print" << endl;
  return(ESMF_SUCCESS);

 } // end ESMC_DELayoutPrint

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayout - native C++ constructor
//
// !INTERFACE:
      ESMC_DELayout::ESMC_DELayout(
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

//cout << "ESMC_DELayout constructor invoked\n";

  // Initialize comm, PE, DE, Machine
  ESMC_DELayoutInit();

 } // end ESMC_DELayout

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ~ESMC_DELayout - native C++ destructor
//
// !INTERFACE:
      ESMC_DELayout::~ESMC_DELayout(void) {
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

//cout << "~ESMC_DELayout() invoked\n";
  ESMC_DELayoutDestruct();

 } // end ~ESMC_DELayout

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutScatter - Perform MPI-like scatter of data array from one DE to all others in layout
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutScatter(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      void *sndArray,      // in  - send data array
      void *rcvArray,      // out - received data array
      int len,             // in  - data array chunk size
      ESMC_Datatype type,  // in - array data type
      int rootDEid) {      // in  - root DE performing the scatter
//
// !DESCRIPTION:
//    Perform MPI-like scatter of a data array from one DE to all others
//    in the layout.
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

  // TODO: make comm public and invoke directly rather than at DELayout level ?
  //       (does not depend on any DELayout knowledge)

  ESMC_DE *rootDE;
  int rc;

  // perform scatter operation across all DEs in the layout
  rc = ESMC_DELayoutGetDE(rootDEid, &rootDE);
  rc = comm.ESMC_CommScatter(sndArray, rcvArray, len, type, rootDE);
  if (rc != ESMF_SUCCESS) {
    cout << "ESMC_DELayoutScatter() error" << endl;
  }

  return(rc);

 } // end ESMC_DELayoutScatter

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutAllGatherVI - Perform MPI-like Allgatherv of equally-sized integer data arrays across a layout
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutAllGatherVI(
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
//    across all {\tt ESMC\_DE}s in a layout
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

  // TODO: make comm public and invoke directly rather than at DELayout level ?
  //       (does not depend on any DELayout knowledge)

  // perform Allgatherv operation across all DEs in the layout
  int rc;
  rc = comm.ESMC_CommAllGatherV(sndArray, sndLen, rcvArray, rcvLen, rcvDispls,
                                ESMC_INT);
  if (rc != ESMF_SUCCESS) {
    cout << "ESMC_DELayoutAllGatherVI() error" << endl;
  }

  return(rc);

 } // end ESMC_DELayoutAllGatherVI

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutAllGatherVF - Perform MPI-like Allgatherv of equally-sized integer data arrays across a layout
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutAllGatherVF(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      float *sndArray,         // in  - integer send data array
      int  sndLen,             // in  - length of send data array
      float *rcvArray,         // out - gathered data array
      int *rcvLen,             // in  - array of receive data array lengths
      int *rcvDispls) {        // in  - array of rcvArray displacements
//
// !DESCRIPTION:
//    Perform MPI-like Allgatherv of integer data arrays
//    across all {\tt ESMC\_DE}s in a layout
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

  // TODO: make comm public and invoke directly rather than at DELayout level ?
  //       (does not depend on any DELayout knowledge)

  // perform Allgatherv operation across all DEs in the layout
  int rc;
  rc = comm.ESMC_CommAllGatherV(sndArray, sndLen, rcvArray, rcvLen, rcvDispls,
                                ESMC_FLOAT);
  if (rc != ESMF_SUCCESS) {
    cout << "ESMC_DELayoutAllGatherVF() error" << endl;
  }

  return(rc);

 } // end ESMC_DELayoutAllGatherVF

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_DELayoutAllReduce - reduce 1D integer data array across layout
//
// !INTERFACE:
      int ESMC_DELayout::ESMC_DELayoutAllReduce(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      int *dataArray,          // in  - 1D integer data array
      int *result,             // out - single integer value
      int arrayLen,            // in  - length of dataArray
      ESMC_Op op)  {         // in  - reduction operation (sum, min, max ...)
//
// !DESCRIPTION:
//      performs requested reduction operation on given data array across
//      all {\tt ESMC\_DE}s in layout
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

  // TODO: make comm public and invoke directly rather than at DELayout level ?
  //       (does not depend on any DELayout knowledge)

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

  // cout << "ESMC_DELayoutAllReduce localResult = " << localResult << endl;

  // perform reduction operation across all DEs in the layout
  int rc;
  rc = comm.ESMC_CommAllReduce(&localResult, result, 1, ESMC_INT, op);
  if (rc != ESMF_SUCCESS) {
    cout << "ESMC_DELayoutAllReduce(1D) error" << endl;
  }

  return(rc);

 } // end ESMC_DELayoutAllReduce
