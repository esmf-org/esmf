// $Id: ESMC_Layout.C,v 1.3 2002/12/10 03:48:51 eschwab Exp $
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
#include <iostream>
#include <ESMC.h>

 // associated class definition file
 #include <ESMC_Layout.h>

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = "$Id: ESMC_Layout.C,v 1.3 2002/12/10 03:48:51 eschwab Exp $";
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
// !IROUTINE:  ESMC_LayoutCreate - Create a new Layout
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

  try {
    layout = new ESMC_Layout;
//std::cout << "ESMC_LayoutCreate() succesful\n";
    *rc = layout->ESMC_LayoutConstruct(nx, ny, nz, pelist, commhint);
    return(layout);
  }
  catch (std::bad_alloc) {
// TODO:  call ESMF log/err handler
    std::cerr << "ESMC_LayoutCreate() memory allocation failed\n";
    *rc = ESMF_FAILURE;
    return(0);
  }

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

  if (layout != 0) {
    layout->ESMC_LayoutDestruct();
    delete layout;
//std::cout << "ESMC_LayoutDestroy() successful\n";
    return(ESMF_SUCCESS);
  } else {
    return(ESMF_FAILURE);
  }

 } // end ESMC_LayoutDestroy

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_LayoutConstruct - fill in an already allocated Layout
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
  catch(std::bad_alloc) {
// TODO:  call ESMF log/err handler
    std::cerr << "ESMC_LayoutConstruct() memory allocation failed\n";
    return(ESMF_FAILURE);
  }

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
      ni = nzLayout;  z = &i; // 3rd fastest
      nj = nyLayout;  y = &j; // 2nd fastest
      nk = nxLayout;  x = &k; // fastest
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

//std::cout << "ESMC_LayoutConstruct() ni, nj, nk: "
          //<< ni << ", " << nj << ", " << nk << std::endl;

  int PEix=0;
  ESMC_PE *pe;
  for (i=0; i<ni; i++) {
    for(j=0; j<nj; j++) {
      for(k=0; k<nk; k++) {

        // retrieve next PE from our list
        peList->ESMC_PEListGetPE(PEix++, &pe);

        // then assign it to this DE
//std::cout << "ESMC_LayoutConstruct(): " << i << ", " << j << ", " << k << "\n";
//std::cout << "ESMC_LayoutConstruct(): " << *x<< ", " << *y<< ", " << *z<< "\n";
        layout[*x][*y][*z].ESMC_DESetPE(pe);
      }
    }
  }

//std::cout << "ESMC_LayoutConstruct() successful\n";
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
//std::cout << "ESMC_LayoutDestruct() invoked\n";

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

#if 0
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
      int arg1,            // in
      int arg2,            // in
      const char *arg3) {  // in
//
// !DESCRIPTION:
//      ESMF routine which only initializes Layout values; it does not
//      allocate any resources.  Define for shallow classes only,
//      for deep classes define and use routines Create/Destroy and
//      Construct/Destruct.  Can be overloaded like ESMC\_LayoutCreate.
//
//EOP
// !REQUIREMENTS:  developer's guide for classes

//
//  code goes here
//

 } // end ESMC_LayoutInit

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

  //std::cout << "nxLayout, nyLayout, nzLayout = " << nxLayout << "," << nyLayout << "," << nzLayout << std::endl;
  //std::cout << "commHint = " << commHint << "\n";

  int i,j,k;
  int ni,nj,nk;
  int *x, *y, *z;

  // TODO:  ?? create commHint lookup table & share with Construct()

  switch (commHint)
  {
    case ESMC_XFAST:
    case (ESMC_NOHINT):
      ni = nzLayout; z = &i;
      nj = nyLayout; y = &j;
      nk = nxLayout; x = &k;
      //std::cout << "i=z, j=y, k=x \n";
      break;
    case (ESMC_YFAST):
      ni = nzLayout; z = &i;
      nj = nxLayout; x = &j;
      nk = nyLayout; y = &k;
      //std::cout << "i=z, j=x, k=y \n";
      break;
    case (ESMC_ZFAST):
      ni = nyLayout; y = &i;
      nj = nxLayout; x = &j;
      nk = nzLayout; z = &k;
      //std::cout << "i=y, j=x, k=z \n";
      break;
  }

  for(i=0; i<ni; i++) {
    for (j=0; j<nj; j++) {
      for (k=0; k<nk; k++) {
        //std::cout << "layout[" << i << "][" << j << "][" << k << "] = ";
        std::cout << "layout[" << *x<< "][" << *y<< "][" << *z<< "] = ";
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

//std::cout << "ESMC_Layout() invoked\n";

  // initialize to an empty layout
  layout = 0;
  peList = 0;
  nxLayout = 0;
  nyLayout = 0;
  nzLayout = 0;
  commHint = ESMC_NOHINT;

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

//std::cout << "~ESMC_Layout() invoked\n";
  ESMC_LayoutDestruct();

 } // end ~ESMC_Layout
