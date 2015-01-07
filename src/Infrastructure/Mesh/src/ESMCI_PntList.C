// $Id: ESMCI_PntList.C,v 1.7 2012/01/06 20:17:51 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2012, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#define ESMC_FILENAME "ESMCI_PntList.C"
//==============================================================================
//
// ESMCI PntList method implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// Holds a list of points
//
//-----------------------------------------------------------------------------

// include associated header file
// For ESMF
#include <Mesh/include/ESMCI_PntList.h>

// For testing
//#include "ESMCI_PntList.h"

#include <stdlib.h>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id: ESMCI_PntList.C,v 1.7 2012/01/06 20:17:51 svasquez Exp $";
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------


// Set up ESMCI name space for these methods
namespace ESMCI{  


//-----------------------------------------------------------------------------
//
// Public Interfaces
//
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::PntList()"
//BOPI
// !IROUTINE:  PntList
//
// !INTERFACE:
PntList::PntList(
//
// !RETURN VALUE:
//    Pointer to a new PntList
//
// !ARGUMENTS:
                 int _coord_dim,
                 int _max_num_pnts
  ){
//
// !DESCRIPTION:
//   Construct PntList
//EOPI
//-----------------------------------------------------------------------------

  // Set values
  coord_dim=_coord_dim;
  max_num_pnts=_max_num_pnts;
  curr_num_pnts=0;

  // allocate memory
  coords=NULL;
  ids=NULL;
  if (max_num_pnts>0) {
    coords=new double [coord_dim*max_num_pnts];
    ids=new int [max_num_pnts];
  }

}
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::~PntList()"
//BOPI
// !IROUTINE:  ~PntList
//
// !INTERFACE:
 PntList::~PntList(void){
//
// !RETURN VALUE:
//    none
//
// !ARGUMENTS:
// none
//
// !DESCRIPTION:
//  Destructor for PntList, deallocates all internal memory, etc. 
//
//EOPI
//-----------------------------------------------------------------------------

   // Reset default values
   max_num_pnts=0;
   curr_num_pnts=0;
   
   // Deallocate memory
   if (coords!=NULL) delete [] coords;
   coords=NULL;   

   if (ids!=NULL) delete [] ids;
   ids=NULL;   
}


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::PntList::add()"
//BOP
// !IROUTINE:  add
//
// !INTERFACE:
void PntList::add(

//
// !RETURN VALUE:
//  none
// 
// !ARGUMENTS:
//
                  int _id,
                  double *_coord
  ) {
//
// !DESCRIPTION:
// Add a point to the PntList.
//
//EOP
//-----------------------------------------------------------------------------

  // Error check
  if (curr_num_pnts > max_num_pnts-1) {
    // Throw() << "PntList full";
  }

  // IF START EXTENDING POINT LIST WHEN OVER SIZE, THEN SWITCH TO VECTORS

  // Add point id
  ids[curr_num_pnts]=_id;

  // Add point coords
  double *pnt_coord_base=coords+coord_dim*curr_num_pnts;
  for (int i=0; i<coord_dim; i++) {
    pnt_coord_base[i]=_coord[i];
  }

  // Advance to next position
  curr_num_pnts++;
}
//-----------------------------------------------------------------------------



} // END ESMCI name space
//-----------------------------------------------------------------------------










