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
//==============================================================================
#define ESMC_FILENAME "ESMCI_PointList.C"
//==============================================================================
//
// PointList class implementation (body) file
//
//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// Holds a list of points
//
// The code in this file implements the C++ PointList methods declared
// in the companion file ESMCI_PointList.h
//
//-----------------------------------------------------------------------------

// include associated header file
#include <PointList/include/ESMCI_PointList.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <algorithm>
#include <fstream>

// include ESMF headers
#include "ESMCI_Macros.h"
//#include "ESMCI_VM.h"

// LogErr headers
#include "ESMCI_LogErr.h"                  // for LogErr

using namespace std;

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version =
  "$Id$";
//-----------------------------------------------------------------------------


namespace ESMCI {

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::PointList()"
  //BOPI
  // !IROUTINE:  PointList
  //
  // !INTERFACE:
  PointList::PointList(
  //
  // !RETURN VALUE:
  //    Pointer to a new PointList
  //
  // !ARGUMENTS:
                   int _max_num_pts,
                   int _coord_dim
                   ){
    //
    // !DESCRIPTION:
    //   Construct PointList
    //EOPI
    //-----------------------------------------------------------------------------
    // Set values
    coord_dim=_coord_dim;
    max_num_pts=_max_num_pts;
    curr_num_pts=0;

    // allocate memory
    points = NULL;

    if (max_num_pts>=0) {
      points=new point [max_num_pts];
    }

    // return successfully
  }




#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::~PointList()"
  //BOPI
  // !IROUTINE:  ~PointList
  //
  // !INTERFACE:
  PointList::~PointList(void){
  //
  // !RETURN VALUE:
  //    none
  //
  // !ARGUMENTS:
  // none
  //
  // !DESCRIPTION:
  //  Destructor for PointList, deallocates all internal memory, etc.
  //
  //EOPI
  //-----------------------------------------------------------------------------

  // Reset default values

    max_num_pts=0;
    curr_num_pts=0;
    coord_dim=0;

    // Deallocate memory
    if (points!=NULL) delete [] points;
    points=NULL;
  }



#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::PointList::add()"
  //BOPI
  // !IROUTINE:  add
  //
  // !INTERFACE:
  int PointList::add(

    //
    // !RETURN VALUE:
    //  none
    //
    // !ARGUMENTS:
    //
                    int _id,
                    const double *_coord
                    ) {
    //
    // !DESCRIPTION:
    // Add a point to the PointList.
    //
    //EOPI
    //-----------------------------------------------------------------------------

    // initialize return code; assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;         // local return code
    int rc = ESMC_RC_NOT_IMPL;              // final return code

    // Error check
    if (curr_num_pts >= max_num_pts) {
      int localrc;
      ESMC_LogDefault.MsgFoundError(ESMC_RC_VAL_WRONG,
                                    "- attempting to add to a full PointList ",
                                    ESMC_CONTEXT, &localrc);
      throw localrc;
    }

    // IF START EXTENDING POINT LIST WHEN OVER SIZE, THEN SWITCH TO VECTORS


    // Add point id
    points[curr_num_pts].id = _id;

    // Add point coords
    for (int i=0; i<coord_dim; i++) {
      points[curr_num_pts].coords[i] = _coord[i];
    }

    // Advance to next position
    curr_num_pts++;

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;


  }

#undef  ESMC_METHOD
#define ESMC_METHOD "ESMCI::PointList::sort_by_id()"
  //BOPI
  // !IROUTINE:  sort_by_id
  //
  // !INTERFACE:
  int PointList::sort_by_id() {

    //
    // !RETURN VALUE:
    //  none
    //
    // !ARGUMENTS:
    //
    //  none
    //
    // !DESCRIPTION:
    // Add a point to the PointList.
    //
    //EOPI
    //-----------------------------------------------------------------------------

    // initialize return code; assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;         // local return code
    int rc = ESMC_RC_NOT_IMPL;              // final return code

    int temp_int;
    double temp_double;
    double *pnt_coord_basei;
    double *pnt_coord_basej;

    //    double tbeg,tend;
    //    VMK::wtime(&tbeg);
    std::sort(points,points+curr_num_pts);
    //    VMK::wtime(&t65);
    //    printf("sort time= %.4f\n",tend-tbeg);
    //    fflush(stdout);

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;


  }

#if 0  //mvr

  //mvr not currently getting used, may need some testing
  #undef  ESMC_METHOD
  #define ESMC_METHOD "ESMCI::PointList::operator=()"
  //BOPI
  // !IROUTINE:  operator=
  //
  // !INTERFACE:
  PointList &PointList::operator=(PointList &rhs)
  {

    // Deallocate memory
    if (coords!=NULL) delete [] coords;
    if (ids!=NULL) delete [] ids;

    curr_num_pts=rhs.get_curr_num_pts();
    max_num_pts=rhs.get_max_num_pts();
    coord_dim=rhs.get_coord_dim();

    coords=new double [coord_dim*max_num_pts];
    ids=new int [max_num_pts];

    int num_bytes=coord_dim*max_num_pts*sizeof(double);
    memcpy(coords,rhs.get_coord_ptr(0),num_bytes);
    num_bytes=max_num_pts*sizeof(int);
    memcpy(ids,rhs.get_id_ptr(0),num_bytes);
    return *this;
  }

#endif   //mvr

  #undef  ESMC_METHOD
  #define ESMC_METHOD "ESMCI::PointList::WriteVTK()"
  //BOPI
  // !IROUTINE:  diagprint
  //
  // !INTERFACE:
  int PointList::WriteVTK(const char *filename

    //
    // !RETURN VALUE:
    //  none
    //
    // !ARGUMENTS:
    //
                          ) {
    //
    // !DESCRIPTION:
    // dump contents of PointList to stdout.
    //
    //EOPI
    //-----------------------------------------------------------------------------

    // initialize return code; assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;         // local return code
    int rc = ESMC_RC_NOT_IMPL;              // final return code

    ofstream myfile;

#define MAX_W3PTV_STR_LEN 1000
    char new_filename[MAX_W3PTV_STR_LEN];

    if (strlen(filename)+4 > MAX_W3PTV_STR_LEN) {
      printf("ERROR: filename too long!!!\n");
      return ESMF_FAILURE;
    }

    sprintf(new_filename,"%s.vtk",filename);

    myfile.open (new_filename);
    myfile << "# vtk DataFile Version 3.0\n";
    myfile << "This file generated by ESMF\n";
    myfile << "ASCII\n";
    myfile << "DATASET POLYDATA\n";
    myfile << "\n";
    myfile << "POINTS "<<curr_num_pts<<" double\n";

    if (coord_dim == 3) {
      for (int i=0; i<curr_num_pts; i++) {
        myfile << points[i].coords[0] << " " << points[i].coords[1] << " " <<
                  points[i].coords[2] << "\n";
      }
    } else {
      for (int i=0; i<curr_num_pts; i++) {
        myfile << points[i].coords[0] << " " << points[i].coords[1] << " 0.0 \n";
      }
    }
    myfile << "\n";
    myfile << "VERTICES "<<curr_num_pts<<" "<<2*curr_num_pts<<"\n";
    for (int i=0; i<curr_num_pts; i++) {
      myfile << "1 " << i <<"\n";
    }
    myfile << "\n";
    myfile << "POINT_DATA "<<curr_num_pts<<"\n";
    myfile << "SCALARS Point_Ids double 1 \n";
    myfile << "LOOKUP_TABLE default \n";
    for (int i=0; i<curr_num_pts; i++){
      myfile << points[i].id << " ";
    }
    myfile << "\n";
    myfile.close();

#undef MAX_W3PTV_STR_LEN
    // return successfully
    rc = ESMF_SUCCESS;
    return rc;


  }

  #undef  ESMC_METHOD
  #define ESMC_METHOD "ESMCI::PointList::diagprint()"
  //BOPI
  // !IROUTINE:  diagprint
  //
  // !INTERFACE:
  int PointList::diagprint(

    //
    // !RETURN VALUE:
    //  none
    //
    // !ARGUMENTS:
    //
                          ) {
    //
    // !DESCRIPTION:
    // dump contents of PointList to stdout.
    //
    //EOPI
    //-----------------------------------------------------------------------------

    // initialize return code; assume routine not implemented
    int localrc = ESMC_RC_NOT_IMPL;         // local return code
    int rc = ESMC_RC_NOT_IMPL;              // final return code

    for (int i=0; i<curr_num_pts; i++) {

      printf("id= %d  coords= ",points[i].id);
      for (int j=0; j<coord_dim; j++) {
        printf("%.4f  ",points[i].coords[j]);
      }

      printf("\n\n\n");
    }

    // return successfully
    rc = ESMF_SUCCESS;
    return rc;


  }

  #undef  ESMC_METHOD
  #define ESMC_METHOD "ESMCI::PointList::get_id_ptr()"
  //BOPI
  // !IROUTINE:  get_id_ptr
  //
  // !INTERFACE:
  const int *PointList::get_id_ptr(

    //
    // !RETURN VALUE:
    //  int *
    //
    // !ARGUMENTS:
    //
                                   int loc) const {
    //
    // !DESCRIPTION:
    // return pointer to the id corresponding to given location.
    //
    //EOPI
    //-----------------------------------------------------------------------------


    if (loc<0 || loc>=curr_num_pts) {
      int localrc;
      ESMC_LogDefault.MsgFoundError(ESMC_RC_VAL_WRONG,
                                    "- invalid location in PointList ",
                                    ESMC_CONTEXT, &localrc);
      throw localrc;
    }

    return &points[loc].id;
  }


  #undef  ESMC_METHOD
  #define ESMC_METHOD "ESMCI::PointList::get_coord()"
  //BOPI
  // !IROUTINE:  get_coord
  //
  // !INTERFACE:
  void PointList::get_coord(

    //
    // !RETURN VALUE:
    //  none
    //
    // !ARGUMENTS:
    //
                            int loc,double *_coord) const {
    //
    // !DESCRIPTION:
    // copy coordinates corresponding to given location into a given array.
    //
    //EOPI
    //-----------------------------------------------------------------------------


    if (loc<0 || loc>=curr_num_pts) {
      int localrc;
      ESMC_LogDefault.MsgFoundError(ESMC_RC_VAL_WRONG,
                                    "- invalid location in PointList ",
                                    ESMC_CONTEXT, &localrc);
      throw localrc;
    }

    if (_coord == NULL) {
      int localrc;
      ESMC_LogDefault.MsgFoundError(ESMC_RC_VAL_WRONG,
                                    "- provided array pointer is NULL ",
                                    ESMC_CONTEXT, &localrc);
      throw localrc;
    }

    for (int i=0; i<coord_dim; i++) {
      _coord[i]=points[loc].coords[i];
    }

    return;
  }


  #undef  ESMC_METHOD
  #define ESMC_METHOD "ESMCI::PointList::get_id()"
  //BOPI
  // !IROUTINE:  get_id
  //
  // !INTERFACE:
  int PointList::get_id(

    //
    // !RETURN VALUE:
    //  int
    //
    // !ARGUMENTS:
    //
                                      int loc) const {
    //
    // !DESCRIPTION:
    // return the id corresponding to given location.
    //
    //EOPI
    //-----------------------------------------------------------------------------


    if (loc<0 || loc>=curr_num_pts) {
      int localrc;
      ESMC_LogDefault.MsgFoundError(ESMC_RC_VAL_WRONG,
                                    "- invalid location in PointList ",
                                    ESMC_CONTEXT, &localrc);
      throw localrc;
    }

    return points[loc].id;
  }


  #undef  ESMC_METHOD
  #define ESMC_METHOD "ESMCI::PointList::get_point()"
  //BOPI
  // !IROUTINE:  get_point
  //
  // !INTERFACE:
  point *PointList::get_point(

    //
    // !RETURN VALUE:
    //  point *
    //
    // !ARGUMENTS:
    //
                                      int loc) const {
    //
    // !DESCRIPTION:
    // return a reference to the id corresponding to given location.
    //
    //EOPI
    //-----------------------------------------------------------------------------


    if (loc<0 || loc>=curr_num_pts) {
      int localrc;
      ESMC_LogDefault.MsgFoundError(ESMC_RC_VAL_WRONG,
                                    "- invalid location in PointList ",
                                    ESMC_CONTEXT, &localrc);
      throw localrc;
    }

    return &points[loc];
  }


  #undef  ESMC_METHOD
  #define ESMC_METHOD "ESMCI::PointList::get_coord_ptr()"
  //BOPI
  // !IROUTINE:  get_coord_ptr
  //
  // !INTERFACE:
  const double *PointList::get_coord_ptr(

    //
    // !RETURN VALUE:
    //  double *
    //
    // !ARGUMENTS:
    //
                                      int loc) const {
    //
    // !DESCRIPTION:
    // return pointer to coordinates corresponding to given location.
    //
    //EOPI
    //-----------------------------------------------------------------------------


    if (loc<0 || loc>=curr_num_pts) {
      int localrc;
      ESMC_LogDefault.MsgFoundError(ESMC_RC_VAL_WRONG,
                                    "- invalid location in PointList ",
                                    ESMC_CONTEXT, &localrc);
      throw localrc;
    }

    return points[loc].coords;
  }

} // namespace ESMCI
