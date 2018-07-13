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
//==============================================================================
#include <Mesh/include/Regridding/ESMCI_ConserveInterp.h>
#include <Mesh/include/Legacy/ESMCI_Exception.h>
#include <Mesh/include/Legacy/ESMCI_MeshObjConn.h>
#include <Mesh/include/Legacy/ESMCI_MeshUtils.h>
#include <Mesh/include/Legacy/ESMCI_MEValues.h>
#include <Mesh/include/Legacy/ESMCI_Polynomial.h>
#include <Mesh/include/Legacy/ESMCI_MeshField.h>
#include <Mesh/include/Legacy/ESMCI_MeshTypes.h>
#include <Mesh/include/Legacy/ESMCI_Ftn.h>
#include <Mesh/include/Legacy/ESMCI_ParEnv.h>
#include <Mesh/include/ESMCI_MathUtil.h>

#include <iostream>
#include <iterator>
#include <iomanip>
#include <cmath>
#include <vector>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif


namespace ESMCI {

  bool mathutil_debug=false;

  ///////// File for random math routines that I didn't know where to put ///////////


//// These should eventually be moved elsewhere (perhaps into ESMCI_ShapeFunc.C??)
// INSTEAD OF THIS USE MACRO!
void mult(double m[], double v[], double out_v[]) {

  out_v[0]=m[0]*v[0]+m[1]*v[1]+m[2]*v[2];
  out_v[1]=m[3]*v[0]+m[4]*v[1]+m[5]*v[2];
  out_v[2]=m[6]*v[0]+m[7]*v[1]+m[8]*v[2];

}

// returns true if matrix is inverted, false otherwise
bool invert_matrix_3x3(double m[], double m_inv[]) {

  const double det =  m[0] * (m[4]*m[8] - m[5]*m[7])
                     -m[1] * (m[3]*m[8] - m[5]*m[6])
                     +m[2] * (m[3]*m[7] - m[4]*m[6]);

  // If det == 0.0 we can't invert
  if (!MU_IS_FINITE(det)) return false;
  if (det == 0.0) return false;

  const double deti = 1.0/det;

  m_inv[0] = (m[4]*m[8] - m[5]*m[7]) * deti;
  m_inv[1] = (m[2]*m[7] - m[1]*m[8]) * deti;
  m_inv[2] = (m[1]*m[5] - m[2]*m[4]) * deti;

  m_inv[3] = (m[5]*m[6] - m[3]*m[8]) * deti;
  m_inv[4] = (m[0]*m[8] - m[2]*m[6]) * deti;
  m_inv[5] = (m[2]*m[3] - m[0]*m[5]) * deti;

  m_inv[6] = (m[3]*m[7] - m[4]*m[6]) * deti;
  m_inv[7] = (m[1]*m[6] - m[0]*m[7]) * deti;
  m_inv[8] = (m[0]*m[4] - m[1]*m[3]) * deti;

  return true;
}


// Intersects between the quad q (entries in counterclockwise order)
// and the line determined by the endpoints l1 and l2
// returns true if the two intersect and the output variables are valid
// outputs p containing the coordinates in the quad and t the coordinate in the line
// of the intersection.
// NOTE: the intersection doesn't have to be inside the quad or line for this to return true
bool intersect_quad_with_line(const double *q, const double *l1, const double *l2, double *p,
                              double *t) {

  double A[3], B[3], C[3], D[3], E[3], F[3];
  double J[3*3], inv_J[3*3];
  double X[3], delta_X[3];

  const double *q0=q;
  const double *q1=q+3;
  const double *q2=q+6;
  const double *q3=q+9;

  // in case we need to rotate
  int rotate_cntr_clk = 0;

  // If q0 and q1 are the same then it causes a problem, so if they are then rotate
  if (MU_EQUAL_PNT3D(q0,q1,1.0E-20)) {
    const double *tmp=q3;
    q3=q2;
    q2=q1;
    q1=q0;
    q0=tmp;
    rotate_cntr_clk = 1;
  }

  // If q0 and q3 are the same then it causes a problem, so if they are then rotate
  if (MU_EQUAL_PNT3D(q0,q3,1.0E-20)) {
    const double *tmp;
    tmp=q3;
    q3=q1;
    q1=tmp;
    tmp=q2;
    q2=q0;
    q0=tmp;
    rotate_cntr_clk = 2;
  }


  // Set some convenient variables
  A[0]=q0[0]-q1[0]+q2[0]-q3[0];
  A[1]=q0[1]-q1[1]+q2[1]-q3[1];
  A[2]=q0[2]-q1[2]+q2[2]-q3[2];

  B[0]=q1[0]-q0[0];
  B[1]=q1[1]-q0[1];
  B[2]=q1[2]-q0[2];

  C[0]=q3[0]-q0[0];
  C[1]=q3[1]-q0[1];
  C[2]=q3[2]-q0[2];

  D[0]=l1[0]-l2[0];
  D[1]=l1[1]-l2[1];
  D[2]=l1[2]-l2[2];

  E[0]=q0[0]-l1[0];
  E[1]=q0[1]-l1[1];
  E[2]=q0[2]-l1[2];

  // Initialize answer
  X[0]=0.0;
  X[1]=0.0;
  X[2]=0.0;

  // Do multiple iterations, exiting inside loop if solution is good enough
 for (int i=0; i<100; i++) {

    // Calculate Value of function at X
    F[0]=X[0]*X[1]*A[0]+X[0]*B[0]+X[1]*C[0]+X[2]*D[0]+E[0];
    F[1]=X[0]*X[1]*A[1]+X[0]*B[1]+X[1]*C[1]+X[2]*D[1]+E[1];
    F[2]=X[0]*X[1]*A[2]+X[0]*B[2]+X[1]*C[2]+X[2]*D[2]+E[2];

    // If we're close enough to 0.0 then exit
     if (F[0]*F[0]+F[1]*F[1]+F[2]*F[2] < 1.0E-20) break;

    // Construct Jacobian
    J[0]=A[0]*X[1]+B[0]; J[1]=A[0]*X[0]+C[0]; J[2]=D[0];
    J[3]=A[1]*X[1]+B[1]; J[4]=A[1]*X[0]+C[1]; J[5]=D[1];
    J[6]=A[2]*X[1]+B[2]; J[7]=A[2]*X[0]+C[2]; J[8]=D[2];

    // Invert Jacobian
    if (!invert_matrix_3x3(J,inv_J)) {
      return false;
    }

    // Calculate change in X
    mult(inv_J, F, delta_X);

    // Move to next approximation of X
    X[0] = X[0] - delta_X[0];
    X[1] = X[1] - delta_X[1];
    X[2] = X[2] - delta_X[2];
  }

 // If not finite then return as not mapped
 if (!MU_IS_FINITE(X[0]) ||
     !MU_IS_FINITE(X[1]) ||
     !MU_IS_FINITE(X[2])) {

   return false;
 }
  // Get answer out
 if (rotate_cntr_clk==0) {
   p[0]=X[0];
   p[1]=X[1];
 } else if (rotate_cntr_clk==1) {
   p[0]=X[1];
   p[1]=1.0-X[0];
 } else if (rotate_cntr_clk==2) {
   p[0]=1.0-X[0];
   p[1]=1.0-X[1];
 }

  *t=X[2];


  //  if (mathutil_debug) {
  //  printf("Q: p=[%f %f] t=%f \n",p[0],p[1],X[2]);
  //}

  return true;
}

//// Intersect a line and a tri
// Intersects between the tri t (entries in counterclockwise order)
// and the line determined by the endpoints l1 and l2 (t=0.0 at l1 and t=1.0 at l2)
// returns true if the two intersect and the output variables are valid
// outputs p containing the coordinates in the tri and t the coordinate in the line
// of the intersection.
// NOTE: the intersection doesn't have to be inside the tri or line for this to return true
bool intersect_tri_with_line(const double *tri, const double *l1, const double *l2, double *p,
                             double *t) {

  double M[3*3], inv_M[3*3];
   double V[3];
  double X[3];

  const double *tri0=tri;
  const double *tri1=tri+3;
  const double *tri2=tri+6;

  // To do intersection just solve the set of linear equations for both
  // Setup M
  M[0]=l1[0]-l2[0]; M[1]=tri1[0]-tri0[0]; M[2]=tri2[0]-tri0[0];
  M[3]=l1[1]-l2[1]; M[4]=tri1[1]-tri0[1]; M[5]=tri2[1]-tri0[1];
  M[6]=l1[2]-l2[2]; M[7]=tri1[2]-tri0[2]; M[8]=tri2[2]-tri0[2];


  // Invert M
  if (!invert_matrix_3x3(M,inv_M)) return false;

  // Set variable holding vector
  V[0]=l1[0]-tri0[0];
  V[1]=l1[1]-tri0[1];
  V[2]=l1[2]-tri0[2];

  // Calculate solution
  mult(inv_M, V, X);

 // If not finite then return as not mapped
 if (!MU_IS_FINITE(X[0]) ||
     !MU_IS_FINITE(X[1]) ||
     !MU_IS_FINITE(X[2])) return false;

  // Get answer out
  *t=X[0];
  p[0]=X[1];
  p[1]=X[2];

  return true;
}


  // Num is the number of vertices in the polygon
  // coords is of size 2*num
  double area_of_flat_2D_polygon(int num, double *coords) {

    double area=0;
    for(int i=0; i<num; i++) {
      int ip1=(i+1)%num; // i plus 1 mod the size to wrap at end

      area += (coords[2*i]*coords[2*ip1+1]-coords[2*ip1]*coords[2*i+1]);
    }

    return 0.5*area;

  }


double tri_area(const double * const u, const double * const v, const double * const w) {

  // temporary vector used below
  double tmp_vec[3];

  // Compute length of side a (i.e. the angle from the center of the sphere)
  MU_CROSS_PRODUCT_VEC3D(tmp_vec,u,v);
  double sina=MU_LEN_VEC3D(tmp_vec);
  double cosa=MU_DOT_VEC3D(u,v);
  double a=atan2(sina,cosa);

  // Compute length of side b (i.e. the angle from the center of the sphere)
  MU_CROSS_PRODUCT_VEC3D(tmp_vec,u,w);
  double sinb=MU_LEN_VEC3D(tmp_vec);
  double cosb=MU_DOT_VEC3D(u,w);
  double b=atan2(sinb,cosb);

  // Compute length of side b (i.e. the angle from the center of the sphere)
  MU_CROSS_PRODUCT_VEC3D(tmp_vec,w,v);
  double sinc=MU_LEN_VEC3D(tmp_vec);
  double cosc=MU_DOT_VEC3D(w,v);
  double c=atan2(sinc,cosc);

  // Compute semi-perimeter
  double s=0.5*(a+b+c);

  // Compute t
  double t = tan( 0.5*s ) * tan( 0.5*(s-a) ) *
             tan( 0.5*(s-b) ) * tan( 0.5*(s-c) );

  // For debugging
  //  if (mathutil_debug) {
  //  printf("gca sina=%f sinb=%f sinc=%f \n",sina,sinb,sinc);
  //  printf("gca a=%f b=%f c=%f \n",a,b,c);
  //  printf("gca t=%f \n",t);
  //}

  // Use t to compute triangle area
  double area= std::abs( 4.0*atan( sqrt( std::abs(t) ) ) );

  // Output area
  return area;
}

// Compute the great circle area of a polygon on a sphere
double great_circle_area(int n, double *pnts) {

  // Make sure that it's at least a triangle
  if (n < 3) Throw() << "Can't compute the area of a polygon containing fewer than 3 corners.";

  // sum areas around polygon
  double sum=0.0;
  double *pnt0=pnts;
  for (int i=1; i<n-1; i++) {
    // points that make up a side of polygon
    double *pnt1=pnts+3*i;
    double *pnt2=pnts+3*(i+1);

    // compute angle for pnt1
    sum += tri_area(pnt0, pnt1, pnt2);
  }

  // return area
  return sum;
}



  // Not really a math routine, but useful as a starting point for math routines
  void get_elem_coords(const MeshObj *elem, const MEField<>  *cfield, int sdim, int max_num_nodes, int *num_nodes, double *coords) {

      // Get number of nodes in element
      const ESMCI::MeshObjTopo *topo = ESMCI::GetMeshObjTopo(*elem);

      // make sure that we're not bigger than max size
      if (topo->num_nodes > max_num_nodes) {
        Throw() << "Element exceeds maximum poly size";
      }

      // Get coords of element
      int k=0;
      for (ESMCI::UInt s = 0; s < topo->num_nodes; ++s){
        const MeshObj &node = *(elem->Relations[s].obj);
        double *c = cfield->data(node);
        for (int i=0; i<sdim; i++) {
          coords[k]=c[i];
          k++;
        }
      }

      // Get number of nodes
      *num_nodes=topo->num_nodes;
  }



  // Get coords, but flip so always counter clockwise
  // Also gets rid of degenerate edges
  // This version only works for elements of parametric_dimension = 2 and spatial_dim=2
  void get_elem_coords_2D_ccw(const MeshObj *elem, MEField<>  *cfield, int max_num_nodes,double *tmp_coords,
                              int *num_nodes, double *coords) {
    int num_tmp_nodes;

    // Get element coords
    get_elem_coords(elem, cfield, 2, max_num_nodes, &num_tmp_nodes, tmp_coords);

    // Remove degenerate edges
    remove_0len_edges2D(&num_tmp_nodes, tmp_coords);

    // Check if degenerate
    // (if there's less than 3 no notion of CCW or CW)
    if (num_tmp_nodes <3) {
      int j=0;
      for (int i=0; i<num_tmp_nodes; i++) {
        coords[j]=tmp_coords[j];
        j++;
        coords[j]=tmp_coords[j];
        j++;
      }

      *num_nodes=num_tmp_nodes;
      return;
    }

    // Get elem rotation
    bool left_turn;
    bool right_turn;
    rot_2D_2D_cart(num_tmp_nodes, tmp_coords, &left_turn, &right_turn);

    // Copy to output array swapping if necessary
    if (left_turn) {
      // Don't Swap
      int j=0;
      for (int i=0; i<num_tmp_nodes; i++) {
        coords[j]=tmp_coords[j];
        j++;
        coords[j]=tmp_coords[j];
        j++;
      }
    } else {
      // Swap
      int j=0; int k=2*(num_tmp_nodes-1);
      for (int i=0; i<num_tmp_nodes; i++) {
        coords[j]=tmp_coords[k];
        coords[j+1]=tmp_coords[k+1];
        j+=2; k-=2;
      }
    }


  // Output num nodes
  *num_nodes=num_tmp_nodes;
}



  // Get coords, but flip so always counter clockwise
  // Also gets rid of degenerate edges
  // This version only works for elements of parametric_dimension = 2 and spatial_dim=2
  void get_elem_coords_3D_ccw(const MeshObj *elem, MEField<>  *cfield, int max_num_nodes,double *tmp_coords,
                              int *num_nodes, double *coords) {
    int num_tmp_nodes;

    // Get element coords
    get_elem_coords(elem, cfield, 3, max_num_nodes, &num_tmp_nodes, tmp_coords);

    // Remove degenerate edges
    remove_0len_edges3D(&num_tmp_nodes, tmp_coords);

    // Check if degenerate
    // (if there's less than 3 no notion of CCW or CW)
    if (num_tmp_nodes <3) {
      int j=0;
      for (int i=0; i<num_tmp_nodes; i++) {
        coords[j]=tmp_coords[j];
        j++;
        coords[j]=tmp_coords[j];
        j++;
        coords[j]=tmp_coords[j];
        j++;
      }
      *num_nodes=num_tmp_nodes;

      return;
    }

    // Get elem rotation
    bool left_turn;
    bool right_turn;
    rot_2D_3D_sph(num_tmp_nodes, tmp_coords, &left_turn, &right_turn);

    // Copy to output array swapping if necessary
    if (left_turn) {
      // Don't Swap
      int j=0;
      for (int i=0; i<num_tmp_nodes; i++) {
        coords[j]=tmp_coords[j];
        j++;
        coords[j]=tmp_coords[j];
        j++;
        coords[j]=tmp_coords[j];
        j++;
      }
    } else {
      // Swap
      int j=0; int k=3*(num_tmp_nodes-1);
      for (int i=0; i<num_tmp_nodes; i++) {
        coords[j]=tmp_coords[k];
        coords[j+1]=tmp_coords[k+1];
        coords[j+2]=tmp_coords[k+2];
        j+=3; k-=3;
      }
    }


  // Output num nodes
  *num_nodes=num_tmp_nodes;
}





  // Not really a math routine, but useful as a starting point for math routines
  void get_elem_coords_and_ids(const MeshObj *elem, MEField<>  *cfield, int sdim, int max_num_nodes, int *num_nodes, double *coords, int *ids) {

      // Get number of nodes in element
      const ESMCI::MeshObjTopo *topo = ESMCI::GetMeshObjTopo(*elem);

      // make sure that we're not bigger than max size
      if (topo->num_nodes > max_num_nodes) {
        Throw() << "Element exceeds maximum poly size";
      }

      // Get coords of element
      int k=0;
      for (ESMCI::UInt s = 0; s < topo->num_nodes; ++s){
        const MeshObj &node = *(elem->Relations[s].obj);
        double *c = cfield->data(node);
        for (int i=0; i<sdim; i++) {
          coords[k]=c[i];
          k++;
        }
        ids[s]=node.get_id();
      }

      // Get number of nodes
      *num_nodes=topo->num_nodes;
  }




  // first_remove_ind is the index of the first point that was removed.
  // if nothing was removed then it'll be equal to -1
  void remove_0len_edges3D(int *num_p, double *p, int *_first_remove_ind) {

#define EQUAL_TOL 1E-15
#define PNTS_EQUAL(p1,p2) ((std::abs(p1[0]-p2[0]) < EQUAL_TOL) &&       \
                           (std::abs(p1[1]-p2[1]) < EQUAL_TOL) &&       \
                           (std::abs(p1[2]-p2[2]) < EQUAL_TOL))

  // Init first remove ind
  int first_remove_ind=-1;

  // Get old value of num_p
  int old_num_p=*num_p;

  // See if there are any equal points
  int j=-1;
  double *last_pnt=p+3*(old_num_p-1);
  for (int i=0; i<old_num_p; i++) {
    double *pnti=p+3*i;

    if (PNTS_EQUAL(pnti,last_pnt)) {
      j=i;
      break;
    }

    // advance last point
    last_pnt=pnti;
  }

  // We found an equal point so start trimming them out
  if (j>-1) {
    // Set first trimmed ind
    first_remove_ind=j;

    for (int i=j; i<old_num_p; i++) {
      double *pnti=p+3*i;
      if (!PNTS_EQUAL(pnti,last_pnt)) {
        double *pntj=p+3*j;

        // copy the non-equal point to j
        pntj[0]=pnti[0];
        pntj[1]=pnti[1];
        pntj[2]=pnti[2];

        // move j
        j++;

        // reset the last pointer to the last non-repeating value
        last_pnt=pntj;
      }
    }

    // reset num_p to the new number of points
    *num_p=j;
  } else {
    // Leave num_p as it is
  }

  // if output var exists then output first removed point
  if (_first_remove_ind !=NULL) *_first_remove_ind=first_remove_ind;

#undef EQUAL_TOL
#undef PNTS_EQUAL
}


void remove_0len_edges2D(int *num_p, double *p) {

#define EQUAL_TOL 1E-15
#define PNTS_EQUAL(p1,p2) ((std::abs(p1[0]-p2[0]) < EQUAL_TOL) &&       \
                           (std::abs(p1[1]-p2[1]) < EQUAL_TOL))

  // Get old value of num_p
  int old_num_p=*num_p;

  // See if there are any equal points
  int j=-1;
  double *last_pnt=p+2*(old_num_p-1);
  for (int i=0; i<old_num_p; i++) {
    double *pnti=p+2*i;

    if (PNTS_EQUAL(pnti,last_pnt)) {
      j=i;
      break;
    }

    // advance last point
    last_pnt=pnti;
  }

  // We found an equal point so start trimming them out
  if (j>-1) {
    for (int i=j; i<old_num_p; i++) {
      double *pnti=p+2*i;
      if (!PNTS_EQUAL(pnti,last_pnt)) {
        double *pntj=p+2*j;

        // copy the non-equal point to j
        pntj[0]=pnti[0];
        pntj[1]=pnti[1];

        // move j
        j++;

        // reset the last pointer to the last non-repeating value
        last_pnt=pntj;
      }
    }

    // reset num_p to the new number of points
    *num_p=j;
  } else {
    // Leave num_p as it is
  }

#undef EQUAL_TOL
#undef PNTS_EQUAL
}



  // Is this a smashed quad?
  // (i.e. a quad with opposite corners equal)
bool is_smashed_quad2D(int num_p, double *p) {

#define EQUAL_TOL 1E-15
#define PNTS_EQUAL(p1,p2) ((std::abs((p1)[0]-(p2)[0]) < EQUAL_TOL) &&   \
                           (std::abs((p1)[1]-(p2)[1]) < EQUAL_TOL))


  // If not a quad, then leave
  if (num_p != 4) return false;

  // See if 1st point is equal to 3rd point
  if (PNTS_EQUAL(p,p+4)) return true;

  // See if 2nd point is equal to 4th point
  if (PNTS_EQUAL(p+2,p+6)) return true;

  // not a smashed quad
  return false;

#undef EQUAL_TOL
#undef PNTS_EQUAL
}


  // Is this a smashed quad?
  // (i.e. a quad with opposite corners equal)
bool is_smashed_quad3D(int num_p, double *p) {

#define EQUAL_TOL 1E-15
#define PNTS_EQUAL(p1,p2) ((std::abs((p1)[0]-(p2)[0]) < EQUAL_TOL) &&   \
                           (std::abs((p1)[1]-(p2)[1]) < EQUAL_TOL) &&   \
                           (std::abs((p1)[2]-(p2)[2]) < EQUAL_TOL))


  // If not a quad, then leave
  if (num_p != 4) return false;

  // See if 1st point is equal to 3rd point
  if (PNTS_EQUAL(p,p+6)) return true;

  // See if 2nd point is equal to 4th point
  if (PNTS_EQUAL(p+3,p+9)) return true;

  // not a smashed quad
  return false;

#undef EQUAL_TOL
#undef PNTS_EQUAL
}

  void write_2D_poly_woid_to_vtk(const char *filename, int num_p, double *p) {
    std::ofstream myfile;
#define MAX_W3PTV_STR_LEN 1000
    char new_filename[MAX_W3PTV_STR_LEN];

    if (strlen(filename)+4 > MAX_W3PTV_STR_LEN) {
       printf("ERROR: filename too long!!!\n");
       return;
    }

    sprintf(new_filename,"%s.vtk",filename);

    myfile.open (new_filename);

    myfile << "# vtk DataFile Version 3.0\n";
    myfile << "This file generated by ESMF\n";
    myfile << "ASCII\n";
    myfile << "DATASET UNSTRUCTURED_GRID\n";
    myfile << "POINTS "<<num_p<<" double\n";
    for (int i=0; i<num_p ; i++){
      myfile << p[2*i]<<" "<< " "<<p[2*i+1] <<" 0.0 \n";
    }
    myfile << "CELLS 1 "<<num_p+1<<"\n";
    myfile << num_p <<" ";
    for (int i=0; i<num_p ; i++){
      myfile << i <<" ";
    }
    myfile << "\n";
    myfile << "CELL_TYPES 1 \n";
    myfile << "7 \n";
    myfile << "POINT_DATA "<<num_p<<"\n";
    myfile << "SCALARS POINT_INDEX double 1 \n";
    myfile << "LOOKUP_TABLE default \n";
    for (int i=0; i<num_p ; i++){
      myfile << i <<" ";
    }
    myfile << "\n";
    myfile.close();
#undef MAX_W3PTV_STR_LEN
  }


  void write_2D_poly_to_vtk(const char *filename, int id, int num_p, double *p) {
#define MAX_W3PTVID_STR_LEN 1000
    char new_filename[MAX_W3PTVID_STR_LEN];

    if (((double)strlen(filename))+log10((double)id)+1.0 > ((double)MAX_W3PTVID_STR_LEN)) {
       printf("ERROR: filename too long!!!\n");
       return;
    }

    sprintf(new_filename,"%s%d",filename,id);

    write_2D_poly_woid_to_vtk(new_filename,num_p,p);

#undef MAX_W3PTVID_STR_LEN
  }


  void write_3D_poly_woid_to_vtk(const char *filename, int num_p, double *p) {
    std::ofstream myfile;
#define MAX_W3PTV_STR_LEN 1000
    char new_filename[MAX_W3PTV_STR_LEN];

    if (strlen(filename)+4 > MAX_W3PTV_STR_LEN) {
       printf("ERROR: filename too long!!!\n");
       return;
    }

    sprintf(new_filename,"%s.vtk",filename);

    myfile.open (new_filename);

    myfile << "# vtk DataFile Version 3.0\n";
    myfile << "This file generated by ESMF\n";
    myfile << "ASCII\n";
    myfile << "DATASET UNSTRUCTURED_GRID\n";
    myfile << "POINTS "<<num_p<<" double\n";
    for (int i=0; i<num_p ; i++){
      myfile << p[3*i]<<" "<< " "<<p[3*i+1] <<" "<< p[3*i+2]<<"\n";
    }
    myfile << "CELLS 1 "<<num_p+1<<"\n";
    myfile << num_p <<" ";
    for (int i=0; i<num_p ; i++){
      myfile << i <<" ";
    }
    myfile << "\n";
    myfile << "CELL_TYPES 1 \n";
    myfile << "7 \n";
    myfile << "POINT_DATA "<<num_p<<"\n";
    myfile << "SCALARS POINT_INDEX double 1 \n";
    myfile << "LOOKUP_TABLE default \n";
    for (int i=0; i<num_p ; i++){
      myfile << i <<" ";
    }
    myfile << "\n";
    myfile.close();
#undef MAX_W3PTV_STR_LEN
  }

  void write_3D_poly_to_vtk(const char *filename, int id, int num_p, double *p) {
#define MAX_W3PTVID_STR_LEN 1000
    char new_filename[MAX_W3PTVID_STR_LEN];

    if (((double)strlen(filename))+log10((double)id)+1.0 > ((double)MAX_W3PTVID_STR_LEN)) {
       printf("ERROR: filename too long!!!\n");
       return;
    }

    sprintf(new_filename,"%s%d",filename,id);

    write_3D_poly_woid_to_vtk(new_filename,num_p,p);

#undef MAX_W3PTVID_STR_LEN
  }



void rot_2D_2D_cart(int num_p, double *p, bool *left_turn, bool *right_turn) {

  // Define Cross product
#define CROSS_PRODUCT2D(out,a,b) out=a[0]*b[1]-a[1]*b[0];
#define TOL 1.0E-17

  // init flags
  *left_turn=false;
  *right_turn=false;

  // Loop through polygon
  for (int i=0; i<num_p; i++) {
    double *pntip0=p+2*i;
    double *pntip1=p+2*((i+1)%num_p);
    double *pntip2=p+2*((i+2)%num_p);

    // vector from pntip1 to pnti0
    double v10[2];
    v10[0]=pntip0[0]-pntip1[0];
    v10[1]=pntip0[1]-pntip1[1];


    // vector from pntip1 to pnti2
    double v12[2];
    v12[0]=pntip2[0]-pntip1[0];
    v12[1]=pntip2[1]-pntip1[1];

    // Calc cross product
    double cross;
    CROSS_PRODUCT2D(cross,v12,v10);

    // Interpret direction
    if (cross > TOL) {
      *left_turn=true;
    } else if (cross < -TOL) {
      *right_turn=true;
    }
  }

  // If no turns default to left
  if (!(*right_turn) && !(*left_turn)) *left_turn=true;


#undef CROSS_PRODUCT2D
#undef TOL
}





void rot_2D_3D_sph(int num_p, double *p, bool *left_turn, bool *right_turn) {

  // Define Cross product
#define CROSS_PRODUCT3D(out,a,b) out[0]=a[1]*b[2]-a[2]*b[1]; out[1]=a[2]*b[0]-a[0]*b[2]; out[2]=a[0]*b[1]-a[1]*b[0];
#define DOT_PRODUCT3D(a,b) a[0]*b[0]+a[1]*b[1]+a[2]*b[2];
#define TOL 1.0E-17

  // init flags
  *left_turn=false;
  *right_turn=false;

  // Loop through polygon
  for (int i=0; i<num_p; i++) {
    double *pntip0=p+3*i;
    double *pntip1=p+3*((i+1)%num_p);
    double *pntip2=p+3*((i+2)%num_p);

    // vector from pntip1 to pnti0
    double v10[3];
    v10[0]=pntip0[0]-pntip1[0];
    v10[1]=pntip0[1]-pntip1[1];
    v10[2]=pntip0[2]-pntip1[2];


    // vector from pntip1 to pnti2
    double v12[3];
    v12[0]=pntip2[0]-pntip1[0];
    v12[1]=pntip2[1]-pntip1[1];
    v12[2]=pntip2[2]-pntip1[2];

    // Calc cross product
    double cross[3];
    CROSS_PRODUCT3D(cross,v12,v10);

    // dot cross product with vector from center of sphere
    // to middle point (pntip1)
    double dir=DOT_PRODUCT3D(cross,pntip1);

    // Interpret direction
    if (dir > TOL) {
      *left_turn=true;
    } else if (dir < -TOL) {
      *right_turn=true;
    }
  }

  // If no turns default to left
  if (!(*right_turn) && !(*left_turn)) *left_turn=true;

#undef CROSS_PRODUCT3D
#undef TOL

}


  // Detect if a point (pnt) is in a polygon (p)
  // here the points are 2D and the polygon is counter-clockwise
  // returns true if the point is in the polygon (including on the edge), and
  // false otherwise
template <class GEOM>
  bool is_pnt_in_poly(int num_p, double *p, double *pnt) {

  // Loop through polygon
  for (int i=0; i<num_p; i++) {
    double *pntip0=GEOM::getPntAt(p,i);
    double *pntip1=GEOM::getPntAt(p,(i+1)%num_p);

    // vector from pntip0 to pnti1
    double v01[GEOM::pnt_size];
    GEOM::sub(v01,pntip1,pntip0);

    // vector from pntip0 to pnt
    double v0p[GEOM::pnt_size];
    GEOM::sub(v0p,pnt,pntip0);

    // Calc cross product
    double cross;
    cross=GEOM::turn(v01,v0p,pntip0);

    //    printf("%d pnt0=%f %f turn=%f\n",i,pntip0[0],pntip0[1],cross);

    if (cross <= 0.0) {
      return false;
    }
  }

  return true;
}

// Create instances for supported geometries, otherwise would have to put template
// in include file
template bool is_pnt_in_poly<GEOM_CART2D>(int num_p, double *p, double *pnt);
template bool is_pnt_in_poly<GEOM_SPH2D3D>(int num_p, double *p, double *pnt);

// See if any other points in the polygon are in the triangle formed by ind[0], ind[1], ind[2]
template<class GEOM>
bool is_ear(int num_p, double *p, int *ind) {

  // Form triangle
  double tri[3*GEOM::pnt_size];

  GEOM::copy(GEOM::getPntAt(tri,0),GEOM::getPntAt(p,ind[0]));
  GEOM::copy(GEOM::getPntAt(tri,1),GEOM::getPntAt(p,ind[1]));
  GEOM::copy(GEOM::getPntAt(tri,2),GEOM::getPntAt(p,ind[2]));

  // Check other points in polygon
  for (int i=0; i<num_p; i++) {
    if (i==ind[0]) continue;
    if (i==ind[1]) continue;
    if (i==ind[2]) continue;
    if (is_pnt_in_poly<GEOM>(3, tri, GEOM::getPntAt(p,i))) return false;
  }

  return true;
}

// Create instances for supported geometries, otherwise would have to put template
// in include file
template bool is_ear<GEOM_CART2D>(int num_p, double *p, int *ind);
template bool is_ear<GEOM_SPH2D3D>(int num_p, double *p, int *ind);


// Triangulate a 2D polygon using the ear clip method.
// This method works on both concave and convex polygons.
// As usual in ESMF Mesh this assumes the polygon is counter-clockwise.
// Output is in tri_ind, which are the 0-based indices of the triangles
// making up the triangulization. tri_ind should be of size 3*(num_p-2)
// td should be the same size as p, ti should be of size num_p.
template <class GEOM>
int triangulate_poly(int num_p, double *p, double *td, int *ti, int *tri_ind) {

  // Error check
  if (num_p < 3) {
    return ESMCI_TP_DEGENERATE_POLY;
  }

  // Handle degenerate case
  if (num_p == 3) {
    tri_ind[0]=0;
    tri_ind[1]=1;
    tri_ind[2]=2;
    return ESMCI_TP_SUCCESS;
  }

  // Copy polygon to temporary array
  memcpy((double *)td, (double *)p, GEOM::pnt_size*num_p*sizeof(double));
  int num_t=num_p;

  // Fill index array
  for(int i=0; i<num_p; i++) {
    ti[i]=i;
  }


  // Loop until we've broken everything up
  int pos_tri_ind=0;
  while (true) {

    // Handle triangle case
    if (num_t == 3) {
      tri_ind[pos_tri_ind]=ti[0];
      tri_ind[pos_tri_ind+1]=ti[1];
      tri_ind[pos_tri_ind+2]=ti[2];
      return ESMCI_TP_SUCCESS;
    }

    // Loop through polygon
    int max_clip_ind[3];
    double max_clip_dot=-std::numeric_limits<double>::max();
    bool found_clip=false;
    for (int i=0; i<num_t; i++) {
      // indices which make up triangle to potentially clip
      int clip_ind[3];
      clip_ind[0]=i;
      clip_ind[1]=(i+1)%num_t;
      clip_ind[2]=(i+2)%num_t;

      // Points which make up triangle
      double *pntip0=GEOM::getPntAt(td,clip_ind[0]);
      double *pntip1=GEOM::getPntAt(td,clip_ind[1]);
      double *pntip2=GEOM::getPntAt(td,clip_ind[2]);

      // vector from pntip1 to pnti0
      double v10[GEOM::pnt_size];
      GEOM::sub(v10,pntip0,pntip1);

      // vector from pntip1 to pnti2
      double v12[GEOM::pnt_size];
      GEOM::sub(v12,pntip2,pntip1);

      // Calc cross product
      double cross;
      cross=GEOM::turn(v12,v10,pntip1);

      // Find the maximum left turn to clip
      // to give good triangles
      if (cross > 0.0) {
        double dot;
        dot=GEOM::dot(v12,v10);
        //printf("%d dot=%f max=%f \n",i,dot,max_clip_dot);
        if (dot > max_clip_dot) {
          bool is_ear_b=is_ear<GEOM>(num_t, td, clip_ind);
          //printf("%d  is_ear_b=%d \n",i,is_ear_b);
          if (is_ear_b) {
          //          if (is_ear<GEOM>(num_t, td, clip_ind)) {
            max_clip_dot=dot;
            max_clip_ind[0]=clip_ind[0];
            max_clip_ind[1]=clip_ind[1];
            max_clip_ind[2]=clip_ind[2];
            found_clip=true;
          }
        }
      }
    }

    // Clip
    if (found_clip) {
      // Add clipped triangle to list
      tri_ind[pos_tri_ind]=ti[max_clip_ind[0]];
      tri_ind[pos_tri_ind+1]=ti[max_clip_ind[1]];
      tri_ind[pos_tri_ind+2]=ti[max_clip_ind[2]];
      pos_tri_ind +=3;

      // remove triangle from polygon and collapse arrays
      for (int j=max_clip_ind[1]+1; j<num_t; j++) {
        GEOM::copy(GEOM::getPntAt(td,j-1),GEOM::getPntAt(td,j));
        ti[j-1]=ti[j];
      }

      // shrink size by 1
      num_t--;

      // Loop back to beginning
      continue;
    } else {
      return ESMCI_TP_CLOCKWISE_POLY;
    }
  } // Loop back to top before triangle detection

}

// Create instances for supported geometries, otherwise would have to put template
// in include file
template int triangulate_poly<GEOM_CART2D>(int num_p, double *p, double *td, int *ti, int *tri_ind);
template int triangulate_poly<GEOM_SPH2D3D>(int num_p, double *p, double *td, int *ti, int *tri_ind);


// calculates spherical coords in radians
// lon -> (-pi to +pi)
// lat -> (0   to +pi)
void convert_cart_to_sph(double x, double y, double z,
                         double *lon, double *lat, double *r) {

  // calc radius of sphere
  *r=sqrt(x*x+y*y+z*z);

  // calc lon
  *lon=atan2(y,x);

  // calc lat
  *lat=acos(z/(*r));
}


// calculates spherical coords in degs
// lon -> (-180 to 180)
// lat -> (-90  to 90)
void convert_cart_to_sph_deg(double x, double y, double z,
                             double *lon, double *lat, double *r) {

  const double RAD2DEG=57.295779513082325;
  double lon_r,lat_r;

  convert_cart_to_sph(x, y, z,
                      &lon_r, &lat_r, r);

  *lon=lon_r*RAD2DEG;
  *lat=90.0-(lat_r*RAD2DEG);
}


// Assumes pnt1, pnt2, pnt3 are of size 3
void calc_plane_equation(double *pnt1, double *pnt2, double *pnt3, double *a, double *b, double *c, double *d) {
#define CROSS_PRODUCT3D(out,a,b) out[0]=a[1]*b[2]-a[2]*b[1]; out[1]=a[2]*b[0]-a[0]*b[2]; out[2]=a[0]*b[1]-a[1]*b[0];

  // vector from pnt2 to pnt1
  double vec21[3];
  vec21[0]=pnt1[0]-pnt2[0];
  vec21[1]=pnt1[1]-pnt2[1];
  vec21[2]=pnt1[2]-pnt2[2];

  // vector from pnt2 to pnt3
  double vec23[3];
  vec23[0]=pnt3[0]-pnt2[0];
  vec23[1]=pnt3[1]-pnt2[1];
  vec23[2]=pnt3[2]-pnt2[2];


  // Calc outward (from sphere) normal to plane
  double normal[3];
  CROSS_PRODUCT3D(normal,vec23,vec21);

  // check if 0.0
  if ((normal[0] == 0.0) &&
      (normal[1] == 0.0) &&
      (normal[2] == 0.0)) {
    Throw() << " Points are collinear, so can't compute normal";
  }

  // get plane equation from normal
  *a=normal[0];
  *b=normal[1];
  *c=normal[2];
  *d=-normal[0]*pnt2[0]-normal[1]*pnt2[1]-normal[2]*pnt2[2];

#undef CROSS_PRODUCT3D
}

// Assumes all variables are of size 3
// returns 0 for success, 1 otherwise
void calc_sph_mmbox(double *pnt1, double *pnt2, double *pnt3, double *min, double *max) {
  double a,b,c,d;

  // Get plane equation
  calc_plane_equation(pnt1, pnt2, pnt3, &a, &b, &c, &d);

  // Calc. distance from plane to sphere
  double dist_p_to_sph;
  dist_p_to_sph=1.0-(std::abs(d)/std::sqrt(a*a+b*b+c*c));

  // Calculate normal to plane with length dist_p_to_sph
  double p_norm_len=std::sqrt(a*a+b*b+c*c);
  double p_normal[3];
  p_normal[0]=dist_p_to_sph*(a/p_norm_len);
  p_normal[1]=dist_p_to_sph*(b/p_norm_len);
  p_normal[2]=dist_p_to_sph*(c/p_norm_len);


  // compute other set of points to enclose sphere section
  double pnt1_pls_vec[3];
  double pnt2_pls_vec[3];
  double pnt3_pls_vec[3];

  MU_ADD_VEC3D(pnt1_pls_vec,pnt1,p_normal);
  MU_ADD_VEC3D(pnt2_pls_vec,pnt2,p_normal);
  MU_ADD_VEC3D(pnt3_pls_vec,pnt3,p_normal);

  // Compute min-max box
  //// set to pnt1
  min[0]=pnt1[0]; min[1]=pnt1[1]; min[2]=pnt1[2];
  max[0]=pnt1[0]; max[1]=pnt1[1]; max[2]=pnt1[2];

  //// modify by pnt2
  MU_SET_MIN_VEC3D(min,pnt2);
  MU_SET_MAX_VEC3D(max,pnt2);

  //// modify by pnt3
  MU_SET_MIN_VEC3D(min,pnt3);
  MU_SET_MAX_VEC3D(max,pnt3);

  //// modify by pnt1_pls_vec
  MU_SET_MIN_VEC3D(min,pnt1_pls_vec);
  MU_SET_MAX_VEC3D(max,pnt1_pls_vec);

  //// modify by pnt2_pls_vec
  MU_SET_MIN_VEC3D(min,pnt2_pls_vec);
  MU_SET_MAX_VEC3D(max,pnt2_pls_vec);

  //// modify by pnt3_pls_vec
  MU_SET_MIN_VEC3D(min,pnt3_pls_vec);
  MU_SET_MAX_VEC3D(max,pnt3_pls_vec);

}

#if 0
  // Compute the angle (in radians) around normal between two 3D vectors a and b
  // CURRENTLY ONLY WORKS FOR -pi/2 to pi/2
  // Returns 0 if successful, 1 otherwise
  int angle_between_VEC3D(double *a, double *b, double*normal, double *_angle) {

    // Compute cross product
    double cross_ab[3];
    MU_CROSS_PRODUCT_VEC3D(cross_ab,a,b);

    // Compute vector lengths
    double cross_ab_len=MU_LEN_VEC3D(cross_ab);
    double a_len=MU_LEN_VEC3D(a);
    double b_len=MU_LEN_VEC3D(b);

    // adjust to be negative if going in other direction
    if (MU_DOT_VEC3D(cross_ab,normal)<0.0) cross_ab_len=-cross_ab_len;

    // Error Check bottom
    if ((a_len==0.0) || (b_len==0.0)) return 1;

    // Compute sin value
  double sin_ab=cross_ab_len/(a_len*b_len);

   // Clamp if out of range
   if (sin_ab<-1.0) sin_ab=-1.0;
   else if (sin_ab>1.0) sin_ab=1.0;

   // Compute angle
   *_angle_=std::asin(sin_ab);

   // Return success
   return 0;
  }
#endif

// Inputs: q1, q2, q3, q4 are 3D cartesian points located on
// a sphere (in counter-clockwise order) making up two planes (with the sphere center (0,0,0)).
// Each of these should be of size 3 doubles.
// Outputs: p - position between plane formed by q1 and q2 and  plane formed by q3 to q4
int calc_gc_parameter_2planes(const double *pnt, double *q1, double *q2, double *q3, double *q4,
                              double *p) {

  // Compute normal to plane through q1 q2 and center (origin)
  double normal_12[3];
  MU_CROSS_PRODUCT_VEC3D(normal_12,q1,q2);

  // Compute normal to plane through q3 q4 and center (origin)
  double normal_34[3];
  MU_CROSS_PRODUCT_VEC3D(normal_34,q3,q4);

  // Compute normal to normals
  // (This is a vector parallel to both planes)
  double parallel[3];
  MU_CROSS_PRODUCT_VEC3D(parallel,normal_12,normal_34);

  // Compute vector lengths
  double parallel_len=MU_LEN_VEC3D(parallel);
   double normal_12_len=MU_LEN_VEC3D(normal_12);
  double normal_34_len=MU_LEN_VEC3D(normal_34);

  // Error Check bottom
  if ((normal_12_len==0.0) || (normal_34_len==0.0)) return 1;

  // compute sin
  double sin_1234=parallel_len/(normal_12_len*normal_34_len);

   // Clamp if out of range
   if (sin_1234<-1.0) sin_1234=-1.0;
   else if (sin_1234>1.0) sin_1234=1.0;

  // Compute the angle between the planes
  double angle_1234=std::asin(sin_1234);

  // parallelx(pntxparallel)  = a pnt projected to the plane perp. to parallel
  double tmp[3];
  MU_CROSS_PRODUCT_VEC3D(tmp,pnt,parallel);
  double pnt_in_plane[3];
  MU_CROSS_PRODUCT_VEC3D(pnt_in_plane,parallel,tmp);


  // parallelx(pntxparallel)  = a pnt projected to the plane perp. to parallel
  MU_CROSS_PRODUCT_VEC3D(tmp,q1,parallel);
  double q1_in_plane[3];
  MU_CROSS_PRODUCT_VEC3D(q1_in_plane,parallel,tmp);


  // Angle from plane 12 to pnt
  double cross_1_to_pnt[3];
  MU_CROSS_PRODUCT_VEC3D(cross_1_to_pnt,pnt_in_plane,q1_in_plane);

   // Compute vector lengths
  double cross_1_to_pnt_len=MU_LEN_VEC3D(cross_1_to_pnt);
  double pnt_in_plane_len=MU_LEN_VEC3D(pnt_in_plane);
  double q1_in_plane_len=MU_LEN_VEC3D(q1_in_plane);

  // adjust to be negative if going in other direction
  if (MU_DOT_VEC3D(cross_1_to_pnt,parallel)<0.0) cross_1_to_pnt_len=-cross_1_to_pnt_len;

  // Error Check
  if ((q1_in_plane_len==0.0) || (pnt_in_plane_len==0.0)) return 1;

  // compute sin
  double sin_12pnt=cross_1_to_pnt_len/(q1_in_plane_len*pnt_in_plane_len);

   // Clamp if out of range
   if (sin_12pnt<-1.0) sin_12pnt=-1.0;
   else if (sin_12pnt>1.0) sin_12pnt=1.0;

   // Compute cosine
   double dot_12pnt=MU_DOT_VEC3D(pnt_in_plane,q1_in_plane);

   double cos_12pnt=dot_12pnt/(q1_in_plane_len*pnt_in_plane_len);

   // Clamp if out of range
   if (cos_12pnt<-1.0) cos_12pnt=-1.0;
   else if (cos_12pnt>1.0) cos_12pnt=1.0;


   // Compute angle
   double angle_12pnt=std::atan2(sin_12pnt,cos_12pnt);


  // Compute angle
   //double angle_12pnt=std::asin(sin_12pnt);

#if 0
  if (mathutil_debug) {

    printf("cross_1_len=%f q1_len=%f pnt_len=%f q1*pnt=%f\n",cross_1_to_pnt_len,q1_in_plane_len,pnt_in_plane_len,q1_in_plane_len*pnt_in_plane_len);

    printf("angle_12pnt=%30.27f\n",angle_12pnt);

    printf("angle_1234=%30.27f\n",angle_1234);
  }
#endif

  // Error check output
  if (angle_1234==0.0) return 1;

  // Output P
  *p=angle_12pnt/angle_1234;

  // return success
  return 0;
}


// Inputs: pnt - is the point to determine the location of
// q1, q2, q3, q4 are 3D cartesian points located on
// a sphere (in counter-clockwise order) making up the quad. Each of these should be of size 3 doubles.
// Outputs: p1 - position in q1 to q2 / q3 to q4 direction
//          p2 - position in q1 to q4 / q2 to q3 direction
int calc_gc_parameters_quad(const double *pnt, double *q1, double *q2, double *q3, double *q4,
                            double *p1, double *p2) {


#ifdef ESMF_REGRID_DEBUG_MAP_NODE
  if (mathutil_debug) {
    double lon,lat,r;

    printf("GC QUAD: \n");

    // Pnt
    convert_cart_to_sph_deg(pnt[0], pnt[1], pnt[2],
                        &lon, &lat, &r);
    printf("   pnt=%f %f\n",lon,lat);


    // q1
    convert_cart_to_sph_deg(q1[0], q1[1], q1[2],
                        &lon, &lat, &r);
    printf("   q1=%f %f\n",lon,lat);

    // q2
    convert_cart_to_sph_deg(q2[0], q2[1], q2[2],
                        &lon, &lat, &r);
    printf("   q2=%f %f\n",lon,lat);

    // q3
    convert_cart_to_sph_deg(q3[0], q3[1], q3[2],
                        &lon, &lat, &r);
    printf("   q3=%f %f\n",lon,lat);

    // q4
    convert_cart_to_sph_deg(q4[0], q4[1], q4[2],
                        &lon, &lat, &r);
    printf("   q4=%f %f\n",lon,lat);
  }
 #endif


  // Calc parameter p1
  if (calc_gc_parameter_2planes(pnt, q1, q2, q3, q4, p1)) return 1;

  // Calc parameter p2
  if (calc_gc_parameter_2planes(pnt, q2, q3, q4, q1, p2)) return 1;


  // return success
  return 0;
}


#if 0

// Inputs: t1, t2 are 3D cartesian points located on
// a sphere making up a plane (with the sphere center(0,0,0)). Each of these should be of size 3 doubles.
// Outputs: p - position parallel to the plane formed by t1 and t2 (and the sphere center).
int calc_gc_parameter_1plane(const double *pnt, double *t1, double *t2, double *p) {

  // Normal to plane of t1 and t2
  double normal[3];
  MU_CROSS_PRODUCT_VEC3D(normal,t1,t2);

  // Compute lengths of vectors
  double normal_len=MU_LEN_VEC3D(normal);
  double t1_len=MU_LEN_VEC3D(t1);
  double t2_len=MU_LEN_VEC3D(t2);

  // Error Check
  if ((t1_len==0.0) || (t2_len==0.0)) return 1;

  // Compute angle between t1 and t2
  double angle_12=std::asin(normal_len/(t1_len*t2_len));

  // Project pnt to plane defined by normal
  // (normalx(pntxnormal))  = pnt projected to the plane perp. to normal
  double tmp[3];
  MU_CROSS_PRODUCT_VEC3D(tmp,pnt,normal);
  double pnt_in_plane[3];
  MU_CROSS_PRODUCT_VEC3D(pnt_in_plane,normal,tmp);

  // Compute angle from t1 to pnt_in_plane
  double cross_1pnt[3];
  MU_CROSS_PRODUCT_VEC3D(cross_1pnt,t1,pnt_in_plane);

   // Compute lengths of vectors
  double cross_1pnt_len=MU_LEN_VEC3D(cross_1pnt);
  double pnt_in_plane_len=MU_LEN_VEC3D(pnt_in_plane);

  // adjust to be negative if going in other direction
  if (MU_DOT_VEC3D(cross_1pnt,normal)<0.0) cross_1pnt_len=-cross_1pnt_len;

  // Error Check
  if ((t1_len==0.0) || (pnt_in_plane_len==0.0)) return 1;

  // Compute angle between t1 and pnt
  double angle_1pnt=std::asin(cross_1pnt_len/(t1_len*pnt_in_plane_len));

  // Error check output
  if (angle_12==0.0) return 1;

  // Calc. output
  *p=angle_1pnt/angle_12;

  // return success
  return 0;
}


// Inputs: pnt - is the point to determine the location of
// t1, t2, t3 are 3D cartesian points located on
// a sphere (in counter-clockwise order) making up the tri. Each of these should be of size 3 doubles.
// Outputs: p1 - position in t1 to t2 direction
//          p2 - position in t2 to t3 direction
int calc_gc_parameters_tri(const double *pnt, double *t1, double *t2, double *t3,
                            double *p1, double *p2) {

  // Calc parameter p1
  if (calc_gc_parameter_1plane(pnt, t1, t2, p1)) return 1;

  // Calc parameter p2
  if (calc_gc_parameter_1plane(pnt, t2, t3, p2)) return 1;


  // return success
  return 0;
}
#endif






// Inputs: t1, t2 are 3D cartesian points located on
// a sphere making up a plane (with the sphere center(0,0,0)). t3 is a point in front of this plane.
//  Each of these should be of size 3 doubles.
// Outputs: p - position between plane formed by t1 and t2 (and the sphere center) and point t3
int calc_gc_parameter_1plane(const double *pnt, double *t1, double *t2, double *t3, double *p) {

  // Normal to plane of t1 and t2
  double normal_12[3];
  MU_CROSS_PRODUCT_VEC3D(normal_12,t1,t2);

  // Plane normal
  double pl_normal[3];
  MU_CROSS_PRODUCT_VEC3D(pl_normal,normal_12,t3);


  // Vector in t1-t2 plane in plane defined by pl_normal
  // STOPPED HERE
  double tmp[3];
  MU_CROSS_PRODUCT_VEC3D(tmp,t1,pl_normal);
  double t1_in_plane[3];
  MU_CROSS_PRODUCT_VEC3D(t1_in_plane,pl_normal,tmp);

  // t3 is already within plane defined by pl_normal

  // Compute angle between t1-t2 plane and t3
  double cross_123[3];
  MU_CROSS_PRODUCT_VEC3D(cross_123,t1_in_plane,t3);
  double cross_123_len=MU_LEN_VEC3D(cross_123);
  double t1_in_plane_len=MU_LEN_VEC3D(t1_in_plane);
  double t3_len=MU_LEN_VEC3D(t3);

  // Error Check
  if ((t1_in_plane_len==0.0) || (t3_len==0.0)) return 1;

  // Sin
  double sin_angle_123=cross_123_len/(t1_in_plane_len*t3_len);

  // Clamp to range
  if (sin_angle_123>1.0) sin_angle_123=1.0;
  else   if (sin_angle_123<-1.0) sin_angle_123=-1.0;

  // Compute angle between t1-t2 plane and t3
 double angle_123=std::asin(sin_angle_123);

 //  printf("angle_123=%f\n",angle_123);


  // Project pnt to plane defined by normal
  // (normalx(pntxnormal))  = pnt projected to the plane perp. to normal
  MU_CROSS_PRODUCT_VEC3D(tmp,pnt,pl_normal);
  double pnt_in_plane[3];
  MU_CROSS_PRODUCT_VEC3D(pnt_in_plane,pl_normal,tmp);

  // Compute angle between projected pnt and t1-t2 plane
  double cross_1pnt[3];
  MU_CROSS_PRODUCT_VEC3D(cross_1pnt,t1_in_plane,pnt_in_plane);

   // Compute lengths of vectors
  double cross_1pnt_len=MU_LEN_VEC3D(cross_1pnt);
  double pnt_in_plane_len=MU_LEN_VEC3D(pnt_in_plane);

  // adjust to be negative if going in other direction
  if (MU_DOT_VEC3D(cross_1pnt,pl_normal)>0.0) cross_1pnt_len=-cross_1pnt_len;

  // Error Check
  if ((t1_in_plane_len==0.0) || (pnt_in_plane_len==0.0)) return 1;

  // Sin
  double sin_angle_1pnt=cross_1pnt_len/(t1_in_plane_len*pnt_in_plane_len);

  // Clamp to range
  if (sin_angle_1pnt>1.0) sin_angle_1pnt=1.0;
  else   if (sin_angle_1pnt<-1.0) sin_angle_1pnt=-1.0;

  // Compute angle between t1 and pnt
  double angle_1pnt=std::asin(sin_angle_1pnt);

  //  printf("angle_1pnt=%f\n",angle_1pnt);


  // Error check output
  if (angle_123==0.0) return 1;

  // Calc. output
  *p=angle_1pnt/angle_123;

  // return success
  return 0;
}


// Inputs: pnt - is the point to determine the location of
// t1, t2, t3 are 3D cartesian points located on
// a sphere (in counter-clockwise order) making up the tri. Each of these should be of size 3 doubles.
// Outputs: p1 - position in t1 to t2 direction
//          p2 - position in t2 to t3 direction
int calc_gc_parameters_tri(const double *pnt, double *t1, double *t2, double *t3,
                            double *p1, double *p2) {

#ifdef ESMF_REGRID_DEBUG_MAP_NODE
  if (mathutil_debug) {
    double lon,lat,r;

    printf("GC TRI: \n");

    // Pnt
    convert_cart_to_sph_deg(pnt[0], pnt[1], pnt[2],
                        &lon, &lat, &r);
    printf("   pnt=%f %f\n",lon,lat);


    // t1
    convert_cart_to_sph_deg(t1[0], t1[1], t1[2],
                        &lon, &lat, &r);
    printf("   t1=%f %f\n",lon,lat);

    // t2
    convert_cart_to_sph_deg(t2[0], t2[1], t2[2],
                        &lon, &lat, &r);
    printf("   t2=%f %f\n",lon,lat);

    // t3
    convert_cart_to_sph_deg(t3[0], t3[1], t3[2],
                        &lon, &lat, &r);
    printf("   t3=%f %f\n",lon,lat);

  }
#endif


#if 0
  // Calc parameter p1
  if (calc_gc_parameter_1plane(pnt, t1, t2, t3, p1)) return 1;

  // Calc parameter p2
  if (calc_gc_parameter_1plane(pnt, t2, t3, t1, p2)) return 1;

#else
  double tmp_p1, tmp_p2, tmp_p3;

  // Calc parameter p1
  if (calc_gc_parameter_1plane(pnt, t1, t2, t3, &tmp_p1)) return 1;

  // Calc parameter p2
  if (calc_gc_parameter_1plane(pnt, t2, t3, t1, &tmp_p2)) return 1;

  // Calc parameter p2
  if (calc_gc_parameter_1plane(pnt, t3, t1, t2, &tmp_p3)) return 1;

  *p1=tmp_p1/(tmp_p1+tmp_p2+tmp_p3);

  *p2=tmp_p2/(tmp_p1+tmp_p2+tmp_p3);
#endif



  // return success
  return 0;
}

  //////// NEW STUFF /////
 /* XMRKX */
  // INPUTS:
  //   pnt0 - point at p=0 expressed in x,y,z Cartesian
  //   pnt1 - point at p=1 expressed in x,y,z Cartesian
  //   p    - parameter between pnt0 and pnt1
  // OUTPUTS:
  //   out_pnt - the point at p between pnt0 and pnt1 also expressed in x,y,z Cartesian
  //   out_angle01 - the angle between pnt0 and pnt1
  void sph_comb_pnts(const double *pnt0, const double *pnt1, double p,
       double *out_pnt, double *out_angle01) {

  // Thought about doing a case here if the points are the same, but I think that
  // the below will just work in that case, and this way I don't have to come up
  // with an arbitrary tol for sameness.

  // lengths
  double len0=MU_LEN_VEC3D(pnt0);
  double len1=MU_LEN_VEC3D(pnt1);

  // calc unit vec
  double u_pnt0[3];
  u_pnt0[0] = pnt0[0]/len0;
  u_pnt0[1] = pnt0[1]/len0;
  u_pnt0[2] = pnt0[2]/len0;

  double u_pnt1[3];
  u_pnt1[0] = pnt1[0]/len1;
  u_pnt1[1] = pnt1[1]/len1;
  u_pnt1[2] = pnt1[2]/len1;

  // Compute angle between 0 and 1
  // Right now this only works up to 180 degrees
  // TODO: fix so it works up to 360
  //// cos
  double cos01=MU_DOT_VEC3D(u_pnt0,u_pnt1);

  //// sin
  double u_pnt0x1[3];
  MU_CROSS_PRODUCT_VEC3D(u_pnt0x1,u_pnt0,u_pnt1);
  double sin01=MU_LEN_VEC3D(u_pnt0x1);

  //// Compute angle
  double angle01=std::atan2(sin01,cos01);

  // Compute info to use to compute new point
  double cosp=cos(p*angle01);  // cos contribution
  double sinp=sin(p*angle01);  // sin contribution
  double radp=(1.0-p)*len0+p*len1; // radius linear between pnt0 and pnt1

  // Clamp rad, so less than 0 doesn't count
  if (radp < 0.0) radp=0.0;

  // printf("cosp=%f sinp=%f radp=%f\n",cosp,sinp,radp);

  // Compute vec. perp to u_pnt0
  double tmp[3];
  MU_CROSS_PRODUCT_VEC3D(tmp,u_pnt1,u_pnt0);
  double pntp[3];
  MU_CROSS_PRODUCT_VEC3D(pntp,u_pnt0,tmp);
  double lenp=MU_LEN_VEC3D(pntp);

  // If lenp is 0.0, then the vectors are parallel or 180 degrees
  // TODO: handle 180 case
  // SHOULD I DO THIS ABOVE IF ANGLE01==0.0??
  // A: MAYBE NOT SINCE NO HAVING lenp == 0.0 IS MORE IMPORTANT FOR AVOIDING NAN
  if (lenp == 0.0) {
    out_pnt[0]=radp*u_pnt0[0];
    out_pnt[1]=radp*u_pnt0[1];
    out_pnt[2]=radp*u_pnt0[2];
    *out_angle01=angle01;

    return;
  }

  double u_pntp[3];
  u_pntp[0]=pntp[0]/lenp;
  u_pntp[1]=pntp[1]/lenp;
  u_pntp[2]=pntp[2]/lenp;


  // compute point and output
  out_pnt[0]=radp*(cosp*u_pnt0[0]+sinp*u_pntp[0]);
  out_pnt[1]=radp*(cosp*u_pnt0[1]+sinp*u_pntp[1]);
  out_pnt[2]=radp*(cosp*u_pnt0[2]+sinp*u_pntp[2]);

  *out_angle01=angle01;
}


// QUESTION: SHOULD quad be a set of different input points (e.g. q1,...q4)?
//        A: MAYBE NOT SINCE HAVING 8 entries for a HEX would be inconvienient.
// Take in a spherical quad represented in Cartesian 3D and
// 2 parameter (p) values. Calculate a new point that is at the
// position described by the parameters in the quad.
// quad_xyz    - should be of size 12 (4 Cartesian 3D points)
// p           - should be of size 2  (2 parameters)
// o_pnt       - should be of size 3  (1 Cartesian 3D point)
// o_max_angle - the maximum angle for the cooresponding p values (2 values)
  void calc_pnt_quad_sph3D_xyz(const double *quad_xyz, double *p,
                               double *o_pnt, double *o_max_angle) {
  const double *q0, *q1, *q2, *q3;
  double pnt01[3];
  double pnt32[3];
  double angle;

  // Grab points
  q0=quad_xyz;
  q1=quad_xyz+3;
  q2=quad_xyz+6;
  q3=quad_xyz+9;


  // Side 0-1
  sph_comb_pnts(q0, q1, p[0], pnt01, &angle);
  o_max_angle[0]=angle;

  // Side 3-2
  sph_comb_pnts(q3, q2, p[0], pnt32, &angle);
  if (o_max_angle[0]>angle) o_max_angle[0]=angle;

  // Merge sides 0-1 and 3-2
  sph_comb_pnts(pnt01, pnt32, p[1], o_pnt, &angle);
  o_max_angle[1]=angle;
}


// Take in a spherical hex represented in Cartesian 3D and
// 2 parameter (p) values. Calculate a new point that is at the
// position described by the parameters in the quad.
// hex_xyz - should be of size 24 (8 Cartesian 3D points)
// p        - should be of size 3  (3 parameters)
// o_pnt    - should be of size 3  (1 Cartesian 3D point)
// o_max_angle - the maximum angle for the cooresponding p values (3 values)
  void calc_pnt_hex_sph3D_xyz(const double *hex_xyz, double *p, double *o_pnt, double *o_max_angle) {
  double pnt_btm[3];
  double pnt_top[3];
  double max_angle2D[2];

  // calc the bottom quad
  calc_pnt_quad_sph3D_xyz(hex_xyz, p, pnt_btm, o_max_angle);

  // calc the top quad
  calc_pnt_quad_sph3D_xyz(hex_xyz+12, p, pnt_top, max_angle2D);
  if (max_angle2D[0] > o_max_angle[0]) o_max_angle[0]=max_angle2D[0];
  if (max_angle2D[1] > o_max_angle[1]) o_max_angle[1]=max_angle2D[1];

  // printf("hex_xyz: pnt_btm=[%f %f %f] pnt_top=[%f %f %f] \n",pnt_btm[0],pnt_btm[1],pnt_btm[2],pnt_top[0],pnt_top[1],pnt_top[2]);

  // Merge the points
  sph_comb_pnts(pnt_btm, pnt_top, p[2], o_pnt, o_max_angle+2);
}



// Take in a spherical quad represented in Cartesian 3D and
// 2 parameter (p) values. Calculate the pnt and the jacobian for the
// position described by the parameters in the quad.
// Do both at the same time to avoid having to recalc. o_pnt
  void calc_pnt_and_jac_hex_sph3D_xyz(const double *hex_xyz, double *p,
                              double *o_pnt, double *o_jac, double *o_max_angle) {
    //  double delta=1.0E-14; // Small distance to use to estimate derivative
  double delta=1.0E-10; // Small distance to use to estimate derivative
  double tmp_max_angle[3];

  // Calculate Function with given p's
  double f[3];
  calc_pnt_hex_sph3D_xyz(hex_xyz, p, f, o_max_angle);

  // output point
  o_pnt[0]=f[0];
  o_pnt[1]=f[1];
  o_pnt[2]=f[2];

  // Variable to hold info for computing derivatives
  double tmp_p[3];
  double tmp_f[3];

  // Compute partial by p[0]
  tmp_p[0]=p[0]+delta;
  tmp_p[1]=p[1];
  tmp_p[2]=p[2];

  calc_pnt_hex_sph3D_xyz(hex_xyz, tmp_p, tmp_f, tmp_max_angle);

  o_jac[0]=(tmp_f[0]-f[0])/delta;
  o_jac[3]=(tmp_f[1]-f[1])/delta;
  o_jac[6]=(tmp_f[2]-f[2])/delta;

  // Compute partial by p[1]
  tmp_p[0]=p[0];
  tmp_p[1]=p[1]+delta;
  tmp_p[2]=p[2];

  calc_pnt_hex_sph3D_xyz(hex_xyz, tmp_p, tmp_f, tmp_max_angle);

  o_jac[1]=(tmp_f[0]-f[0])/delta;
  o_jac[4]=(tmp_f[1]-f[1])/delta;
  o_jac[7]=(tmp_f[2]-f[2])/delta;

  // Compute partial by p[2]
  tmp_p[0]=p[0];
  tmp_p[1]=p[1];
  tmp_p[2]=p[2]+delta;

  calc_pnt_hex_sph3D_xyz(hex_xyz, tmp_p, tmp_f, tmp_max_angle);

  o_jac[2]=(tmp_f[0]-f[0])/delta;
  o_jac[5]=(tmp_f[1]-f[1])/delta;
  o_jac[8]=(tmp_f[2]-f[2])/delta;

}


// Take in a spherical hex represented in xyz and
// a point value. Calculate the parameters for where the
// point is in the hex
// hex_xyz - should be of size 24 (8 llr points)
// pnt_xyz  - should be of size 3  (1 llr point)
// p        - should be of size 3  (3 parameters)

// Returns: true - if converged and p is valid, false otherwise
bool calc_p_hex_sph3D_xyz(const double *hex_xyz, const double *pnt_xyz, double *p) {

  // List of optional guesses, and which we're using
  int guess=0;
  double p_guess[8][3]={{0.0,0.0,0.0},
                        {0.0,0.0,1.0},
                        {0.0,1.0,0.0},
                        {0.0,1.0,1.0},
                        {1.0,0.0,0.0},
                        {1.0,0.0,1.0},
                        {1.0,1.0,0.0},
                        {1.0,1.0,1.0}};

  // PIs
  double pi=M_PI;
  double two_pi=2*M_PI;

  // Initial guess
  p[0]=0.5;
  p[1]=0.5;
  p[2]=0.5;


  // Do multiple interations exiting in loop if solution is close enough
  bool converged=false;
  for (int i=0; i<1000; i++) {

#ifdef ESMF_REGRID_DEBUG_MAP_NODE
    if (mathutil_debug) {
      printf("%d --- Begin Iteration %d --- \n",i,i);
      printf("%d p      =%f %f %f \n",i,p[0],p[1],p[2]);
    }
#endif

    // Calculate point and jacobian at p
    double tmp_pnt[3];
    double jac[3*3];
    double max_angle[3];
    calc_pnt_and_jac_hex_sph3D_xyz(hex_xyz, p,
                                   tmp_pnt, jac, max_angle);


#ifdef ESMF_REGRID_DEBUG_MAP_NODE
    if (mathutil_debug) {
      printf("%d tmp_pnt=%f %f %f \n",i,tmp_pnt[0],tmp_pnt[1],tmp_pnt[2]);
      printf("%d jac    =%f %f %f %f %f %f %f %f %f\n",i,jac[0],jac[1],jac[2],jac[3],jac[4],jac[5],jac[6],jac[7],jac[8]);
      printf("%d max_ang=%f %f %f \n",i,max_angle[0],max_angle[1],max_angle[2]);
      // printf("%d ~ang   =%f %f %f \n",i,p[0]*max_angle[0],p[1]*max_angle[1],p[2]*max_angle[2]);
    }
#endif

    // Calculate function we're trying to 0
    // (point at p-pnt_xyz)
    double f[3];
    MU_SUB_VEC3D(f,tmp_pnt,pnt_xyz);

    // Calculate Cart. dist. between point at p and actual point
    double cart_dist_at_p=MU_LEN_VEC3D(f);

    // Invert Jacobian
    double inv_jac[3*3];
    if (!invert_matrix_3x3(jac, inv_jac)) {
      // Oops, couldn't invert, so try another guess
      if (guess<8) {
#ifdef ESMF_REGRID_DEBUG_MAP_NODE
        if (mathutil_debug) {
          printf("%d Couldn't invert so trying guess=%d\n",i,guess);
        }
#endif
        p[0]=p_guess[guess][0];
        p[1]=p_guess[guess][1];
        p[2]=p_guess[guess][2];
        guess++;
      } else { //... if we've tried them all then just stop
#ifdef ESMF_REGRID_DEBUG_MAP_NODE
        if (mathutil_debug) {
          printf("%d Couldn't invert and no more guesses, so giving up and leaving\n",i,guess);
        }
#endif
        break;
      }
    }

    // Calculate change in p
    double delta_p[3];
    MU_MAT_X_VEC3D(delta_p, inv_jac, f);

    // get length of change
    double len_delta_p=MU_LEN_VEC3D(delta_p);

#ifdef ESMF_REGRID_DEBUG_MAP_NODE
    if (mathutil_debug) {
      printf("%d p_dist =%E   cart_dist=%E\n",i,len_delta_p,MU_LEN_VEC3D(f));
    }
#endif

    // The length of the change in p gives an approximation for
    // the distance in p-space from the point.
    // If we're close enough in p-space dist. to be significantly
    // within 1.0E-10 mapping tol and reasonably close in actual Cart. dist.
    // then exit.
    if ((len_delta_p < 1.0E-11) && (cart_dist_at_p < 1.0E-11)) {
#ifdef ESMF_REGRID_DEBUG_MAP_NODE
      if (mathutil_debug) {
        printf("%d Within tols so exiting...\n",i);
      }
#endif
      converged=true;
      break;
    }

    // If change is too big, make it smaller
    if (len_delta_p > 1.0) {
      delta_p[0] = delta_p[0]/len_delta_p;
      delta_p[1] = delta_p[1]/len_delta_p;
      delta_p[2] = delta_p[2]/len_delta_p;
    }

    // Move to next approximation of p
    MU_SUB_VEC3D(p,p,delta_p);


#ifdef ESMF_REGRID_DEBUG_MAP_NODE
    if (mathutil_debug) {
       printf("%d new p  =%f %f %f \n",i,p[0],p[1],p[2]);
     }
#endif

    // Calculate half the max_angle to make things easier below
    double half_max_angle[3];
    half_max_angle[0]=0.5*max_angle[0];
    half_max_angle[1]=0.5*max_angle[1];
    half_max_angle[2]=0.5*max_angle[2];

    // If p is wrapping around, then modify to be within range
    if (p[0]*max_angle[0] > pi+half_max_angle[0]) {
      // how many turns
      double turns=trunc(p[0]*max_angle[0]/two_pi);
 #ifdef ESMF_REGRID_DEBUG_MAP_NODE
      if (mathutil_debug) printf("       P[0] big p=%f angle=%f turns=%f -> ",p[0],p[0]*max_angle[0],turns);
#endif
       // subtract off all the complete turns
      p[0]=p[0]-turns*two_pi/max_angle[0];

       // If still too big one more turn to push to pos
        if (p[0]*max_angle[0] > pi+half_max_angle[0]) {
        p[0]=p[0]-two_pi/max_angle[0];
      }
#ifdef ESMF_REGRID_DEBUG_MAP_NODE
      if (mathutil_debug) printf(" AFTER P[0] big p=%f angle=%f \n",p[0],p[0]*max_angle[0]);
#endif
    } else if (p[0]*max_angle[0] < -pi+half_max_angle[0]) {
      // how many complete turns
      double turns=trunc(std::abs(p[0]*max_angle[0])/two_pi);
#ifdef ESMF_REGRID_DEBUG_MAP_NODE
      if (mathutil_debug) printf("       P[0] small p=%f angle=%f turns=%f -> ",p[0],p[0]*max_angle[0],turns);
#endif
      // Add on all the complete turns
       p[0]=p[0]+turns*two_pi/max_angle[0];

       // If still inside cell on neg. side add one more turn to push to pos
      if (p[0]*max_angle[0] < -pi+half_max_angle[0]) {
        p[0]=p[0]+two_pi/max_angle[0];
      }
#ifdef ESMF_REGRID_DEBUG_MAP_NODE
      if (mathutil_debug) printf(" AFTER P[0] small p=%f angle=%f \n",p[0],p[0]*max_angle[0]);
#endif
    }

    if (p[1]*max_angle[1] > pi+half_max_angle[1]) {
      // how many turns
       double turns=trunc(p[1]*max_angle[1]/two_pi);
#ifdef ESMF_REGRID_DEBUG_MAP_NODE
       if (mathutil_debug) printf("       P[1] big p=%f angle=%f turns=%f -> ",p[1],p[1]*max_angle[1],turns);
#endif
      // subtract off all the complete turns
      p[1]=p[1]-turns*two_pi/max_angle[1];

        // If still too big one more turn to push to pos
      if (p[1]*max_angle[1] > pi+half_max_angle[1]) {
        p[1]=p[1]-two_pi/max_angle[1];
      }
#ifdef ESMF_REGRID_DEBUG_MAP_NODE
      if (mathutil_debug) printf(" AFTER P[1] big p=%f angle=%f \n",p[1],p[1]*max_angle[1]);
#endif
    } else if (p[1]*max_angle[1] < -pi+half_max_angle[1]) {
      // how many complete turns
      double turns=trunc(std::abs(p[1]*max_angle[1])/two_pi);
#ifdef ESMF_REGRID_DEBUG_MAP_NODE
      if (mathutil_debug) printf("       P[1] small p=%f angle=%f turns=%f -> ",p[1],p[1]*max_angle[1],turns);
#endif
       // Add on all the complete turns
      p[1]=p[1]+turns*two_pi/max_angle[1];

       // If still inside cell on neg. side add one more turn to push to pos
      if (p[1]*max_angle[1] < -pi+half_max_angle[1]) {
        p[1]=p[1]+two_pi/max_angle[1];
      }
#ifdef ESMF_REGRID_DEBUG_MAP_NODE
      if (mathutil_debug) printf(" AFTER P[1] small p=%f angle=%f \n",p[1],p[1]*max_angle[1]);
#endif
    }

    if (p[2]*max_angle[2] > pi+half_max_angle[2]) {
       // how many turns
      double turns=trunc(p[2]*max_angle[2]/two_pi);
#ifdef ESMF_REGRID_DEBUG_MAP_NODE
      if (mathutil_debug) printf("       P[2] big p=%f angle=%f turns=%f -> ",p[2],p[2]*max_angle[2],turns);
#endif
      // subtract off all the complete turns
      p[2]=p[2]-turns*two_pi/max_angle[2];

       // If still too big one more turn to push to pos
      if (p[2]*max_angle[2] > pi+half_max_angle[2]) {
        p[2]=p[2]-two_pi/max_angle[2];
      }
#ifdef ESMF_REGRID_DEBUG_MAP_NODE
      if (mathutil_debug) printf(" AFTER P[2] big p=%f angle=%f \n",p[2],p[2]*max_angle[2]);
#endif
    } else if (p[2]*max_angle[2] < -pi+half_max_angle[2]) {
      // how many complete turns
      double turns=trunc(std::abs(p[2]*max_angle[2])/two_pi);
#ifdef ESMF_REGRID_DEBUG_MAP_NODE
      if (mathutil_debug) printf("       P[2] small p=%f angle=%f turns=%f -> ",p[2],p[2]*max_angle[2],turns);
 #endif
       // Add on all the complete turns
      p[2]=p[2]+turns*two_pi/max_angle[2];

       // If still inside cell on neg. side add one more turn to push to pos
      if (p[2]*max_angle[2] < -pi+half_max_angle[2]) {
        p[2]=p[2]+two_pi/max_angle[2];
      }
#ifdef ESMF_REGRID_DEBUG_MAP_NODE
      if (mathutil_debug) printf(" AFTER P[2] small p=%f angle=%f \n",p[2],p[2]*max_angle[2]);
#endif
    }

   }

  // Report if we've converged close enough to the actual point
  if (converged) return true;

  // Otherwise  return false
  return false;
}

  // Do some quick checks to see if the point is
  // definitely outside the hex.
  //
  // Take in a spherical hex represented in xyz and
  // a point value.
  // hex_xyz - should be of size 24 (8 llr points)
  // pnt_xyz  - should be of size 3  (1 llr point)
  //
  // Returns: true - if the point is definitely outside the hex, false otherwise
  bool is_outside_hex_sph3D_xyz(const double *hex_xyz, const double *pnt_xyz) {
#define TOL 1.0E-10

    // Calc point radius squared
    double pnt_radsq=MU_LENSQ_VEC3D(pnt_xyz);

    // Loop through hex points calculating the min and max radius
    double min_hex_radsq=std::numeric_limits<double>::max();
    double max_hex_radsq=-std::numeric_limits<double>::max();
    for (int i=0; i<8; i++) {
      // Get hex pnt
      const double *hex_pnt=hex_xyz+3*i;

      // Calculate radius of hex point
      double hex_radsq=MU_LENSQ_VEC3D(hex_pnt);

      // Calulate min and max
      if (hex_radsq < min_hex_radsq) min_hex_radsq=hex_radsq;
      if (hex_radsq > max_hex_radsq) max_hex_radsq=hex_radsq;
    }

    // See if we're outside the hex
    if (pnt_radsq < min_hex_radsq - TOL) return true;
    if (pnt_radsq > max_hex_radsq + TOL) return true;


    return false;
#undef TOL
  }

  // Calculate the counter-clockwise angle from a to b
  // a, b, un should be arrays of the correct size for the geometry
  // un is a unit normal pointing out of the surface
  //      (e.g. away from the center of the sphere)
  // un isn't used in all cases (e.g. with GEOM_CART2D)
  template <>
  double calc_angle<GEOM_CART2D>(double *a, double *b, double *un) {
    double dot = a[0]*b[0] + a[1]*b[1];
    double det = a[0]*b[1] - a[1]*b[0];
    return atan2(det, dot);
  }

  template <>
  double calc_angle<GEOM_SPH2D3D>(double *a, double *b, double *un) {
    double dot = a[0]*b[0] + a[1]*b[1] + a[2]*b[2];
    double det = a[0]*b[1]*un[2] + b[0]*un[1]*a[2] + un[0]*a[1]*b[2] - a[2]*b[1]*un[0] - b[2]*un[1]*a[0] - un[2]*a[1]*b[0];
    return atan2(det, dot);
  }

  // Count the number of 0len edges
  void count_0len_edges3D(int num_p, double *p, int *_num_0len) {

#define EQUAL_TOL 1E-15
#define PNTS_EQUAL(p1,p2) ((std::abs(p1[0]-p2[0]) < EQUAL_TOL) &&       \
                           (std::abs(p1[1]-p2[1]) < EQUAL_TOL) &&       \
                           (std::abs(p1[2]-p2[2]) < EQUAL_TOL))

    // Init to 0
    int num_0len=0;

    // See if there are any equal points
    double *last_pnt=p+3*(num_p-1);
    for (int i=0; i<num_p; i++) {
      double *pnti=p+3*i;

      if (PNTS_EQUAL(pnti,last_pnt)) {
        num_0len++;
      }

      // advance last point
      last_pnt=pnti;
    }

    // Do output
    *_num_0len=num_0len;

#undef EQUAL_TOL
#undef PNTS_EQUAL
}


  // Intersects between the line a and the seqment s
  // where both line and segment are great circle lines on the sphere represented by
  // 3D cartesian points.
  // Subroutine eturns true if we should add point p.
  // sin is the end of the segment inside the polygon
  // sout is the end of the segment outside the polygon
  // This subroutine is set up to be used with the poly intersect
  // code below, and has a number of tweaks for special cases
  // which might make it odd to be used as a general intesect code.
  bool line_with_gc_seg3D(double *a1, double *a2, double *sin, double *sout,
                       double *p) {

    //// If we're too close to parallel to be accurate then just treat like parallel ////
#define PARALLEL_TOL 1.0E-10
    double plane_norm[3];
    MU_CROSS_PRODUCT_VEC3D(plane_norm,a1,a2);

    // Get length of plane norm
    double len_plane_norm=MU_LEN_VEC3D(plane_norm);

    // Get vector from sin to sout
    double sio_vec[3];
    MU_SUB_VEC3D(sio_vec,sout,sin);

    // Get length of t12_vec
    double len_sio_vec=MU_LEN_VEC3D(sio_vec);

    // calculate dot product
    double cos_plane_sio=MU_DOT_VEC3D(plane_norm,sio_vec);

    // divide by lengths to get cos
    if (len_plane_norm != 0.0)  cos_plane_sio=cos_plane_sio/len_plane_norm;
    if (len_sio_vec != 0.0) cos_plane_sio=cos_plane_sio/len_sio_vec;

    if(std::abs(cos_plane_sio) < PARALLEL_TOL) {
      p[0]=sout[0];
      p[1]=sout[1];
      p[2]=sout[2];

      return true;
    }
#undef PARALLEL_TOL


    // Do this intersection by reprsenting the line a1 to a2 as a plane through the
    // two line points and the origin of the sphere (0,0,0). This is the
    // definition of a great circle arc.
    double plane[9];
    double plane_p[2];
    double t;

    // Load points defining plane into variable (these are supposed to be in counterclockwise order)
    plane[0]=a1[0];
    plane[1]=a1[1];
    plane[2]=a1[2];
    plane[3]=a2[0];
    plane[4]=a2[1];
    plane[5]=a2[2];
    plane[6]=0.0;
    plane[7]=0.0;
    plane[8]=0.0;


    // Intersect the segment with the plane
    if(!intersect_tri_with_line(plane, sin, sout, plane_p, &t)) {
      // If can't intesect then the lines and plane are parallel, this
      // shouldn't happen, but if it does it makes the
      // most sense to add the out point
      p[0]=sout[0];
      p[1]=sout[1];
      p[2]=sout[2];
       return true;
    }

    // We shouldn't be off the ends, but
    // if we are because of rounding then
    // do what makes sense

    // if we're off the in end, then don't add
    if (t<0.0) return false;

    // if we're off the out end, then just add the
    // out point
    if (t>=1.0) {
      p[0]=sout[0];
      p[1]=sout[1];
      p[2]=sout[2];
      return true;
    }

    // Otherwise calculate point of intersection
    // and add that
    p[0]=sin[0] + t*(sout[0]-sin[0]);
    p[1]=sin[1] + t*(sout[1]-sin[1]);
    p[2]=sin[2] + t*(sout[2]-sin[2]);
    return true;
  }




  // intersects convex polygons whose vertices are stored in counter clockwise
  // order and which lie on a sphere. Note that the polygons have 3D coordinates
  // but are only 2D in the parameter space of the sphere. This subroutine
  // assumes that the polygons have great circle edges
  // p should be of size 3*num_p
  // q should be of size 3*num_q
  // tmp and out should both be allocated to be at least of size 3*(num_p+num_q)
  void intersect_convex_2D_3D_sph_gc_poly(int num_p, double *p,
                               int num_q, double *q,
                               double *tmp,
                               int *num_out, double *out)
  {

#define CROSS_PRODUCT3D(out,a,b) out[0]=a[1]*b[2]-a[2]*b[1]; out[1]=a[2]*b[0]-a[0]*b[2]; out[2]=a[0]*b[1]-a[1]*b[0];
#define DOT_PRODUCT3D(a,b) (a[0]*b[0]+a[1]*b[1]+a[2]*b[2])
#define NORM(a) sqrt(a[0]*a[0]+a[1]*a[1]+a[2]*a[2])
#define CLIP_EQUAL_TOL 1.0e-20
#define CLOSE_TO_ZERO(vec) ((std::abs(vec[0])<CLIP_EQUAL_TOL) && (std::abs(vec[1])<CLIP_EQUAL_TOL) && (std::abs(vec[2])<CLIP_EQUAL_TOL))

    // If p is empty then leave
    if (num_p==0) {
      *num_out=0;
    }

    // If q is empty then leave
    if (num_q==0) {
      *num_out=0;
    }

    // INSTEAD OF TMP USE T???

    // Copy q into tmp
    double *end_q=q+3*num_q;
    for (double *q_i=q, *tmp_i=tmp; q_i<end_q; q_i++, tmp_i++) {
      *tmp_i=*q_i;
    }
    int num_tmp=num_q;

    // Setup alias for output array
    int num_o=0;
    double *o=out;

    // Loop through p
   for (int ip=0; ip<num_p; ip++) {

      // Get points of current edge of p
      double *p1=p+3*ip;
      double *p2=p+3*((ip+1)%num_p);


      //      if (debug) printf("ip=%d p1=[%f %f %f] p2=[%f %f %f] -------------------- \n ",ip,p1[0],p1[1],p1[2],p2[0],p2[1],p2[2]);

      // calc p_vec (vector along the current edge of p)
      double p_vec[3];
      p_vec[0]=p2[0]-p1[0]; p_vec[1]=p2[1]-p1[1]; p_vec[2]=p2[2]-p1[2];

      double p_norm=NORM(p_vec);

      // Set initial t1 (last point in tmp polygon)
      double *t1=tmp+3*(num_tmp-1);

      // Vector from p to t1
      double pt1_vec[3];
      pt1_vec[0]=t1[0]-p1[0]; pt1_vec[1]=t1[1]-p1[1]; pt1_vec[2]=t1[2]-p1[2];


      // Make sure that we're not dealing with a zero length vector
      bool inout1_same=false;
      double inout1;
      if (CLOSE_TO_ZERO(pt1_vec)) {
         inout1_same=true;
         inout1=1000.0; // Set to a value so we know where to catch in point processing ifs
      } else {
        // Normal vector which is the length of |p_vec||pt_vec|*the sin between them
        double n_vec[3];
        CROSS_PRODUCT3D(n_vec,p_vec,pt1_vec);
        
        // Get magnitude which is distance out * |p_vec| without sign to indicate direction
        inout1=NORM(n_vec)/p_norm;
        
        // Dot normal with normal to sphere at point (i.e. just p1 since origin of sphere is (0,0,0))
        // This gives angle with respect to surface of the sphere and hence allows us to assign
        // a direction (i.e. a sign) to inout1
        if (DOT_PRODUCT3D(n_vec,p1)<0.0) inout1=-inout1;

      }

      // Make sure we don't have a degenerate polygon after clipping
      bool in_but_not_on_p_vec=false;

      // Init output poly
      num_o=0;

      // Loop through other polygon
      for (int it=0; it<num_tmp; it++) {
        double *t2=tmp+3*it;
        
        // calculate variable which says if t2 is in or out of polygon p
        // (the cross product of p_vec with the vector from p1 to t2
        //// Vector from p to t2
        double pt2_vec[3];
        pt2_vec[0]=t2[0]-p1[0]; pt2_vec[1]=t2[1]-p1[1]; pt2_vec[2]=t2[2]-p1[2];
        
        // Make sure that we're not dealing with a zero length vector
        bool inout2_same=false;
        double inout2;
        if (CLOSE_TO_ZERO(pt2_vec)) {
          inout2_same=true;
          inout2=1000.0; // Set to a value so we know where to catch in point processing ifs
        }
        else {
          //// Normal vector which is the length of |p_vec||pt2_vec|*the sin between them
          double n2_vec[3];
          CROSS_PRODUCT3D(n2_vec,p_vec,pt2_vec);
        
          //// Get magnitude which is distance out * |p_vec| without sign to indicate direction
          inout2=NORM(n2_vec)/p_norm;
                
          //// Dot normal with normal to sphere at point (i.e. just p1 since origin of sphere is (0,0,0))
          //// This gives angle with respect to surface of the sphere and hence allows us to assign
          //// a direction (i.e. a sign) to inout1
          if (DOT_PRODUCT3D(n2_vec,p1)<0.0) inout2=-inout2;
        }

        //      if (debug) printf("   it=%d t1=[%f %f %f] t2=[%f %f %f] inout1=%20.17f inout2=%20.17f \n ",it,t1[0],t1[1],t1[2],t2[0],t2[1],t2[2],inout1,inout2);
        

        // process point
        if (inout2 > CLIP_EQUAL_TOL) { // t2 inside
          if ((inout1 < 0.0) && !inout2_same && !inout1_same) { //  t1 outside
            double intersect_pnt[3];
        
            // Do intersection and add that point
            if (line_with_gc_seg3D(p1, p2, t2, t1, intersect_pnt)) {
              double ipnorm=NORM(intersect_pnt);
              o[3*num_o]=intersect_pnt[0]/ipnorm;
              o[3*num_o+1]=intersect_pnt[1]/ipnorm;
              o[3*num_o+2]=intersect_pnt[2]/ipnorm;
              //              if (debug) printf("    it=%d t1-out t2-in inter=[%f %f %f] \n ",it,o[3*num_o],o[3*num_o+1],o[3*num_o+2]);
              num_o++;
            }
          }
        
          // Add t2 point because it's inside
          o[3*num_o]=t2[0];
          o[3*num_o+1]=t2[1];
          o[3*num_o+2]=t2[2];
          num_o++;
        
          // record the fact that a point isn't on p_vec, but is in
          in_but_not_on_p_vec=true;
        
        } else if (inout2 < 0.0) { // t2 outside
        
          if (!inout1_same && (inout1 > CLIP_EQUAL_TOL)) {  //  t1 inside (this excludes the EQUAL region, because
            double intersect_pnt[3];      //             if a point was added in there we don't
                                         //              want to add another one right next to it)
        
            // Do intersection and add that point
            if (line_with_gc_seg3D(p1, p2, t1, t2, intersect_pnt)) {
              double ipnorm=NORM(intersect_pnt);
              o[3*num_o]=intersect_pnt[0]/ipnorm;
              o[3*num_o+1]=intersect_pnt[1]/ipnorm;
              o[3*num_o+2]=intersect_pnt[2]/ipnorm;

              // if (debug) printf("    it=%d t1-in t2-out inter=[%f %f %f] \n ",it,o[3*num_o],o[3*num_o+1],o[3*num_o+2]);
              num_o++;
            }
          }
        
        } else {  // t2 on edge
          // Just add point because it's on the edge
          o[3*num_o]=t2[0];
          o[3*num_o+1]=t2[1];
          o[3*num_o+2]=t2[2];
          num_o++;
        }
        
        // old t2 becomes the new t1
        t1=t2;
        inout1=inout2;
        inout1_same=inout2_same;
      }

      // if only on p_vec then degenerate and get rid of output poly
      if (!in_but_not_on_p_vec) num_o=0;

      // if poly is empty then leave
      if (num_o==0) break;

        remove_0len_edges3D(&num_o, o);

      // if poly is empty then leave
      if (num_o==0) break;

      // if not on the last cycle then copy out poly back to tmp
      if (ip != num_p-1) {
        double *end_o=o+3*num_o;
        for (double *o_i=o, *tmp_i=tmp; o_i<end_o; o_i++, tmp_i++) {
          *tmp_i=*o_i;
        }
        num_tmp=num_o;

      }
    }

  // Do output
    *num_out=num_o;
    // o is an alias for out so don't need to copy

#undef CROSS_PRODUCT3D
#undef DOT_PRODUCT3D
#undef NORM
#undef CLIP_EQUAL_TOL
  }


  // Intersects between the line a and the seqment s
  // returns true if we should add point p.
  // sin is the end of the segment inside the polygon
  // sout is the end of the segment outside the polygon
  // This subroutine is set up to be used with the poly intersect
  // code below, and has a number of tweaks for special cases
  // which might make it odd to be used as a general intesect code.
  bool line_with_seg2D(double *a1, double *a2, double *sin, double *sout,
                       double *p) {

    // Calculate thing to divide both line equations by
    double ttdb=
      a1[0]*(sout[1] - sin[1]) +
      a2[0]*(sin[1] - sout[1]) +
      sin[0]*(a1[1] - a2[1]) +
      sout[0]*(a2[1] - a1[1]);


    // if ttdb is 0.0 then the lines are parallel, this
    // shouldn't happen, but if it does it makes the
    // most sense to add the out point
    if (ttdb == 0.0) {
      p[0]=sout[0];
      p[1]=sout[1];
      return true;
    }

    // Calculate t
    double t=
      -(a1[0]*(sin[1]-a2[1]) +
        a2[0]*(a1[1]-sin[1]) +
        sin[0]*(a2[1]-a1[1]))/ttdb;

    // We shouldn't be off the ends, but
    // if we are because of rounding then
    // do what makes sense

    // if we're off the in end, then don't add
    if (t<0.0) return false;

    // if we're off the out end, then just add the
    // out point
    if (t>=1.0) {
      p[0]=sout[0];
      p[1]=sout[1];
      return true;
    }

    // Otherwise calculate point of intersection
    // and add that
    p[0]=sin[0] + t*(sout[0]-sin[0]);
    p[1]=sin[1] + t*(sout[1]-sin[1]);
    return true;

  }

  // intersects 2D convex polygons whose vertices are stored in counter clockwise
  // order.
  // p should be of size 2*num_p
  // q should be of size 2*num_q
  // tmp and out should both be allocated to be at least of size 2*(num_p+num_q)
  void intersect_convex_poly2D(int num_p, double *p,
                               int num_q, double *q,
                               double *tmp,
                               int *num_out, double *out)
  {

#define CLIP_EQUAL_TOL 1.0e-20

    // If p is empty then leave
    if (num_p==0) {
      *num_out=0;
    }

    // If q is empty then leave
    if (num_q==0) {
      *num_out=0;
    }

    // INSTEAD OF TMP USE T???

    // Copy q into tmp
    double *end_q=q+2*num_q;
    for (double *q_i=q, *tmp_i=tmp; q_i<end_q; q_i++, tmp_i++) {
      *tmp_i=*q_i;
    }
    int num_tmp=num_q;

    // Setup alias for output array
    int num_o=0;
    double *o=out;

    // Loop through p
    for (int ip=0; ip<num_p; ip++) {
      // Get points of current edge of p
      double *p1=p+2*ip;
      double *p2=p+2*((ip+1)%num_p);

    // calc p_vec (vector along the current edge of p)
      double p_vec[2];
      p_vec[0]=p2[0]-p1[0];
      p_vec[1]=p2[1]-p1[1];

      // Set initial t1
      double *t1=tmp+2*(num_tmp-1);
      double inout1=p_vec[0]*(t1[1]-p1[1]) - p_vec[1]*(t1[0]-p1[0]);

      // Make sure we don't have a degenerate polygon after clipping
      bool in_but_not_on_p_vec=false;

      // Init output poly
      num_o=0;

      // Loop through other polygon
      for (int it=0; it<num_tmp; it++) {
        double *t2=tmp+2*it;
        
        // calculate variable which says if t2 is in or out of polygon p
        // (the cross product of p_vec with the vector from p1 to t2
        double inout2=p_vec[0]*(t2[1]-p1[1]) - p_vec[1]*(t2[0]-p1[0]);
        
        // process point
        if (inout2 > CLIP_EQUAL_TOL) { // t2 inside
          if (inout1 < 0.0) { //  t1 outside
            double intersect_pnt[2];
        
            // Do intersection and add that point
            if (line_with_seg2D(p1, p2, t2, t1, intersect_pnt)) {
              o[2*num_o]=intersect_pnt[0];
              o[2*num_o+1]=intersect_pnt[1];
              num_o++;
            }
          }
        
          // Add t2 point because it's inside
          o[2*num_o]=t2[0];
          o[2*num_o+1]=t2[1];
          num_o++;
        
          // record the fact that a point isn't on p_vec, but is in
          in_but_not_on_p_vec=true;
        
        } else if (inout2 < 0.0) { // t2 outside
        
          if (inout1 > CLIP_EQUAL_TOL) {  //  t1 inside (this excludes the EQUAL region, because
            double intersect_pnt[2];      //             if a point was added in there we don't
            //             want to add another one right next to it)
        
            // Do intersection and add that point
            if (line_with_seg2D(p1, p2, t1, t2, intersect_pnt)) {
              o[2*num_o]=intersect_pnt[0];
              o[2*num_o+1]=intersect_pnt[1];
              num_o++;
            }
          }
        
        } else {  // t2 on edge
          // Just add point because it's on the edge
          o[2*num_o]=t2[0];
          o[2*num_o+1]=t2[1];
          num_o++;
        }
        
        // old t2 becomes the new t1
        t1=t2;
        inout1=inout2;
      }

      // if only on p_vec then degenerate and get rid of output poly
      if (!in_but_not_on_p_vec) num_o=0;

      // if poly is empty then leave
      if (num_o==0) break;

      // if not on the last cycle then copy out poly back to tmp
      if (ip != num_p-1) {
        double *end_o=o+2*num_o;
        for (double *o_i=o, *tmp_i=tmp; o_i<end_o; o_i++, tmp_i++) {
          *tmp_i=*o_i;
        }
        num_tmp=num_o;
      }
    }

  // Do output
    *num_out=num_o;
    // o is an alias for out so don't need to copy

#undef CLIP_EQUAL_TOL
  }


} // namespace
